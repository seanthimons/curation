
# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(magrittr)
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(ComptoxR)
  library(stringdist)
  library(fuzzyjoin)
  
  
  setwd(here('epa'))
  load('epa.Rdata')
}

# National Primary Drinking Water Regulations -----------------------------

dw_headers <- read_html('https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations') %>%
  html_elements(., 'h3') %>%
  html_text()

dw <- read_html('https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations') %>% 
  html_elements(., 'table') %>% 
  html_table() %>% 
  set_names(., dw_headers) %>% 
  map(., ~{
    as_tibble(.x) %>%
      select(1:3) %>% 
      `colnames<-`(., c('analyte', 'mclg', 'mcl')) %>% 
      mutate(across(everything(), as.character))
  }, .progress = TRUE) %>% 
  list_rbind(names_to = 'source') %>% 
  pivot_longer(., cols = c(mclg, mcl), names_to = 'name', values_to = 'value') %>% 
  mutate(
    unit = 'mg/L',
    analyte = str_remove_all(analyte, pattern = "\n.*")
    # orig_value = value,
    # value = as.numeric(value),
    # value = case_when(
    #   orig_value == 'zero' ~ 0,
    #   .default = value)
  )

rm(dw_headers)

# Aquatic -----------------------------------------------------------------


alc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'fw_ac', 'fw_chr', 'sw_ac', 'sw_chr', 'year', 'notes')) %>% 
  pivot_longer(., cols = fw_ac:sw_chr) %>% 
  mutate(
    unit = 'ug/l'
  )

# Human -------------------------------------------------------------------


hhc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn','hh_wo', 'hh_o', 'year', 'notes')) %>% 
  pivot_longer(., cols = c('hh_wo', 'hh_o')) %>% 
  mutate(
    unit = 'ug/l'
  )


# Taste -------------------------------------------------------------------


oe <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-organoleptic-effects') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'result')) %>% 
  mutate(
    unit = 'ug/l'
  ) %>% 
  filter(analyte != '1Non-Priority pollutant.') %>% 
  rename(value = result)

# cleaning ----------------------------------------------------------------


raw <- list_rbind(list('dw' = dw, 'alc' = alc, 'hhc' = hhc, 'oe' = oe), names_to = 'frame')

write_rds(raw, 'raw.RDS')

rm(alc, hhc, oe, dw)

raw_dat <- raw %>% 
  distinct(analyte, casrn, .keep_all = F) %>% 
  mutate(
    idx = 1:n(),
    cas_chk = webchem::as.cas(casrn),
    raw_name = str_remove_all(analyte, pattern = "\\([^()]*\\)") %>% str_squish()
  )

bad <- raw_dat %>% 
  filter(is.na(cas_chk)) %>% 
  select(-c(casrn, cas_chk))

bad_search <- ct_search(query = bad$raw_name, request_method = 'GET', search_method = 'equal')

bad_src <- bad_search %>% 
  arrange(rank) %>% 
  distinct(raw_search, .keep_all = T)

bad_dat <- inner_join(bad, bad_src, join_by(raw_name == raw_search)) %>% 
  select(idx, dtxsid)

rm(bad, bad_search, bad_src)

good <- raw_dat %>% 
  filter(!is.na(cas_chk)) %>% 
  mutate(cas_chk = unname(cas_chk))

good_cas <- ct_search(query = good$cas_chk, request_method = 'GET', search_method = 'equal')

good_src <- good_cas %>% 
  arrange(rank) %>% 
  distinct(raw_search, .keep_all = T)

good_dat <- inner_join(good, good_src, join_by(cas_chk == raw_search)) %>% 
  select(idx, dtxsid)

rm(good, good_cas, good_src)

raw_cur <- bind_rows(list(good_dat, bad_dat)) %>% 
  left_join(raw_dat, ., join_by(idx)) %>% 
  select(-raw_name, -casrn, -cas_chk) %>% 
  arrange(dtxsid)

# hashing -----------------------------------------------------------------
{
  params <- raw_cur %>% 
    filter(is.na(dtxsid)) %>% 
    select(idx, analyte) %>% 
    mutate(raw_search = str_remove_all(analyte, pattern = '\\*') %>%
             str_remove_all(., pattern = '\\(P\\)') %>% 
             str_remove_all(., pattern = "\\s*\\([^()]*\\)$") %>% 
             str_remove_all(., pattern = "\\s*\\([^()]*\\)$") %>% 
             str_squish())
  
  {
    
    srs_e <- map(params$raw_search, ~srs_search(query = .x, method = 'exact'), .progress = T) %>% 
      set_names(., params$raw_search) %>% 
      list_rbind(names_to = 'raw_search')
    
    srs_e_details <- map(srs_e$itn, ~srs_details(query = .x), .progress = T) %>% 
      set_names(., srs_e$raw_search) %>% 
      list_rbind(names_to = 'raw_search') %>% 
      group_by(raw_search) %>% 
      arrange(desc(synonyms)) %>% 
      ungroup() %>% 
      distinct(raw_search, .keep_all = T) 
    
    missing <- params %>% 
      filter(!raw_search %in% srs_e_details$raw_search) %>% 
      select(raw_search) %>% 
      unlist() %>% 
      unname() %>% 
      print()
    
    srs_c <- map(missing, ~srs_search(query = .x, method = 'contains'), .progress = T) %>% 
      set_names(., missing) %>% 
      list_rbind(names_to = 'raw_search')
    
    srs_c_details <- map(srs_c$itn, ~srs_details(query = .x), .progress = T) %>% 
      set_names(., srs_c$raw_search) %>% 
      list_rbind(names_to = 'raw_search') %>% 
      group_by(raw_search) %>% 
      arrange(desc(synonyms)) %>% 
      ungroup() %>% 
      distinct(raw_search, .keep_all = T) 
    
    missing <- params %>% 
      filter(!raw_search %in% c(srs_c_details$raw_search, srs_e_details$raw_search)) %>% 
      #select(raw_search) %>% 
      #unlist() %>% 
      #unname() %>% 
      print()
    
    #NOTE Manual process here!
    missing <- missing %>% 
      mutate(itn = case_when(
        raw_search == 'Hazard Index PFAS' ~ '1995414',
        raw_search == 'Radium 226 and Radium 228' ~ '701037'
      )) %>% 
      select(-analyte, -idx)
    
    missing_src <- map(missing$itn, ~srs_details(query = .x)) %>% 
      list_rbind()
    
    missing_dat <- missing %>% 
      left_join(., missing_src, join_by(itn == internalTrackingNumber)) %>% 
      rename(internalTrackingNumber = itn)
    
    rm(srs_e, srs_c)
    
    params_cur <- list(missing_dat, srs_e_details, srs_c_details) %>%
      list_rbind() %>% 
      select(
        raw_search, 
        internalTrackingNumber, 
        systematicName, 
        currentCasNumber,
        dtxsid
      ) %>% 
      `colnames<-`(., c(
        'raw_search',
        'itn',
        'preferredName',
        'casrn',
        'dtxsid'
      )) 
    
    #TODO need to join against raw names for final list. 
    
    missing <- params %>%
      anti_join(., params_cur, join_by(raw_search))
    
    }
}

params_fin <- params_cur %>% 
  left_join(params, ., join_by(raw_search)) %>% 
  select(-casrn, -raw_search)

raw_cur <- raw_cur %>% 
  filter(!is.na(dtxsid))

final <- bind_rows(params_fin, raw_cur) %>% 
  mutate(
    preferredName = case_when(
      !is.na(dtxsid) ~ NA,
      is.na(dtxsid) ~ preferredName)
  )

dss <- final %>% 
  filter(!is.na(dtxsid)) %>% 
  select(-preferredName, -itn)

#NOTE Some duplicates here
dss_cur <- ct_details(dss$dtxsid) %>% 
  select(-casrn)

dss <- dss %>% 
  left_join(., dss_cur, join_by(dtxsid))

wq <- final %>% 
  filter(!idx %in% dss$idx) %>% 
  mutate(dtxsid = paste0('E', itn)) %>% 
  select(-itn) %>% 
  mutate(preferredName = str_remove_all(preferredName, pattern = ', from SDWA NPDWR|-- SWDA NPDWR'))

final_dictionary <- bind_rows(wq, dss) %>% 
  arrange(idx) %>% 
  select(-idx) %>% 
  distinct(.keep_all = T)


# Final pairing -----------------------------------------------------------
{
ndwqs <- raw %>% 
  select(-casrn) %>%
  
  # NOTE debugging
  # anti_join(., final_dictionary, join_by(analyte)) %>% 
  
  left_join(., final_dictionary, join_by(analyte)) %>% 
  select(-year) %>% 
  mutate(
    idx = 1:n(),
    source = if_else(is.na(source), 'ND', source),
    notes = case_when(
      notes == '' ~ NA,
      .default = notes
    ),
    orig_value = value,
    value = str_remove_all(value, pattern = '\\u2014|ug/L|,|--|---|none|vacated|NP1'), 
    value = str_remove_all(value, pattern = 'TT.*|n/a.*'),
    value = str_replace_all(value, pattern = '\\u2013', '-'),
    value = str_squish(value),
    value = str_replace_all(value, pattern = 'zero', replacement = "0"),
    value = na_if(value, "")
  ) %>% filter(!is.na(value))

n_1 <- ndwqs %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value))

n_2 <- ndwqs %>% 
  filter(idx %ni% n_1$idx) %>% 
  filter(source != 'Disinfectants', source != 'Selected Per- and poly-fluoroalkyl substances (PFAS)') %>% 
  arrange(preferredName) %>% 
  mutate(
    unit = case_when(
      preferredName == 'Alpha particle'  ~ 'pCi/L',
      preferredName == 'Asbestos' ~ 'million fibers/L',
      preferredName == 'Beta Particles And Photon Emitters' ~ 'mrem/yr',
      preferredName == 'Methyl mercury(II) cation' ~ 'mg/kg',
      preferredName == 'Radium, isotope of mass 226 and/or radium, isotope of mass 228' ~ 'pCi/L',
      preferredName == 'Total coliforms' ~ '%',
      .default = unit
    ),
    value = case_when(
      preferredName == 'Alpha particle'  ~ '15',
      preferredName == 'Asbestos' ~ '7',
      preferredName == 'Arsenic' ~ '0.01',
      preferredName == 'Beta Particles And Photon Emitters' ~ '4',
      preferredName == 'Methyl mercury(II) cation' ~ '0.3',
      preferredName == 'Radium, isotope of mass 226 and/or radium, isotope of mass 228' ~ '5',
      preferredName == 'Total coliforms' ~ '5',
      preferredName == 'Perfluorooctanesulfonic acid' ~ '0.25',
      preferredName == 'Perfluorooctanoic acid' ~ '100',
      preferredName == 'Uranium' ~ '30',
      value == 'Total' ~ NA,
      .default = value
    ),
    value = str_squish(value),
    value = str_remove_all(value, pattern = '[[:SPACE:]]')
  
  ) %>% filter(!is.na(value))

{
  n_3 <- n_2 %>% 
    #HACK experimental function
    #group_split(., str_detect(value, '-'))
    split(., ~str_detect(.$value, pattern = '-'))
  
n_3$`FALSE` %<>% 
  mutate(value = as.numeric(value))

n_3$`TRUE` %<>%  
 mutate(
   idx_r = 1:n()
 ) %>% 
  separate_longer_delim(., value, delim = '-') %>% 
  mutate(
    value = as.numeric(value),
    is_range = TRUE,
    n_r = NA
  ) %>% 
  filter(!is.na(value)) %>% 
  group_by(idx) %>% 
  mutate(
    n_r = case_when(
      min(value) == value ~ 'Lower range',
      max(value) == value ~ 'Upper range')
  ) %>% 
  unite(., col = 'notes', n_r, notes, sep = '; ', remove = T, na.rm = T) %>% 
  select(-idx_r) %>% 
  ungroup()

n_3 <- list_rbind(n_3)
}

# n_2 %<>%
#   mutate(value = as.numeric(value)) %>% 
#   filter(!is.na(value))

ndwqs_final <- bind_rows(
  n_3,
  #n_2,
  n_1) %>% 
  ungroup() %>% 
  mutate(
    idx = 1:n(),
    is_range = if_else(is.na(is_range), FALSE, is_range),
    endpoint = case_when(
      name == "mclg" ~ 'MCLG',
      name == "mcl" ~ 'MCL',
      name == "fw_ac" ~ 'Aquatic life', 
      name == "fw_chr" ~ 'Aquatic life',
      name == "sw_ac" ~ 'Aquatic life',
      name == "sw_chr" ~ 'Aquatic life',
      name == "hh_wo" ~ 'Human health - Water & Organism',
      name == "hh_o" ~ 'Human health - Organism',
      .default = 'Organoleptic'
    ), 
    sourcewater = case_when(
      str_detect(name, 'fw_') ~ 'Fresh water', 
      str_detect(name, 'sw_') ~ 'Salt water',
      .default = NA
    ),
    
    data_category = 'Primary',
    
    duration = case_when(
      str_detect(name, '_ac') ~ 'Acute', 
      str_detect(name, '_chr') ~ 'Chronic',
      .default = NA
    ), 
    
    meta = NA, 
    
    cit = case_when(
      frame == 'dw' ~ 'National Primary Drinking Water Regulations',
      frame != 'dw' ~ 'Clean Water Act 304 (a): National Recommended Water Quality Criteria'),
    origin_category = 'Federal',
    priority_id = case_when(
      frame == 'dw' ~ 1
      frame != 'dw' ~ 2
    ),
    ) %>% 
  arrange(idx) %>% 
  mutate(
    value = case_when(
      preferredName == 'pH' ~ value,
      unit == "ug/l" ~ value / 1000,
      .default = value
    ),
    unit = case_when(
      preferredName == 'pH' ~ 'standard units',
      unit == "ug/l" ~ "mg/L",
      .default = unit
    ),
    across(
      .cols = everything(),
      .fns = ~ if_else(is.na(.), "ND", as.character(.))
    ),
    value = as.numeric(value)
  ) %>%
  select(
    -source,
    -frame,
    -name,
    -analyte
  )

}

write_rds(ndwqs_final, here('final', 'nwqs.RDS'))

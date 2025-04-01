{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(V8)
  library(httr2)
  library(ComptoxR)
  library(stringdist)
  library(fuzzyjoin)
  library(todor)
  
  setwd(here('sswqs'))
  
  source(here('functions.R'))
  
  todor::todor()
}

# Load data ---------------------------------------------------------------

pt <- rio::import(here('pt', 'pt.RDS'))

rads <- left_join(pt$isotopes, pt$elements, join_by(element == Symbol)) %>% 
  select(Z, element, Name) %>% 
  filter(!is.na(Name)) %>% 
  mutate(
    short_search1 = paste0(element, Z),
    short_search2 = paste0(Z, element),
    full_search = paste0(Name,'-', Z)
  )

job::job({
  #TODO beef this up to also search by short and other variations? 
  
  rads_dat <- ct_search(query = rads$full_search, search_method = 'equal', request_method = 'GET')
  
  # rads <- rads %>% 
  #   filter(full_search %ni% rads_dat$raw_search)
  # 
  # rads_dat2 <- ct_search(query = rads$short_short2, search_method = 'equal', request_method = 'GET')
  
  
})

rads_dat_cur <- rads_dat %>% 
  select(raw_search, dtxsid) %>% 
  inner_join(rads, ., join_by(full_search == raw_search)) %>% 
  select(
    full_search, 
    short_search1, 
    short_search2, 
    dtxsid
  ) %>% 
  mutate(
    s_3 = str_replace_all(full_search, pattern = '-', replacement = ' '),
    s_4 = str_to_lower(short_search1),
    s_5 = str_to_lower(short_search2),
    s_6 = str_to_lower(full_search),
    s_7 = str_to_lower(s_3)
  ) %>% 
  pivot_longer(., cols = !dtxsid, values_to = 'raw_search') %>% 
  select(-name)

unit_dict <- rio::import(here('final', 'unit_dict.RDS'))

#Download----
{
  
  cli::cli_alert_info('Downloading spec')
  
  cx <- v8()
  
  cx$source("https://cfpub.epa.gov/wqsits/wqcsearch/data/criteria_json_5a.js")
  
  vars <- cx$eval(
    "
  Object.keys(this).filter(function(key) {
    return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
  })
"
  ) %>%
    str_split(., ",") %>%
    pluck(., 1)
  
  parent_dat <- vars %>%
    map(., ~ {
      cx$get(.x)
    }) %>%
    set_names(., vars)
  
  state_vars <- parent_dat$entities %>%
    map(., ~ as_tibble(.) %>%
          t(.) %>%
          as_tibble()) %>%
    list_rbind() %>%
    set_names(., c("area", "region", "abv", "cat", "file", "coverage")) %>%
    mutate(
      across(everything(), ~ na_if(., "")),
      json = paste0(
        "https://cfpub.epa.gov/wqsits/wqcsearch/data/stateJson_",
        abv,
        ".js"
      ),
      idx = 1:n()
    )
  
  state_extra <- state_vars %>%
    select(area, region, cat, file, coverage, idx, abv)
  
  state_vars %<>%
    select(abv, json)
  
  rm(vars)
}


  cli::cli_alert_info('Requesting state data')
  
  state_dat <- state_vars %>%
    pmap(., function(abv, json) {
      cli::cli_inform(abv, "\n")
      ctx <- v8()
      ctx$source(json)
      
      st_vars <- ctx$eval(
        "
  Object.keys(this).filter(function(key) {
    return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
  })
"
      ) %>%
        str_split(., ",") %>%
        pluck(., 1)
      
      dat <- st_vars %>%
        map(., ~ {
          ctx$get(.x)
        }) %>%
        set_names(., st_vars) %>%
        modify_at(., "criteriaData_sub", ~ pluck(., 1)) %>%
        compact(.)
      
      if (length(dat) == 0) {
        dat <- NULL
      } else {
        dat$desc_use_class_sub %<>%
          flatten(.) %>%
          enframe(., name = "key", value = "local") %>%
          unnest(., "local")
        
        new_names <- c(
          "analyte" = "analyte",
          "result" = "V1",
          "unit" = "V2",
          "protection" = "V3",
          "use_class" = "V4",
          "source" = "V5",
          "page_source" = "V6"
        )
        
        dat$criteriaData_sub %<>%
          map_if(., is.list, ~ {
            map(., ~ {
              t(.x) %>%
                as_tibble(.)
            }) %>%
              list_rbind(.)
          }) %>%
          map_if(., is.character, ~ {
            as_tibble(.x) %>%
              mutate(across(everything(), as.character))
          }) %>%
          list_rbind(., names_to = "analyte") %>%
          rename(!!!new_names)
      }
      rm(ctx)
      return(dat)
    }, .progress = T) %>%
    set_names(., state_vars$abv) %>%
    compact(.)
  
  sources <- state_dat %>%
    map(., ~ {
      pluck(.x, "sourcedoc_sub") %>%
        flatten(.) %>%
        enframe(., name = "key", value = "link")
    }) %>%
    list_rbind(names_to = "area") %>%
    unnest("link")
  
  use_class <- state_dat %>%
    map(., ~ pluck(., "desc_use_class_sub")) %>%
    list_rbind(names_to = "area")
  
  crit_dat <- state_dat %>%
    map(., ~ pluck(., "criteriaData_sub")) %>%
    list_rbind(names_to = "area")
  
  rm(state_dat)
  
  parent_dat$pollutants %<>%
    enframe(., name = 'idx', value = 'v') %>% 
    unnest_wider(., 'v', names_sep = '') %>% 
    rename(
      'analyte' = 'v1', 
      'cas' = 'v2',
      'remap' = 'v3', 
      'dtxsid' = 'v4'
    ) %>%
    mutate(across(everything(), ~na_if(., "")))
  
  parent_dat$pollutantRemap %>% 
    enframe(., name = 'idx', value = 'v') %>% 
    unnest_longer(., col = 'v')
  
  parent_dat$units %<>%
    enframe(., name = 'idx', value = 'v') %>% 
    unnest_wider(., 'v', names_sep = '') %>% 
    select(-v2) %>% 
    rename(
      unit = v1
    )
  
  
  # entities ----------------------------------------------------------------
  
  entities <- parent_dat$entities %>% 
    map(., ~enframe(.x) %>% pivot_wider(., names_from = name)) %>% 
    list_rbind() %>% 
    rename(
      name = `1`,
      region = `2`,
      short_code = `3`,
      level = `4`,
      cit = `5`,
      coverage = `6`
    ) %>% 
    mutate(coverage = na_if(coverage, ""))
  
  # Pollutant cleaning -------------------------------------------------------
  
  
  #NOTE Creates df of params that need to be cleaned + curated, no DTXSID
  
  ## Initial set -------------------------------------------------------------
  
  raw_pol <- parent_dat$pollutants %>%
    filter(is.na(dtxsid), is.na(remap)) %>% 
    distinct(., analyte, cas, .keep_all = T) %>% 
    filter((idx %in% crit_dat$analyte)) %>% 
    select(
      -remap, 
      -dtxsid
    )
  
  
  ## Remapped vars -----------------------------------------------------------
  
  #NOTE Issue with circular references here, eg 195 -> 333 radium 226 / 228
  remapped <- parent_dat$pollutants %>%
    filter(!is.na(remap))
    select(idx, remap)
  
  
  ## Already curated ---------------------------------------------------------
  
  
  raw_pol_dtxsids <- parent_dat$pollutants %>%
    filter(!is.na(dtxsid)) %>% 
    distinct(., idx, dtxsid, .keep_all = T) %>% 
    filter((idx %in% crit_dat$analyte)) %>% 
    select(-analyte, -cas, -remap)
  
  raw_pol_dtxsids_dat <- ct_details(query = raw_pol_dtxsids$dtxsid) %>% select(-casrn)
  
  #NOTE Warning about join relationships - duplicates within dataset on original data
  raw_pol_dtxsids <- left_join(raw_pol_dtxsids, raw_pol_dtxsids_dat, join_by(dtxsid))
  
  rm(raw_pol_dtxsids_dat)
  
  #NOTE analysis df for params to prioritize first by abundance
  stats <- crit_dat %>%
    filter(analyte %in% raw_pol$idx) %>%
    count(analyte) %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    rename(idx_a = analyte) %>% 
    left_join(., raw_pol, join_by(idx_a == idx))
  
  ## CASRN -------------------------------------------------------------------
  
  
  raw_pol_cas <- raw_pol %>% 
    filter(!is.na(cas)) %>% 
    distinct(cas)
  
  raw_pol_cas_src <- ct_search(query = raw_pol_cas$cas, request_method = 'GET', search_method = 'equal') %>% 
    select(raw_search, dtxsid)
  
  pol_cur_cas <- inner_join(raw_pol, raw_pol_cas_src, join_by(cas == raw_search))
  
  rm(raw_pol_cas_src)
  
  raw_pol_srs <- raw_pol %>% 
    filter(idx %ni% pol_cur_cas$idx) %>% 
    select(-cas)
  
  ## PT ----------------------------------------------------------------------
  
  pol_pt <- raw_pol_srs %>% 
    inner_join(., rads_dat_cur, join_by(analyte == raw_search))
  
  raw_pol_srs <- raw_pol_srs %>% 
    filter(idx %ni% pol_pt$idx)
  
  ## Exact -------------------------------------------------------------------
  
  
  srs_e <- map(raw_pol_srs$analyte, ~srs_search(query = .x, method = 'exact'), .progress = T) %>% 
    set_names(., raw_pol_srs$analyte) %>% 
    list_rbind(names_to = 'raw_search') %>%
    distinct(raw_search, .keep_all = T)
  
  srs_e_details <- map(srs_e$itn, ~srs_details(query = .x), .progress = T) %>% 
    set_names(., srs_e$raw_search) %>% 
    list_rbind(names_to = 'raw_search')
  
  exact <- raw_pol_srs %>% 
    inner_join(., srs_e_details, join_by(analyte == raw_search)) %>% 
    select(
      idx, 
      analyte,
      internalTrackingNumber,
      dtxsid
    )
  
  missing <- raw_pol_srs %>% 
    filter(idx %ni% exact$idx) %>% 
    select(analyte) %>% 
    unlist() %>% 
    unname() %>% 
    print()
  
  
  ## Contains ----------------------------------------------------------------
  
  
  srs_c <- map(missing, ~srs_search(query = .x, method = 'contains'), .progress = T) %>% 
    set_names(., missing) %>% 
    list_rbind(names_to = 'raw_search') %>% 
    distinct(raw_search, .keep_all = T)
  
  srs_c_details <- map(srs_c$itn, ~srs_details(query = .x), .progress = T) %>% 
    set_names(., srs_c$raw_search) %>% 
    list_rbind(names_to = 'raw_search') %>% 
    mutate(
      dtxsid = case_when(
        raw_search == 'unat'~ 'DTXSID1042522', 
        .default = dtxsid
      ))
  
  contains <- raw_pol_srs %>% 
    inner_join(., srs_c_details, join_by(analyte == raw_search)) %>% 
    select(
      idx, 
      analyte,
      internalTrackingNumber,
      dtxsid
    )
  
  missing <- raw_pol_srs %>% 
    filter(idx %ni% c(exact$idx, contains$idx)) %>% 
    #select(analyte) %>% 
    #unlist() %>% 
    #unname() %>% 
    print()
  
  #NOTE Uncomment to regenerate  
  # dict <- rio::import(here('sswqs', 'sswqs_unique_curated.xlsx')) %>% 
  #   filter(std_poll_id %in% missing$idx_a)
  # 
  # rio::export(dict, file = 'dict_raw.xlsx')
  
  ## Manual ------------------------------------------------------------------
  
  
  dict <- rio::import(here('sswqs', 'dict_raw.txt')) %>% 
    select(-raw_search) %>% 
    mutate(across(everything(), as.character))
  
  missing_cur <- left_join(missing, dict, join_by(idx)) %>% 
    filter(final != 'remove') %>% 
    rename(dtxsid = final)
  
  missing %>% filter(idx %ni% missing_cur$idx)
  
  rm(missing, dict)
  
  pol_final <- bind_rows(
    pol_pt %>% select(idx, dtxsid),
    exact %>% select(-analyte), 
    contains %>% select(-analyte),
    raw_pol_dtxsids %>% select(-preferredName),
    pol_cur_cas %>% select(idx, dtxsid), 
    missing_cur %>% select(-analyte)
  ) %>% 
    pivot_longer(., cols = c(dtxsid, internalTrackingNumber), values_to = 'v', values_drop_na = TRUE) %>% 
    select(-name) %>% 
    mutate(
      id = case_when(
        str_detect(v, pattern = 'DTX') ~ 'dtxsid',
        .default = 'itn'
      ),
      v = str_remove_all(v, pattern = 'E')
    ) %>% 
    #NOTE sorts by DTXSID -> ITN
    arrange(idx, id) %>% 
    distinct(idx, .keep_all = T) %>% 
    split(.$id)
  
  pol_final$dtxsid <- ct_details(pol_final$dtxsid$v) %>% 
    left_join(pol_final$dtxsid, ., join_by(v == dtxsid)) %>% 
    distinct(., .keep_all = T) %>% 
    select(-id, -casrn)
  
  pol_final$itn <- map(pol_final$itn$v, ~srs_details(.x), .progress = T) %>% 
    #set_names(., pol_final$itn$v) %>% 
    list_rbind(., names_to = 'raw_search') %>% 
    select(internalTrackingNumber, systematicName) %>% 
    left_join(pol_final$itn, ., join_by(v == internalTrackingNumber)) %>% 
    distinct(idx, v, .keep_all = T) %>% 
    select(-id) %>% 
    rename(
      preferredName = systematicName
    )
  
  pol_final <- list_rbind(pol_final)
  
  #write_rds(pol_final, file = here('sswqs', 'pollutant_final.RDS'))
  
  wqs_pollutants <- remapped %>% 
    left_join(., pol_final, join_by(remap == idx)) %>% 
    select(-remap) %>% 
    bind_rows(., pol_final)
  
  write_rds(wqs_pollutants, file = here('sswqs', 'wqs_pollutants.RDS'))
  
  # Result cleaning -------------------------------------------------------------------
  
  result_idx <- crit_dat %>% 
    count(result) %>% 
    rename(raw_result = result) %>% 
    mutate(idx = 1:n()) %>% 
    as_tibble()
  
  
  ## numerical ---------------------------------------------------------------
  
  result_idx_num <- result_idx %>% 
    mutate(
      result = raw_result %>%
        as.numeric()) %>% 
    filter(!is.na(result)) %>% 
    select(-n, -idx)
  
  ## character ---------------------------------------------------------------
  
  result_idx_char <- result_idx %>% 
    filter(idx %ni% result_idx_num$idx) %>% 
    filter(!str_detect(raw_result, "^[A-Za-z]")) %>%
    filter(!str_detect(raw_result, "%|\\*|\\/")) %>% 
    mutate(
      result = raw_result %>%
        str_remove_all(., pattern = '\\,|\\^|<') %>% 
        str_replace_all(., pattern = '7 million', replacement = '7000000') %>% 
        str_remove_all(., pattern = '[[:space:]]') %>% 
        str_replace_all(.,  pattern = 'e|x10|X10', replacement = 'E'),
      sci_note_count = str_count(result, 'E'),
      is_range = case_when( 
        sci_note_count == 1 & str_detect(result, '-', negate = TRUE) ~ FALSE, #positive sci notation
        sci_note_count == 1 & str_detect(result, '-') ~ FALSE, #negative sci notation
        
        sci_note_count == 2 & str_detect(result, '-') ~ TRUE, #range with sci notation
        sci_note_count == 0 & str_detect(result, '-') ~ TRUE, #range w/o sci notation
        sci_note_count == 0 & str_detect(result, '-', negate = TRUE) ~ FALSE, #numerical
        .default = NA)
    )
  
  ### Sci note +ranged  -------------------------------------------------------
  
  
  {  
    result_idx_char_cur <- result_idx_char %>% 
      nest(., .by = c(sci_note_count, is_range)) %>% 
      arrange(., sci_note_count, is_range) %>% 
      pluck(., 'data') %>% 
      set_names(., LETTERS[1:length(.)])
    
    result_idx_char_cur$A %<>%
      mutate(
        result = result %>%
          str_replace_all(., pattern = '0\\.\\.0', replacement = '0.0') %>% 
          str_replace_all(., pattern = '\\+', replacement = 'E') %>% str_squish(),
        result = as.numeric(result)
      )
    
    result_idx_char_cur$B %<>% 
      separate_longer_delim(., cols = result, delim = '-') %>% 
      mutate(
        result = as.numeric(result)
      ) %>% 
      group_by(idx) %>% 
      mutate(
        n_r = case_when(
          min(result) == result ~ 'Lower range',
          max(result) == result ~ 'Upper range'))
    
    result_idx_char_cur$C %<>% 
      separate_wider_delim(., cols = result, delim = 'E', names = c('r', 'pwr')) %>% 
      mutate(
        pwr = str_replace_all(pwr, pattern = '\\+0\\.1', replacement = '1'),
        pwr = paste0('E', pwr)
      ) %>% 
      unite(., result, r, pwr,  sep = '', remove = T) %>% 
      mutate(result = as.numeric(result))
    
    result_idx_char_cur$D %<>%
      mutate(result = result %>% 
               str_extract_all(., pattern = '\\d*.\\d*E-\\d*', simplify = F)) %>% 
      unnest_longer(., col = result, transform = as.numeric) %>% 
      group_by(idx) %>% 
      mutate(
        n_r = case_when(
          min(result) == result ~ 'Lower range',
          max(result) == result ~ 'Upper range'))
    
    result_idx_char_cur %<>%
      list_rbind() %>% 
      select(-n, -idx)
      
    
  }
  
  result_idx_cur <- bind_rows(
    result_idx_char_cur, 
    result_idx_num
  )
  
  
  #TEMP debugging for symbols to coerce out any unicode
  # result_idx_char %>%
  #   mutate(symbol = result %>% str_remove_all(., "[A-Za-z0-9]") %>% str_remove_all(., pattern = '\\.') %>% str_squish() %>% na_if(., "")
  #   ) %>% filter(!is.na(symbol)) %>% 
  #   count(symbol) %>% arrange(desc(n))
  # 
  # result_idx %>% filter(idx %ni% c(result_idx_char$idx, result_idx_num$idx)) %>% print(n = Inf)
  
  #Units----
  
  cleaned_crit_dat <- crit_dat %>%
    rename(orig_result = result) %>% 
    left_join(., wqs_pollutants, join_by(analyte == idx)) %>% 
    inner_join(., result_idx_cur, join_by(orig_result == raw_result))
  
  units <- cleaned_crit_dat %>% 
    count(unit) %>%
    arrange(desc(n)) %>%
    rename(unit_idx = unit) %>% 
    left_join(., parent_dat$unit, join_by(unit_idx == idx)) %>% 
    filter(!is.na(unit), n > 1) %>% 
    #filter(str_detect(unit, pattern = 'g/l'))
    mutate(
      remap = NA,
      conversion_factor = NA_real_, 
      unit = str_remove_all(unit, pattern = '\\u00b0'), 
      unit = str_replace_all(unit, pattern = '\\u00b5', replacement = 'u'),
      remap = case_when(
        str_detect(unit, pattern = 'g/l') ~ 'mg/l',
        str_detect(unit, pattern = 'ppm|ppb|ppq') ~ 'mg/l',
        
        str_detect(unit, pattern = 'million fibers/l') ~ 'fibers/l',
        str_detect(unit, pattern = 'mf/l') ~ 'fibers/l',
        
        str_detect(unit, pattern = 'picocuries/l') ~ 'pCi/l',
        str_detect(unit, pattern = 'pci/l') ~ 'pCi/l',
        
        str_detect(unit, pattern = 'organisms/100 ml') ~ 'count/100ml',
        
        str_detect(unit, pattern = '\\[no units\\]') ~ 'standard units',
        str_detect(unit, pattern = 'ph units') ~ 'standard units',
        str_detect(unit, pattern = 'color units') ~ 'PCCU',
        str_detect(unit, pattern = 'platinum cobalt units') ~ 'PCCU',
        
        str_detect(unit, pattern = 'F') ~ 'C',
        
        str_detect(unit, pattern = 'F') ~ 'C',
        
        
        .default = NA),
      
      conversion_factor = case_when(
        
        unit == 'ug/l' ~ 1e-3,
        unit == 'ng/l' ~ 1e-6,
        unit == 'pg/l' ~ 1e-9,
        unit == 'g/l' ~ 1e3,
        unit == 'fg/l' ~ 1e-12,
        unit == 'mg/l' ~ 1,
        unit == 'ppm'~ 1,
        unit == 'ppq' ~ 1e-9,
        unit == 'ppb' ~ 1e-3, 
        
        unit == 'million fibers/l' ~ 1e-6,
        unit == 'mf/l' ~ 1e-6,
        
        unit == 'F' ~ NA
        
        ,.default = 1)
    )
  
  resolve <- units %>% 
    left_join(., cleaned_crit_dat, join_by(unit_idx == unit)) %>% 
    group_by(preferredName, remap, unit) %>% 
    reframe(n = n()) %>% 
    ungroup() %>% 
    arrange(preferredName, desc(n)) %>% 
    get_dupes(preferredName)
    filter(is.na(remap)) %>% 
    group_by(unit) %>% 
    reframe(
      n = sum(n),
    ) %>% 
    arrange(unit, desc(n)) 
   
    #%>% split(.$preferredName)

  
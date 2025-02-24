
# Packages ----------------------------------------------------------------

{
    library(rio)
    library(janitor)
    library(tidyverse)
    library(here)
    library(httr2)
    library(rvest)
    library(ComptoxR)
    library(stringdist)
    library(fuzzyjoin)
    
    setwd(here('epa'))
    load('.Rdata')
}

# functions ---------------------------------------------------------------

srs_search <- function(query, method){
  request("https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/autoComplete/nameSearch") |>
    req_url_query(
      term = query,
      qualifier = method
    ) |>
    req_headers(
      accept = "*/*") |>
    #req_dry_run()
    req_perform() %>% 
    resp_body_json() %>% 
    map(., as_tibble) %>% 
    list_rbind()
}

srs_details <- function(query){
  request("https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/substance/itn/") |>
    req_url_path_append(query) %>% 
    # req_url_query(
    #   excludeSynonyms = "true"
    # ) |>
    req_headers(
      accept = "application/json") |>
    #req_dry_run()
    req_perform() %>% 
    resp_body_json() %>% 
    pluck(., 1) %>% 
    modify_at(
      "synonyms",
      ~ length(.x)
    ) %>% 
    flatten() %>% 
    compact() %>% 
    map(., ~ if(is.null(.x)){NA}else{.x}) %>%
    as_tibble()
}


# Aquatic -----------------------------------------------------------------


alc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'fw_ac', 'fw_chr', 'sw_ac', 'sw_chr', 'year', 'notes')) %>% 
  pivot_longer(., cols = fw_ac:sw_chr) %>% 
  mutate(
    unit = 'ug/l',
    cas_chk = webchem::as.cas(casrn)
  )



# Human -------------------------------------------------------------------


hhc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn','hh_wo', 'hh_o', 'year', 'notes')) %>% 
  pivot_longer(., cols = c('hh_wo', 'hh_o')) %>% 
  mutate(
    unit = 'ug/l',
    cas_chk = webchem::as.cas(casrn)
  )


# Taste -------------------------------------------------------------------


oe <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-organoleptic-effects') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'result')) %>% 
  mutate(
    unit = 'ug/l',
    cas_chk = webchem::as.cas(casrn)
  ) %>% 
  filter(analyte != '1Non-Priority pollutant.') %>% 
  rename(value = result)

raw <- list_rbind(list('alc' = alc, 'hhc' = hhc, 'oe' = oe), names_to = 'frame')

rm(alc, hhc, oe)

# cleaning ----------------------------------------------------------------

raw_dat <- raw %>% 
  distinct(analyte, casrn, cas_chk, .keep_all = F) %>% 
  mutate(
    idx = 1:n(),
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

good <- raw_dat %>% 
  filter(!is.na(cas_chk)) %>% 
  mutate(cas_chk = unname(cas_chk))

good_cas <- ct_search(query = good$cas_chk, request_method = 'GET', search_method = 'equal')

good_src <- good_cas %>% 
  arrange(rank) %>% 
  distinct(raw_search, .keep_all = T)

good_dat <- inner_join(good, good_src, join_by(cas_chk == raw_search)) %>% 
  select(idx, dtxsid)

raw_cur <- bind_rows(list(good_dat, bad_dat)) %>% 
  left_join(raw_dat, ., join_by(idx)) %>% 
  select(-raw_name) %>% 
  arrange(dtxsid)

rm(
  list = setdiff(ls(), "raw_cur")
)


# hashing -----------------------------------------------------------------
{
params <- raw_cur %>% 
  filter(is.na(dtxsid)) %>% 
  select(analyte) %>% 
  mutate(raw_search = str_remove_all(analyte, pattern = '\\*') %>%
                    str_remove_all(., pattern = '\\(P\\)') %>% 
                    str_remove_all(., pattern = "\\([^()]*\\)") %>% 
                    str_squish())

chars <- rio::import(here('dict','Characteristic.csv')) %>% 
  clean_names() %>% 
  select(name, cas_number, srs_id)

{
# srs_b <- map(params$raw_search, ~srs_search(query = .x, method = 'begins'), .progress = T) %>% 
#     set_names(., params$raw_search) %>% 
#     list_rbind(names_to = 'raw_search')
  
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
  unlist()

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

rm(srs_e, srs_c)

params_cur <- list(srs_e_details, srs_c_details) %>%
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

# missing <- params %>% 
#   anti_join(., params_cur, join_by(raw_search))
  }
}
# 
# hash <- 
#   map(
#     c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"), 
#     ~ {stringdist_join(params_hash, chars,
#                    by = c('raw_search' = 'name'),
#                    mode = 'right',
#                    method = .x,
#                    #max_dist = 9,
#                    distance_col = 'dist') %>% 
#       group_by(analyte) %>% 
#       filter(!is.na(dist)) %>% 
#       slice_min(order_by = dist, n = 5)
#       },
#     .progress = T) %>% 
#   set_names(., c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"))
# 
# #TODO Repeat this as each method for comparison...? 
# {
# hash_1 <- 
#   keep(hash, names(hash) %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram")) %>% 
#   list_rbind(names_to = 'method') %>% 
#   filter(!is.na(srs_id)) %>% 
#   distinct(analyte, name, cas_number, srs_id, .keep_all = T) %>% 
#   select(-c(raw_search, method, dist)) %>% 
#   mutate(srs_id = as.character(srs_id))
# 
# hash_2 <- 
#   keep(hash, names(hash) %in% c("cosine", "jaccard", "jw")) %>% 
#   list_rbind(names_to = 'method') %>% 
#   group_by(analyte) %>% 
#   slice_min(dist, n = 1) %>% 
#   filter(!(analyte %in% hash_1$analyte)) %>% 
#   distinct(analyte, name, cas_number, srs_id, .keep_all = T) %>% 
#   filter(dist <= 0.1) %>% 
#   select(-c(raw_search, method, dist)) %>% 
#   ungroup() %>% 
#   mutate(srs_id = as.character(srs_id))
# 
# #NOTE Unlikely to produce any good results
# 
# 
# {
#   hash_3 <- 
#     keep(hash, names(hash) %in% c("cosine", "jaccard", "jw")) %>% 
#     list_rbind(names_to = 'method') %>% 
#     filter(!c(analyte %in% c(hash_1$analyte, hash_2$analyte))) %>% 
#     group_by(analyte) %>% 
#     slice_min(dist, n = 1) %>% 
#     distinct(analyte, raw_search)
#   
#   hash_4 <- map(hash_3$raw_search, ~srs_search(query = .x, method = 'begins'), .progress = T) %>% 
#     set_names(., hash_3$analyte) %>% 
#     list_rbind(names_to = 'raw_search') %>% 
#     select(raw_search, itn)
# 
#   hash_4 <- map(hash_4$itn, ~srs_details(query = .x), .progress = T) %>% 
#     set_names(., hash_4$raw_search) %>% 
#     list_rbind(names_to = 'raw_search') %>% 
#     select(raw_search, systematicName, internalTrackingNumber) %>% 
#     rename(
#       name = systematicName,
#       srs_id = internalTrackingNumber
#     ) %>% 
#     mutate(name = str_to_title(name))
#   
#   hash_3 <- hash_3 %>% 
#     left_join(., hash_4, join_by(raw_search)) %>% 
#     select(-raw_search) %>% 
#     ungroup() %>% 
#     mutate(srs_id = as.character(srs_id))
#   
#   rm(hash_4)
#   } 
# 
#   params <- list_rbind(list(hash_1, hash_2, hash_3)) %>% 
#     select(-cas_number)
#   
#   rm(hash_1, hash_2, hash_3, hash, chars)
#   }
# }

  
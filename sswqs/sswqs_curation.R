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
  
  todor::todor()
}


# functions ---------------------------------------------------------------

srs_search <- function(query, method){
  request("https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/autoComplete/nameSearch") |>
    req_url_query(
      #begins, contains, exact
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

{
  
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

 
 remapped <- parent_dat$pollutants %>%
   filter(!is.na(remap)) %>% 
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
   list_rbind(names_to = 'raw_search')
 
 srs_e_details <- map(srs_e$itn, ~srs_details(query = .x), .progress = T) %>% 
   set_names(., srs_e$raw_search) %>% 
   list_rbind(names_to = 'raw_search') %>% 
   group_by(raw_search) %>% 
   arrange(desc(synonyms)) %>% 
   ungroup() %>% 
   distinct(raw_search, .keep_all = T)
 
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
   list_rbind(names_to = 'raw_search')
 
 srs_c_details <- map(srs_c$itn, ~srs_details(query = .x), .progress = T) %>% 
   set_names(., srs_c$raw_search) %>% 
   list_rbind(names_to = 'raw_search') %>% 
   group_by(raw_search) %>% 
   arrange(desc(synonyms)) %>% 
   ungroup() %>% 
   distinct(raw_search, .keep_all = T) 
 
 srs_c_details <- srs_c_details %>% 
   mutate(dtxsid = case_when(
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
   
   write_rds(pol_final, file = here('sswqs', 'pollutant_final.RDS'))
   
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
     mutate(result = raw_result %>% as.numeric()) %>% 
     filter(!is.na(result))

# character ---------------------------------------------------------------
  
   
   block_list <- c(
     "See",
     "SEE",
     "see", 
     "Not Detectable", 
     "<", 
     "within", 
     "%", 
     "Calculated", 
     "million"
   )
   
   
   result_idx_char <- result_idx %>% 
     filter(idx %ni% result_idx_num$idx)
   
   
   
#Range----

# For finding narrative or funky standards
# wqs_bad <- wqs_temp %>% 
#   filter(str_detect(criterion_value, 'See|SEE|see|/|Not Detectable|<|within|%|Calculated|million'))

temp <- wqs_temp %>% 
  filter(!str_detect(criterion_value, block_list)) %>% 
  mutate(
    orig_crit_value = criterion_value,
       criterion_value = str_remove_all(criterion_value, '[[:blank:]]|[[:symbol:]]|\\,|\\u00ad'), #removes spaces, other symbols ><, commas, soft hyphens
       criterion_value = str_replace_all(criterion_value,'\\u2013|\\u2014','-'), #removes em dashes, soft hyphens
       criterion_value = str_replace_all(criterion_value, pattern = 'e|x10|X10', replacement = 'E'),
       IS_RANGE = NA,
       sci_note_count = str_count(criterion_value, 'E'),
       IS_RANGE = case_when( 
         #does overwrite the upload....
         sci_note_count == 1 & str_detect(criterion_value, '-', negate = TRUE) ~ FALSE, #positive sci notation
         sci_note_count == 1 & str_detect(criterion_value, '-') ~ FALSE, #negative sci notation
         sci_note_count == 2 & str_detect(criterion_value, '-') ~ TRUE, #range with sci notation
         sci_note_count == 0 & str_detect(criterion_value, '-') ~ TRUE, #range w/o sci notation
         sci_note_count == 0 & str_detect(criterion_value, '-', negate = TRUE) ~ FALSE, #numerical
         #.default = IS_RANGE
  )
)

stds_r <- temp %>% 
  filter(IS_RANGE == TRUE & sci_note_count != 2) %>% 
  separate(criterion_value, into = c('range_l', 'range_u'), sep = '-') %>%
  mutate(across(c(range_l, range_u), as.numeric)) %>%
  select(-sci_note_count) %>% 
  rowwise() %>% 
  mutate(criterion_value = mean(c(range_l, range_u))) %>% 
  relocate(criterion_value, .after = preferredName)

stds_nr <- temp %>% 
  filter(IS_RANGE == FALSE) %>% 
  mutate(range_l = NA_real_, range_u = NA_real_) %>% 
  mutate(criterion_value = as.numeric(criterion_value)) %>% 
  select(-sci_note_count)

stds_bad <- stds_nr %>% filter(is.na(criterion_value)) %>% select(criterion_id)

temp %>% 
  filter(criterion_id %in% stds_bad$criterion_id)

stds_r_sci <- temp %>% 
  filter(IS_RANGE == TRUE & sci_note_count != 0) %>% 
  separate(criterion_value, into = c('range_l','e_l', 'range_u','e_u'), sep = '-') %>%
  mutate(range_l = paste0(range_l,'-', e_l),
         range_u = paste0(range_u,'-', e_u)) %>% 
  select(-c(e_l, e_u)) %>% 
  mutate(across(c(range_l, range_u), as.numeric)) %>%
  select(-sci_note_count) %>% 
  rowwise() %>% 
  mutate(criterion_value = mean(c(range_l, range_u))) %>% 
  relocate(criterion_value, .after = preferredName)
  
temp <- bind_rows(stds_r, stds_nr, stds_r_sci)

rm(stds_r, stds_nr, stds_r_sci)

wqs_temp <- temp

#Units----

temp <- wqs_temp %>%
  #degree
  mutate(unit_name = str_remove_all(unit_name, pattern = '\\u00b0')) %>% #Removes µ
  mutate(unit_name = str_replace_all(unit_name, pattern = '\\u00b5', replacement = 'u')) %>%
  mutate(original_unit = unit_name) %>% 
  ungroup()

#Diagnostic
w_units <- temp %>% 
  count(unit_name) %>% 
  arrange(desc(n))
rm(w_units)

unit_dict <- temp %>%
  distinct(unit_name, .keep_all = T) %>%
  select(unit_name, original_unit) %>% 
  mutate(conversion = NA_real_) %>%
  relocate(original_unit, .before = unit_name)

rio::export(unit_dict, file = 'units_dict_dump.xlsx')

#Unit cleaning----

unit_dict <- rio::import(file = 'sswqs_units_curated.xlsx')

temp1 <- temp %>% 
  select(-unit_name) %>%
  left_join(., unit_dict, join_by('original_unit')) %>% 
  filter(unit_name != 'REMOVE' ) %>% 
  filter(is.na(preferredName) | preferredName != "temperature") %>% #issues with records that differentials records
  rowwise() %>% 
  mutate(needs_convert = !identical(unit_name, original_unit),
         orig_parsed_value = criterion_value, 
         orig_range_l = range_l,
         orig_range_u = range_u) %>% 
  ungroup() %>% 
  mutate(
    across(
      c(criterion_value, range_l, range_u), ~case_when(
      needs_convert == TRUE ~ .*conversion
      ,.default = .)
  ))

  
  # missing protection data -------------------------------------------------

temp2 <- temp1 %>% 
  mutate(
    meta = NA_character_,
    cit = 'State-Specific Water Quality Standards',
    protection = case_when(
      criteriatypeaquahumhlth == 'A' ~ 'Aquatic',
      criteriatypeaquahumhlth == 'H' ~ 'Human',
      criteriatypeaquahumhlth == 'O' ~ 'Organoleptic'
      ,.default = criteriatypeaquahumhlth),
    sourcewater = case_when(
      criteriatypefreshsaltwater == 'S' ~ 'Salt Water',
      criteriatypefreshsaltwater == 'B' ~ 'Brackish',
      criteriatypefreshsaltwater == 'F' ~ 'Fresh Water',
      criteriatypefreshsaltwater == 'SW' ~ 'Storm Water',
      criteriatypefreshsaltwater == 'I' ~ 'Industry Process Water',
      criteriatypefreshsaltwater == 'MN' ~ 'Treated Municipal Wastewater',
      criteriatypefreshsaltwater == 'CW'~ 'Onsite Collected Waters'
      ,.default = criteriatypefreshsaltwater),
    duration = case_when(
      criteriatype_acutechronic == 'A' ~ 'Acute',
      criteriatype_acutechronic == 'C' ~ 'Chronic',
      criteriatype_acutechronic == 'S' ~ 'Sample',
      criteriatype_acutechronic == 'D' ~ 'Daily',
      criteriatype_acutechronic == 'W' ~ 'Weekly',
      criteriatype_acutechronic == 'M' ~ 'Monthly',
      criteriatype_acutechronic == 'Y' ~ 'Yearly'
      ,.default = criteriatype_acutechronic),
    #NOTE Update to endpoint + combine wuth protection? 
    enduse = case_when(
      criteriatype_waterorg == 'O' ~ 'Organism',
      criteriatype_waterorg == 'W' ~ 'Water & Organism',
      criteriatype_waterorg == 'A' ~ 'Agriculture',
      criteriatype_waterorg == 'L' ~ 'Landscaping',
      criteriatype_waterorg == 'NPR' ~ 'Centralized Non-Potable Reuse',
      criteriatype_waterorg == 'PWR' ~ 'Potable Water Reuse',
      criteriatype_waterorg == 'I' ~ 'Impoundments',
      criteriatype_waterorg == 'ER' ~ 'Environmental Restoration',
      criteriatype_waterorg == 'IND' ~ 'Industry',
      criteriatype_waterorg == 'NPWR' ~ 'Onsite Non-Potable Water Reuse',
      criteriatype_waterorg == 'LIV' ~ 'Livestock'
      ,.default = criteriatype_waterorg)) %>% 
  rename(
    local = use_class_name_location_etc
  ) %>% 
  ungroup() %>% 
  select(-c(criteriatypeaquahumhlth:criteriatype_waterorg)) %>% 
  select(criterion_id:entity_name, final:preferredName, criterion_value, unit_name, IS_RANGE, range_l, range_u, local, meta:enduse) %>% 
  mutate(across(protection:enduse, ~replace_na(., 'UNC')))

dtx <- temp2 %>%
  filter(final != 'bulk') %>%
  distinct(final, .keep_all = F)

dtx <- ct_details(query = dtx$final, projection = 'id') %>% 
  select(dtxsid, preferredName)

temp3 <- temp2 %>%
  split(is.na(.$preferredName)) %>% 
  map(., ~rename(., dtxsid = final))

temp3$`TRUE` <- temp3$`TRUE` %>% 
  select(-preferredName) %>% 
  inner_join(., dtx, join_by(dtxsid))

temp3 <- list_rbind(temp3)

entity_list <- temp3 %>% 
  distinct(., entity_name, .keep_all = F) %>% 
  mutate(
    source = 'State-Specific Water Quality Standards', 
    subsource = NA_character_,
    origin_category = case_when(
      entity_name %in% c(
        state.name, #from dataset package
        'California - Statewide',
        'California Region',
        'CTR - California Toxics Rule') ~ 'State',
      str_detect(entity_name, 'California Region') ~ 'State',
      entity_name %in% c(
        #NOTE Adjust this to mirror other data sets
        'EPA 304(a) Recommended Criteria', 
        'NTR - National Toxics Rule') ~ 'Federal',
      .default = 'Other'),
     origin_supercategory = NA_character_,
     origin_agency = NA_character_,
     data_category = 'Primary',
     priority_id = case_when(
       origin_category == 'State' ~ 1,
       origin_category == 'Federal' ~ 1, 
       .default = 3 #For tribes and others
     )
)

temp4 <- temp3 %>% 
  left_join(., entity_list, join_by('entity_name')) %>% 
  rename(is_range = IS_RANGE) %>% 
  relocate(protection:enduse, .after = range_u)

# TODO Change to RDS format
# rio::export(temp4, file = paste0('sswqs_curated_', Sys.Date(), '.xlsx'))
# 
# rm(list = ls())

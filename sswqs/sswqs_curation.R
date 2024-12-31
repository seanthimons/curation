{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(V8)
  library(magrittr)
  library(ComptoxR)
  library(stringdist)
  
}


# block list --------------------------------------------------------------

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

#Download----
{
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
  state_dat <- state_vars %>%
    pmap(., function(abv, json) {
      message(abv, "\n")
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
          # pluck(., 1) %>%
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
 
 parent_dat$units %<>%
   enframe(., name = 'idx', value = 'v') %>% 
   unnest_wider(., 'v', names_sep = '') %>% 
   select(-v2) %>% 
   rename(
     unit = v1
   )

 raw_pol <- parent_dat$pollutants %>%
   filter(is.na(dtxsid)
          , is.na(remap)
          ) %>% 
   distinct(., analyte, cas, .keep_all = T) %>% 
   filter((idx %in% crit_dat$analyte))
 
 stats <- crit_dat %>% 
   filter(analyte %in% raw_pol$idx) %>% 
   count(analyte) %>% 
   arrange(desc(n)) %>% 
   ungroup() %>% 
   left_join(., raw_pol, join_by(analyte == idx))
 
 raw_pol_cas <- raw_pol %>% 
   filter(!is.na(cas)) %>% 
   distinct(cas)
 
 raw_pol_cas <- ct_search(type = 'string', query = raw_pol_cas$cas, suggestions = F)
 
 raw_pol_cas <- raw_pol_cas %>% 
   arrange(rank) %>% 
   distinct(searchValue, .keep_all = T) %>% 
   select(searchValue, dtxsid) %>% 
   rename(dtxsid_cas = dtxsid)
 
 cur_pol <- left_join(raw_pol, raw_pol_cas, join_by(cas == searchValue))
 
 tada <- rio::import(here::here('sswqs', 'TADASynonymTable.csv')) %>% 
   clean_names() %>% 
   select(tada_characteristic_name, harmonization_group) %>% 
   rename(target = tada_characteristic_name) %>% 
   mutate(harmonization_group = case_when(
     is.na(harmonization_group) ~ target, 
     .default = harmonization_group
   )) %>% 
   distinct(., .keep_all = T)
 
 raw_pol_name <- cur_pol %>% 
   filter(is.na(dtxsid_cas)) %>% 
   select(idx, analyte) %>% 
   mutate(raw_analyte = analyte,
          analyte = str_to_upper(analyte)) %>% 
   left_join(., tada, join_by(analyte == target)) %>% 
   distinct(., idx, .keep_all = T)
 
 raw_pol_name <- ct_search(type = 'string', query = raw_pol_name$analyte, suggestions = F)
 
 
 
}

#Processing----

uc <- distinct(wqs, std_poll_id, std_pollutant_name, cas_no) %>%
  mutate(std_poll_id = as.numeric(std_poll_id), 
         cas_no = webchem::as.cas(cas_no) %>% unname())
  

rio::export(uc, file = paste0('sswqs_unique_dump_', Sys.Date(),'.xlsx'))

#Clean the unique records here, overwrite the file

rm(uc)


#WQS Cleaning-----

dict <- rio::import('sswqs_unique_curated.xlsx') %>%
  filter(final != 'remove') %>% 
  mutate(std_poll_id = paste0(as.character(std_poll_id),'.0')) %>% 
  select(!c('std_pollutant_name', 'cas_no')) %>% 
  distinct(.keep_all = T)

wqs_temp <- inner_join(wqs, dict, by = 'std_poll_id') %>% 
  select(!c(
    entity_id,
    entity_abbr,
    pollutant_id:std_pollutant_name,
    crit_source_id,
    use_class_name_location_etc_id,
    last_entry_in_db,
    effective_date,
    unit_id,
    pdfpgno
    ))

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
      criteriatypefreshsaltwater == 'S' ~ 'Salt water',
      criteriatypefreshsaltwater == 'B' ~ 'Brackish',
      criteriatypefreshsaltwater == 'F' ~ 'Freshwater',
      criteriatypefreshsaltwater == 'SW' ~ 'Stormwater',
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

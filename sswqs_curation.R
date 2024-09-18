{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(V8)
  library(magrittr)
  library(ComptoxR)
  
}

#Download----
{
temp <- tempfile(fileext = ".xlsx")
download.file('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx', temp, mode = "wb")
if(dir.exists('temp')){unlink("temp", recursive = T)}

dir.create(here('temp'))

file.copy(from = temp,
          to = here('temp'))

setwd(here('temp'))

wqs <- rio::import(list.files(path = '.', pattern = "\\.xlsx$", full.names = TRUE))
unlink(temp, recursive = T)
setwd(here())
unlink("temp", recursive = T)
rm(temp)
}

##Criteria names----
index = which(wqs$ReportName == 'CRIT_SOURCE_ID') %>% max()

wqs_critnames <- wqs[c(4:index-1), c(1:2)] #get the colnames for the table
colnames(wqs_critnames) <- c('col', 'expl')
rownames(wqs_critnames) <- NULL
wqs_critnames <- wqs_critnames %>% filter(!is.na(col))

wqs_temp <- wqs[index:nrow(wqs), ]
rownames(wqs_temp) <- NULL

##Links----

index <- which(wqs_temp$ReportName == 'CRITERION_ID') %>% max()

wqs_links <- wqs_temp[1:index-1, 1:2]
rownames(wqs_links) <- NULL

wqs_temp <- wqs_temp[index:nrow(wqs_temp), ]

##Data----
names(wqs_temp) = as.matrix(wqs_temp[1,])

wqs <-  wqs_temp[-1,]

rownames(wqs) <- NULL
wqs <- clean_names(wqs) %>% as_tibble()

rm(wqs_temp, 
   wqs_links,
   wqs_critnames,
   index)

write_rds(wqs, file = 'raw_wqs.Rds')

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
  filter(!str_detect(criterion_value, 'See|SEE|see|/|Not Detectable|<|within|%|Calculated|million')) %>% 
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

rio::export(temp4, file = paste0('sswqs_curated_', Sys.Date(), '.xlsx'))

rm(list = ls())

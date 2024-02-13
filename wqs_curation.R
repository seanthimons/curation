{
  library(here)
  library(readxl)
  library(tidyverse)
  library(rio)
  library(timeplyr)
  library(janitor)
  library(ComptoxR)
  library(webchem)
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

wqs_critnames <- wqs[c(4:26), c(1:2)] #get the colnames for the table
colnames(wqs_critnames) <- c('col', 'expl')
rownames(wqs_critnames) <- NULL

threshold <- 'CRIT_SOURCE_ID'
index <- which(wqs$ReportName == threshold) %>% max()

wqs_temp <- wqs[index:nrow(wqs), ]
rownames(wqs_temp) <- NULL

##Links----
threshold <- 'CRITERION_ID'
index <- which(wqs_temp$ReportName == threshold) %>% max()

wqs_links <- wqs_temp[1:index-1, 1:2]
rownames(wqs_links) <- NULL

wqs_temp <- wqs_temp[index:nrow(wqs_temp), ]

##Data----
names(wqs_temp) = as.matrix(wqs_temp[1,])

wqs <-  wqs_temp[-1,]

rownames(wqs) <- NULL
wqs <- clean_names(wqs) %>% as.data.frame()

rm(wqs_temp, 
   wqs_links,
   wqs_critnames,
   index,
   threshold)

write_rds(wqs, file = 'raw_wqs.Rds')

#Processing----

w_units <- rio::import('dict_units_wqs.csv')
missing_dict <- rio::import('dict_missing_wqs.xlsx')
missing_keep <- missing_dict %>% filter(fix != 'GROUP' & fix != 'REMOVE')
missing_remove <- missing_dict %>% filter(fix == 'GROUP' | fix == 'REMOVE')
rm(missing_dict)

require(webchem)
wqs <- wqs %>% 
  mutate(cas_no = as.cas(cas_no))

wqs_search_distinct <- distinct(wqs, cas_no, std_pollutant_name, std_poll_id)

##Cas-----
{
  cas <- unique(wqs$cas_no) %>%
    as.data.frame() %>%
    filter(!is.na(.))
  
  wqs_cas_search <- ct_name(query = cas$., param = 'equal', debug = T)
  
  wqs_curated_cas <- inner_join(cas %>% as.data.frame(), wqs_cas_search, by = c('.' = 'searchValue')) %>% 
    rowwise() %>% 
    mutate(equal = identical(., casrn)) %>% 
    ungroup() %>% 
    rename(searchedValue = '.') %>% 
    select(searchedValue, casrn, preferredName, dtxsid
           #, equal
           )
  
  wqs_curated_cas_key <- inner_join(wqs_search_distinct, wqs_curated_cas, by = c("cas_no" = 'searchedValue'))
  wqs_curated_cas_missing <- anti_join(wqs_search_distinct, wqs_curated_cas, by = c("cas_no" = "searchedValue"))
  
  rm(wqs_cas_search, wqs_curated_cas)
}

##Name----
{
  name <- wqs_curated_cas_missing
  wqs_name_search <- ct_name(name$std_pollutant_name, param = 'equal', debug = T)
  
  wqs_name_search2 <- wqs_name_search %>%
    filter(!is.na(dtxsid))
  
  name <- name %>%
    mutate(caps = str_to_upper(std_pollutant_name))
  
  wqs_name_search2 <- wqs_name_search2 %>%
    mutate(caps = str_to_upper(searchValue))
  
  wqs_curated_name_key <- inner_join(name, wqs_name_search2, by = 'caps') %>%
    select(-c(title:suggestions)) %>% 
    select(cas_no,
           std_pollutant_name,
           std_poll_id,
          # searchValue,
           preferredName,
           dtxsid,
           casrn)
  
  wqs_curated_name_missing <- anti_join(name, wqs_name_search2, by = 'caps')
  
}

wqs_dictionary <- bind_rows(wqs_curated_cas_key, wqs_curated_name_key) %>% 
  distinct(std_poll_id, .keep_all = T)

rm(wqs_curated_cas_key, wqs_curated_name_key)
rm(name, cas, wqs_name_search, wqs_name_search2)

#Missing vars----
wqs_dictionary_missing <- wqs_search_distinct %>% 
  filter(!(std_poll_id %in% wqs_dictionary$std_poll_id))

rm(wqs_dictionary_missing)

##manual file + fixing -----
dict_missing <- read_xlsx('~/mapping/dict/dict_missing_wqs.xlsx') %>%
  select(-note)


dict_missing_dtx <- dict_missing %>%
  filter(str_detect(fix, 'DTX')) %>%
  select(-map) %>% 
  filter(!(fix %in% wqs_dictionary$dtxsid)) %>%
  inner_join(wqs_search_distinct, ., by = c('std_pollutant_name' = 'id')) %>% 
  rename(dtxsid = fix)

dict_dtx <- ct_details(dict_missing_dtx$dtxsid) %>% 
  select(dtxsid, preferredName, casrn)

dict_missing_dtx <- left_join(dict_missing_dtx, dict_dtx, by = 'dtxsid') %>% 
  distinct(std_poll_id, .keep_all = T) 

rm(dict_dtx)

dict_missing_params <- dict_missing %>%
  filter(!str_detect(fix, 'DTX')) %>%
  inner_join(wqs_search_distinct, ., by = c('std_pollutant_name' = 'id')) %>% 
  filter(fix != 'REMOVE') %>% 
  rename(preferredName = map) %>% 
  select(-fix)

dict_curated_key <- bind_rows(dict_missing_dtx, dict_missing_params)

rm(dict_missing, dict_missing_dtx, dict_missing_params)

dict_missing_bad<- dict_missing %>%
  filter(!str_detect(fix, 'DTX')) %>%
  inner_join(wqs_search_distinct, ., by = c('std_pollutant_name' = 'id')) %>% 
  filter(fix == 'REMOVE')

wqs_dictionary_final <- bind_rows(wqs_dictionary, dict_curated_key)

rm(wqs_dictionary, dict_curated_key)
rm(wqs_search_distinct)



#WQS Cleaning-----
#44519 to 43848 ~1.5%
holding_wqs <- validated_wqs <- wqs_temp <- inner_join(wqs, wqs_dictionary_final, by = c("std_poll_id" = "std_poll_id"))

wqs_temp <- wqs_temp %>%
  select(
    criterion_id,
    region,
    entity_name,
    entity_abbr,
    casrn,
    preferredName,
    criterion_value,
    unit_name,
    unit_id,
    criteriatypeaquahumhlth:criteriatype_waterorg,
    use_class_name_location_etc
    )

##Removing narrative stds----

wqs_temp <- wqs_temp %>%
  filter(!str_detect(criterion_value, 'See|SEE|see|/|Not Detectable|<|within|%|Calculated|million'))

#Range----

temp <- wqs_temp %>% 
  mutate( 
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
  mutate(unit_name = str_remove_all(unit_name, pattern = '\\u00b0')) %>%
  #µ
  mutate(unit_name = str_replace_all(unit_name, pattern = '\\u00b5', replacement = 'u')) %>%
  mutate(original_unit = unit_name)
  
w_units <- temp %>% 
  count(unit_name, unit_id) %>% 
  arrange(desc(n))

w_1 <- temp %>%
  mutate(unit_name = case_when(
    unit_name =='mg/l' ~ "ug/l"
    ,unit_name =='ng/l' ~ "ug/l" 
    ,unit_name =='ntu' ~ "NTU" 
    ,unit_name =='pg/l' ~ "ug/l" 
    ,unit_name =='F' ~ "C" 
    ,unit_name =='picocuries/l' ~ "pci/l" 
    ,unit_name =='ph units' ~ "standard units" 
    ,unit_name =='mg/kg' ~ "ug/l" 
    ,unit_name =='% of saturation value' ~ "REMOVE" 
    ,unit_name =='CFU/100 mL or MPN/100 mL' ~ "count/100ml" 
    ,unit_name =='meters' ~ "REMOVE" 
    ,unit_name =='MPN/100 ml' ~ "count/100ml" 
    ,unit_name =='organisms/100 ml' ~ "count/100ml" 
    ,unit_name =='%' ~ "REMOVE"  
    ,unit_name =='umhos/cm' ~ "uS/cm" 
    ,unit_name =='million fibers/l' ~ "fibers/l"
    ,unit_name =='ppm' ~ "ug/l" 
    ,unit_name =='% increase above background' ~ "REMOVE" 
    ,unit_name =='pounds per year' ~ "REMOVE" 
    ,unit_name =='threshold odor number' ~ "TON"
    ,unit_name =='no unit name' ~ "standard units" 
    ,unit_name =='mg/l as n' ~ "ug/l"
    ,unit_name =='[no units]' ~ "standard units" 
    ,unit_name =='mpn' ~ "count/100ml" 
    ,unit_name =='dS/m' ~ "uS/cm" 
    ,unit_name =='millivolts' ~ "mV" 
    ,unit_name =='pounds per acre-foot of lake volume per year' ~ "REMOVE" 
    ,unit_name =='μg/g' ~ "ug/l" 
    ,unit_name =='% below background levels' ~ "REMOVE" 
    ,unit_name =='g/l' ~ "ug/l" 
    ,unit_name =='lbs/year' ~ "REMOVE" 
    ,unit_name =='parts per billion (ppb)' ~ "ug/l" 
    ,unit_name =='ppq' ~ "ug/l" 
    ,unit_name =='total thms' ~ "REMOVE" 
    ,unit_name =='% of total cations' ~ "REMOVE" 
    ,unit_name =='kg/yr' ~ "REMOVE" 
    ,unit_name =='mf/l' ~ "fibers/l" 
    ,unit_name =='mg/L as CaCO3' ~ "ug/l" 
    ,unit_name =='millirems' ~ "mRem" 
    ,unit_name =='ug/kg' ~ "ug/l" 
    ,unit_name =='% of total count' ~ "REMOVE" 
    ,unit_name =='SAR' ~ "standard units" 
    ,unit_name =='change in PCU' ~ "REMOVE" 
    ,unit_name =='fg/l' ~ "ug/l" 
    ,unit_name =='mg TAN/L' ~ "REMOVE" 
    ,unit_name =='tons/million cubic meters of water' ~ "REMOVE" 
    ,unit_name =='% change from natural conditions' ~ "REMOVE" 
    ,unit_name =='% positive samples/month' ~ "REMOVE" 
    ,unit_name =='TUa' ~ "REMOVE" 
    ,unit_name =='TUc' ~ "REMOVE" 
    ,unit_name =='jtu' ~ "NTU" 
    ,unit_name =='meq/l' ~ "REMOVE"  
    ,unit_name =='microfibers/l' ~ "fibers/l" 
    ,unit_name =='micromhos' ~ "REMOVE" 
    ,unit_name =='tons' ~ "REMOVE"  
    ,.default = unit_name
  ))

w_units <- w_1 %>%
  filter(unit_name != 'REMOVE') %>% 
  count(unit_name
        #, original_unit
        ) %>% 
  arrange(desc(n))

w_r <- w_1 %>%
  filter(unit_name != 'REMOVE') %>%
  mutate(equal = identical(unit_name, original_unit)) %>%
  filter(equal == FALSE & IS_RANGE == TRUE) %>%
  mutate(
    orig_range_l = range_l,
    orig_range_u = range_u
    ) %>% 
  mutate(
    crit_val_conv = case_when(
      original_unit == 'F' ~ ((criterion_value - 32)*(5/9)),
      original_unit == 'mg/l' ~ criterion_value*1000,
      original_unit == 'ppm' ~ criterion_value*1000,
      original_unit == 'mg/l as n' ~ criterion_value*1000,
      original_unit == 'mg/l' ~ criterion_value*1000,
      .default = criterion_value),
    range_l = case_when(  
      original_unit == 'F' ~ ((range_l - 32)*(5/9)),
      original_unit == 'mg/l' ~ range_l*1000,
      original_unit == 'ppm' ~ range_l*1000,
      original_unit == 'mg/l as n' ~ range_l*1000,
      original_unit == 'mg/l' ~ range_l*1000,
      .default = range_l),
    range_u = case_when(  
      original_unit == 'F' ~ ((range_u - 32)*(5/9)),
      original_unit == 'mg/l' ~ range_u*1000,
      original_unit == 'ppm' ~ range_u*1000,
      original_unit == 'mg/l as n' ~ range_u*1000,
      original_unit == 'mg/l' ~ range_u*1000,
      .default = range_u
    )
) %>% 
  rename('original_value' = 'criterion_value')
  
w_s <- w_1 %>%
  filter(unit_name != 'REMOVE') %>% 
  mutate(equal = identical(unit_name, original_unit)) %>% 
  filter(equal == FALSE & IS_RANGE == FALSE) %>%  
  mutate(crit_val_conv = case_when(
    
    original_unit == 'F' ~ ((criterion_value - 32)*(5/9)),
    
    original_unit == 'million fibers/l' ~ criterion_value*1e6,
    original_unit == 'mf/l' ~ criterion_value*1e6,
    
    original_unit == 'dS/c,' ~ criterion_value*1e6,
    
    original_unit == 'mg/l' ~ criterion_value*1000,
    original_unit == 'mg/l as n' ~ criterion_value*1000,
    original_unit == 'ng/l' ~ criterion_value*0.001,
    original_unit == 'pg/l' ~ criterion_value*1e-6,
    
    original_unit == 'mg/kg' ~ criterion_value*1000,
    original_unit == 'μg/g' ~ criterion_value*1000,
    original_unit == 'g/l' ~ criterion_value*1e6,
    original_unit == 'ppq' ~ criterion_value*0.001,
    original_unit == 'mg/L as CaCO3' ~ criterion_value*1000,
    original_unit == 'fg/l' ~ criterion_value*1e-9
    
    ,.default = criterion_value
  )) %>%
  mutate(orig_range_l = range_l,
         orig_range_u = range_u) %>% 
  rename('original_value' = 'criterion_value') 
  

w_ok <- w_1 %>%
  filter(unit_name != 'REMOVE') %>% 
  mutate(equal = identical(unit_name, original_unit)) %>% 
  filter(equal == TRUE) %>% 
  mutate(crit_val_conv = criterion_value,
         original_unit = unit_name,
         orig_range_l = range_l,
         orig_range_u = range_u) %>% 
  rename('original_value' = 'criterion_value')
  
temp2 <- bind_rows(w_ok, w_r, w_s, .id = 'conv_source') %>% 
  relocate(crit_val_conv, .after = preferredName) %>% 
  relocate(is_range = IS_RANGE, .after = crit_val_conv) %>% 
  relocate(original_value,conv_source, .after = equal)

rm(w_ok, w_r, w_s)

wqs_temp <- temp2

#crit fix-----

temp <- wqs_temp %>% 
  mutate(
    meta = NA_character_,
    cit = 'SSWQS',
    protection = case_when(
      criteriatypeaquahumhlth == 'A' ~ 'Aquatic',
      criteriatypeaquahumhlth == 'H' ~ 'Human',
      criteriatypeaquahumhlth == 'O' ~ 'Organoleptic'
      ,TRUE ~ criteriatypeaquahumhlth),
    sourcewater = case_when(
      criteriatypefreshsaltwater == 'S' ~ 'Salt water',
      criteriatypefreshsaltwater == 'B' ~ 'Brackish',
      criteriatypefreshsaltwater == 'F' ~ 'Freshwater',
      criteriatypefreshsaltwater == 'SW' ~ 'Stormwater',
      criteriatypefreshsaltwater == 'I' ~ 'Industry Process Water',
      criteriatypefreshsaltwater == 'MN' ~ 'Treated Municipal Wastewater',
      criteriatypefreshsaltwater == 'CW'~ 'Onsite Collected Waters'
      ,TRUE ~ criteriatypefreshsaltwater),
    duration = case_when(
      criteriatype_acutechronic == 'A' ~ 'Acute',
      criteriatype_acutechronic == 'C' ~ 'Chronic',
      criteriatype_acutechronic == 'S' ~ 'Sample',
      criteriatype_acutechronic == 'D' ~ 'Daily',
      criteriatype_acutechronic == 'W' ~ 'Weekly',
      criteriatype_acutechronic == 'M' ~ 'Monthly',
      criteriatype_acutechronic == 'Y' ~ 'Yearly'
      ,TRUE ~ criteriatype_acutechronic),
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
      ,TRUE ~ criteriatype_waterorg)) %>% 
  rename(
    local = use_class_name_location_etc
  ) %>% 
  ungroup() %>% 
  select(-c(criteriatypeaquahumhlth:criteriatype_waterorg)) %>% 
  left_join(., select(wqs, criterion_id, std_poll_id), by = 'criterion_id') %>% 
  left_join(., select(wqs_dictionary_final, std_poll_id, dtxsid, preferredName), by = 'std_poll_id')
  

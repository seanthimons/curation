{
  library(here)
  library(readxl)
  library(tidyverse)
  library(rio)
  library(timeplyr)
  library(janitor)
}

#Custom WQS----
stds <- rio::import_list("combined_wqs.xlsx") %>% .[-c(1, #cover
                                                       2, #user
                                                       #4, #re
                                                       #5, #livestock
                                                       9  #other
)]
##Cleaning standards----

stds1 <-
  map(stds, ~mutate(., 
                    CRITERION_VALUE = as.character(CRITERION_VALUE),
                    CRITERION_VALUE = str_remove_all(CRITERION_VALUE, '[[:blank:]]|[[:symbol:]]|\\,|\\u00ad'), #removes spaces, other symbols ><, commas, soft hyphens
                    CRITERION_VALUE = str_replace_all(CRITERION_VALUE,'\\u2013|\\u2014','-'), #removes em dashes, soft hyphens
                    CRITERION_VALUE = str_replace_all(CRITERION_VALUE, pattern = 'e|x10|X10', replacement = 'E'),
                    IS_RANGE = as.logical(IS_RANGE),
                    sci_note_count = str_count(CRITERION_VALUE, 'E'),
                    IS_RANGE = case_when( #does overwrite the upload....
                      sci_note_count == 1 & str_detect(CRITERION_VALUE, '-', negate = TRUE) ~ FALSE, #positive sci notation
                      sci_note_count == 1 & str_detect(CRITERION_VALUE, '-') ~ FALSE, #negative sci notation
                      sci_note_count == 2 & str_detect(CRITERION_VALUE, '-') ~ TRUE, #range with sci notation
                      sci_note_count == 0 & str_detect(CRITERION_VALUE, '-') ~ TRUE, #range w/o sci notation
                      sci_note_count == 0 & str_detect(CRITERION_VALUE, '-', negate = TRUE) ~ FALSE, #numerical
                      #.default = IS_RANGE
                    ),
                    CRITERION_ID = 1:n(),
  )
  ) %>% 
  map_dfr(., \(x) x, .id = 'source')


stds_r <- stds1 %>% 
  filter(IS_RANGE == TRUE) %>%
  separate(CRITERION_VALUE, into = c('range_l', 'range_u'), sep = '-') %>%
  mutate(across(c(range_l, range_u), as.numeric)) %>%
  select(-sci_note_count) %>% 
  rowwise() %>% 
  mutate(CRITERION_VALUE = mean(c(range_l, range_u))) %>% 
  relocate(CRITERION_VALUE, .after = 'STD_POLLUTANT_NAME') 

stds_nr <- stds1 %>% 
  filter(IS_RANGE == FALSE) %>% 
  mutate(range_l = NA_real_, range_u = NA_real_) %>% 
  mutate(CRITERION_VALUE = as.numeric(CRITERION_VALUE)) %>% 
  select(-sci_note_count)

stds_curated <- bind_rows(stds_r, stds_nr)

rm(stds_nr, stds_r, stds1, stds)

stds_curated1 <- stds_curated %>% 
  mutate(media = NA) %>% 
  relocate(media, .before = PROTECTION) %>% 
  #unit conversions
  mutate(UNIT_NAME = case_when(
    UNIT_NAME == 'µg/l' ~ 'ug/l',
    UNIT_NAME == 'µS/cm' ~ 'uS/cm',
    UNIT_NAME == 'µmhos/cm' ~ 'uS/cm',
    UNIT_NAME == 'umhos/cm' ~ 'uS/cm',
    UNIT_NAME == 'micromhos' ~ 'uS/cm',
    UNIT_NAME == 'micromhos/cm' ~ 'uS/cm',
    UNIT_NAME == 'ph units' ~ 'standard units',
    UNIT_NAME == 'ntu' ~ 'NTU',
    UNIT_NAME == 'MPN/100 ml' ~ 'count/100ml',
    UNIT_NAME == 'CFU/100ml' ~ 'count/100ml',
    UNIT_NAME == 'no unit name' ~ 'standard units',
    UNIT_NAME == '[no units]' ~ 'standard units',
    UNIT_NAME == 'no units' ~ 'standard units',
    UNIT_NAME == 'mg/l as n' ~ 'mg/l',
    UNIT_NAME == 'mg/L' ~ 'mg/l',
    UNIT_NAME == '(mg/kg-day)-1' ~ 'mg/kg-day',
    UNIT_NAME == 'MFL' ~ 'million fibers/l',
    UNIT_NAME == 'ratio' ~ 'standard units',
    UNIT_NAME == 'TON' ~ 'threshold odor number',
    .default = UNIT_NAME
  )) %>% 
  #value conversions
  mutate(CRITERION_VALUE = case_when(
    UNIT_NAME == 'mg/l' ~ CRITERION_VALUE*1000,
    UNIT_NAME == 'ng/l' ~ CRITERION_VALUE/1000,
    UNIT_NAME == 'pg/l' ~ CRITERION_VALUE/1e+6,
    UNIT_NAME == 'fg/l' ~ CRITERION_VALUE/1e+9,
    UNIT_NAME == 'g/l' ~ CRITERION_VALUE*1e+6,
    UNIT_NAME == 'million fibers/l' ~ CRITERION_VALUE*1e+6,
    UNIT_NAME == 'dS/m' ~ CRITERION_VALUE*1000, 
    .default = CRITERION_VALUE)) %>%
  #redoing unit names
  mutate(
    UNIT_NAME = case_when(
      UNIT_NAME == 'mg/l' ~ 'ug/l',
      UNIT_NAME == 'ng/l' ~ 'ug/l',
      UNIT_NAME == 'pg/l' ~ 'ug/l',
      UNIT_NAME == 'fg/l' ~ 'ug/l',
      UNIT_NAME == 'g/l' ~ 'ug/l',
      UNIT_NAME == 'million fibers/l' ~ 'fibers/l',
      UNIT_NAME == 'dS/m' ~ 'uS/cm',
      .default = UNIT_NAME))

##unique compounds----

unique_compounds <- stds_curated1 %>%
  distinct(., CAS_NO, STD_POLLUTANT_NAME, .keep_all = T) %>%
  select(source, CAS_NO, STD_POLLUTANT_NAME) %>% 
  mutate(
    STD_POLLUTANT_NAME = str_remove_all(STD_POLLUTANT_NAME, pattern = '\\~'),
    STD_POLLUTANT_NAME = str_replace_all(STD_POLLUTANT_NAME, pattern = '\r\n', ' '),
    STD_POLLUTANT_NAME = str_replace_all(STD_POLLUTANT_NAME, pattern = '\\(', ' \\(') %>% str_squish(),
    CAS_NO = str_replace_all(CAS_NO, '00-00-0', NA_character_),
    CAS_NO = str_replace_all(CAS_NO, 'NA', NA_character_),
    STD_POLLUTANT_NAME = str_trim(STD_POLLUTANT_NAME)) %>% 
  arrange(STD_POLLUTANT_NAME, desc(CAS_NO)) %>% 
  distinct(., STD_POLLUTANT_NAME, .keep_all = T)

if(file.exists('custom_wqs_uc.csv')){TRUE}else{write.csv(unique_compounds, file = 'custom_wqs_uc.csv')}

rm(stds_curated)
#----


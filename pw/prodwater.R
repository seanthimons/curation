
# packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(tidyverse)
  library(duckdb)
  library(duckplyr)
  library(nanoparquet)
  
  setwd(here('pw'))
}


# init --------------------------------------------------------------------

con <- dbConnect(duckdb(), dbdir = 'prod_water.duckdb')

# read in -----------------------------------------------------------------


## PA ----------------------------------------------------------------------

data.table::fread(file = here('pw', 'PA_DEP_26r_processed.csv')) %>%
  #janitor::clean_names() %>% 
  select(-c(
    V1 
    #`Original_Latitude`, `Original_Longitude`
    )) %>%
  mutate(
    across(
      where(is.character), ~na_if(.x, ""))) %>% 
  mutate(
    date = lubridate::mdy_hm(Date_Collected) %>% lubridate::as_date(), 
    #.keep = 'unused',
    .before = 1) %>% 
  pivot_longer(., cols = !c(
    'date',
    'Date_Collected',
    'Original_Latitude',
    'Original_Longitude',
    'NEWTS_Water_Type',
    'Original_Dataset_ID',
    'Sample_Description',
    'Sample_Matrix',
  ), names_to = 'raw_analyte', values_to = 'raw_val', values_drop_na = TRUE) %>% 
  write_parquet(., file = 'pa_prod_water')


duckdb::dbWriteTable(con, "pa_prod_water", read_parquet(here('pw','pa_prod_water')), overwrite = TRUE)

# tbl(con, 'pa_prod_water') %>%
#   #filter(raw_analyte == 'Europium_154') %>% 
#   group_by(raw_analyte) %>% 
#   summarize(
#     mean = mean(raw_val),
#     max = max(raw_val),
#     n = n()
#   ) %>% 
#   arrange(desc(n))



## NEWTS -------------------------------------------------------------------

  rio::import("NEWTS_FieldDictionary_052024.xlsx") %>% 
  .[,2:5] %>%
  row_to_names(., 1) %>% 
  clean_names() %>%
  dbWriteTable(con, 'newts_headers', ., overwrite = TRUE)

  tbl(con, 'newts_headers') %>% 
    filter(str_detect(feature_class_field_alias, pattern = 'ppm|\\/L|ppq|per mil|\\/|ft|TUa|NTU|FTU|%|mV|Celsius|TU|psi|pH|gravity|ohm', negate = T)) %>% 
    collect() %>% 
    dbWriteTable(con, 'newts_meta', value = .)
  
  tbl(con, 'newts_headers') %>% 
    filter(str_detect(feature_class_field_alias, pattern = 'ppm|\\/L|ppq|per mil|\\/|ft|TUa|NTU|FTU|%|mV|Celsius|TU|psi|pH|gravity|ohm', negate = F)) %>% 
    collect() %>% 
    dbWriteTable(con, 'newts_params', value = .)
  
  data.table::fread(file = "NEWTS_Integrated_Full_052024.csv") %>% 
    select(!c(1:3)) %>% 
    dbWriteTable(con, 'newts', value = .)

  np <- tbl(con, 'newts_params') %>%
    select('csv_field_name') %>%
    collect() %>%
    unlist() %>%
    unname()
  
  tbl(con, 'newts') %>% 
  
    #TEMP Need to assess columnn type and then pivot out? 
    
      pivot_longer(., cols = any_of(np), names_to = 'analyte', values_to = 'value', values_drop_na = TRUE)
  
    # collect() %>% 
    # dbWriteTable(con, 'newts_long', value = .)


# diagnostics -------------------------------------------------------------




# shutdown ----------------------------------------------------------------


dbDisconnect(con)
rm(con)


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

  n1 <- duckdb::duckdb_read_csv(con, name = 'newts', files = "NEWTS_Integrated_Full_052024.csv")

  newts <- data.table::fread(file = "NEWTS_Integrated_Full_052024.csv") %>% 
    select(!c(1:3)) %>% 
    
    glimpse()
    
 
  select(!(c(1:3))) %>% 
  
  pivot_longer(., cols = !c(
    'NEWTS_Water_Type',
    'newts_unique_id',
    'newts_source_id',
    'newts_source_name',
    'newts_source_url',
    'newts_source_reference',
    'Original_Dataset_ID',
    'Original_Latitude',
    'Original_Longitude',
    'LONGITUDE_WGS84',
    'LATITUDE_WGS84'))


# diagnostics -------------------------------------------------------------




dbDisconnect(con)

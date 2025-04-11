
# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(tidyverse)
  library(ComptoxR)
  library(arrow)
  library(duckdb)
  library(duckplyr)
  
  
  setwd(here('threat_build'))
  
}


# functions ---------------------------------------------------------------

query_db <- function(...){
  variables <- rlang::ensyms(...)
  
  dbListTables(threat_db) %>%
    map(., ~{
      tbl(threat_db, .x) %>%
        distinct(
          !!!variables
        ) %>%
        collect()
      
    }, .progress = TRUE) %>%
    list_rbind() %>% distinct()
}

pull_table <- function(...){
  
  variables <- rlang::ensyms(...)
  
  dbListTables(threat_db) %>%
    map(., ~{
      tbl(threat_db, .x) %>% 
        select(!!!variables) %>% 
        collect()
    }, .progress = TRUE) %>%
    list_rbind()
}

# init --------------------------------------------------------------------

if(file.exists('threat.duckdb')){
  
  #threat_db <- dbConnect(duckdb(), dbdir = 'threat.duckdb', read_only = FALSE)
  
  #dbListTables(threat_db)
  
  
}else{
  
  final_lof <- list.files(here('final')) %>% 
    str_subset(., pattern = '.RDS|bayes|treatment', negate = TRUE) %>% 
    #TEMP
    str_subset(., pattern = 'toxval_v96_1.parquet')
  
  final_lof
  
  threat_db <- 
    dbConnect(duckdb(), dbdir = ":memory:", read_only = FALSE)
    #dbConnect(duckdb(), dbdir = 'threat.duckdb', read_only = FALSE)
  
  final_lof %>% 
    walk(., function(x){
      cli::cli_alert_info(x, '\n')
      dbWriteTable(threat_db, str_remove(x, pattern = '.parquet'), read_parquet(here('final', x)), overwrite = TRUE)
      
    }, .progress = TRUE)
  
  dbListTables(threat_db)
}







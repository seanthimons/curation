
# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(tidyverse)
  #library(httr2)
  #library(rvest)
  library(ComptoxR)
  #library(jsonlite)
  
  
  setwd(here('epa'))
  #load('epa.Rdata')
}

# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=62e184ebe4b055edffbfc22b&page=0


# raw ---------------------------------------------------------------------

lof <- list.files(here('epa', 'toxval_raw'))

# tbl_names <- lof %>%
#   str_remove_all(., pattern = 'toxval_all_res_toxval_v96_0_|.xlsx') %>% 
#   janitor::make_clean_names(.)

raw <- 
  lof %>% 
  #.[1] %>% 
  map(., ~{
    .x <- readxl::read_excel(
      here('epa', 'toxval_raw', .x),
      col_types = c(
        "text"
      ),
      na = c("-", ""))
  }, .progress = TRUE) %>% 
    list_rbind()

write_rds(raw, file = here('final', 'toxval_9_6.RDS'))

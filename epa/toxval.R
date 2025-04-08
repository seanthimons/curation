
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
  library(arrow)
  
  setwd(here('epa'))
  #load('epa.Rdata')
}

# 9.6
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=62e184ebe4b055edffbfc22b&page=0

# 9.5
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=66853bf7e4b0a7c65d177bc3&page=0

# 9.4 
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=645a5c33e4b08a6b39438b19&page=0

# 9.3/2
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=631759b4e4b04f6bb132461a&page=0


# Clowder files -----------------------------------------------------------

toxval_ver <- read_html("https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=62e184ebe4b055edffbfc22b&page=0")

toxval_ver %>% 
html_elements(., xpath = '//*[@id="folderListDiv"]')
  #html_elements(., css = 'h3') %>% 
  html_text()
  
# raw ---------------------------------------------------------------------

dir.create('toxval_raw')
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

write_parquet(raw, sink = here('final', 'toxval_9_6.parquet'))

#write_rds(raw, file = here('final', 'toxval_9_6.RDS'))
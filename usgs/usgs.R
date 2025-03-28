
# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(magrittr)
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(ComptoxR)
  library(jsonlite)
  
  
  setwd(here('usgs'))
  #load('epa.Rdata')
}


# raw ---------------------------------------------------------------------

raw <- read_html('https://water.usgs.gov/water-resources/hbsl/')

tbl_headers <- raw %>%
  html_table() %>% 
  pluck(., 1) %>% 
  .[1,] %>% 
  unlist()

tbl <- read_json('https://water.usgs.gov/water-resources/hbsl/data/HBSL.json', simplifyVector = T) %>% 
  pluck(1)

df2 <- tbl %>% 
  select(-'Chemical Class', -`USGS Parameter Code(s)`) %>%
  pivot_longer()
  
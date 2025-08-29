# packages ----------------------------------------------------------------
{
  library(here)
  library(tidyverse)
  library(httr2)
  library(arrow)
  library(rvest)

  setwd(here('herts'))
}

# raw ---------------------------------------------------------------------

loc <- read_html('https://sitem.herts.ac.uk/aeru/bpdb/atoz.htm') %>%
  html_elements(., xpath = '//*[@id="maincontent"]/div') %>%
  html_elements(., "a") %>%
  html_attrs() %>%
  map(., ~ pluck(., 'href')) %>%
  compact() %>%
  keep(., ~ str_detect(.x, pattern = 'Reports'))

raw <- map(loc, ~ {})

# clean -------------------------------------------------------------------

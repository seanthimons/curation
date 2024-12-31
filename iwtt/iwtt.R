{
    library(rio)
    library(janitor)
    library(tidyverse)
    library(here)
    library(httr)
    library(rvest)
    library(polite)
  
  setwd(here('iwtt'))
}

url <- 'https://watersgeo.epa.gov/iwtt/assets/IWTT-DATA-DOWNLOAD.zip'
  
download.file(url = url, destfile = 'iwtt.zip')

unzip('iwtt.zip')

url <- 'https://watersgeo.epa.gov/iwtt/download-database'

resp <- read_html_live(url)
  html_elements(., xpath = '//*[@id="root"]/div/div/section/section/ul') 
  html_elements(., 'a') %>% 
  html_attr('href')
  .[str_detect(., pattern = '.xlsx')]

iwtt_dict <- list.files(here('iwtt'), pattern = '.xlsx')

iwtt_dict <- rio::import(file = here('iwtt', iwtt_dict))

iwtt_ls <- list.files(here('iwtt', 'IWTT-DATA-DOWNLOAD'))
iwtt_raw <- map(iwtt_ls, ~import(here('iwtt', 'IWTT-DATA-DOWNLOAD', .x))) %>%
  setNames(., iwtt_ls) %>% 
  map(., clean_names)


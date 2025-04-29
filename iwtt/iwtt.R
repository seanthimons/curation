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


# Download ----------------------------------------------------------------


read_html_live(url = 'https://watersgeo.epa.gov/iwtt/download-database') %>% 
  html_elements(., xpath = '//*[@id="root"]/div/div/section/section/ul') %>% 
  html_elements(., 'a') %>% 
  html_attr('href') %>% 
  str_remove_all(., "./assets/") %>% 
  map(., ~{
    download.file(
      url = paste0('https://watersgeo.epa.gov/iwtt/assets/', .x),
      destfile = .x,
      mode = 'wb'
    )
  })


# Load --------------------------------------------------------------------

dir.create(here('iwtt', 'raw'))

iwtt_dict <- rio::import_list(file = list.files(here('iwtt'), pattern = '.xlsx')) %>% 
  map(., clean_names)

setwd(here('iwtt', 'raw'))

unzip(zipfile = here('iwtt', list.files(here('iwtt'), pattern = '.zip'))) 

unlink(here('iwtt', list.files(here('iwtt'), pattern = '.zip')))



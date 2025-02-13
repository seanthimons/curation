

{
    library(rio)
    library(janitor)
    library(tidyverse)
    library(here)
    library(httr2)
    library(rvest)
  
    setwd(here('epa'))
}


nrwqc_alc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'fw_ac', 'fw_chr', 'sw_ac', 'sw_chr', 'year', 'notes')) %>% 
  pivot_longer(., cols = fw_ac:sw_chr) %>% 
  mutate(
    unit = 'ug/l',
    
  )

nrwqc_hhc <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table') %>% 
  html_elements(., xpath = '//*[@id="datatable"]') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn','hh_wo', 'hh_o', 'year', 'notes')) %>% 
  pivot_longer(., cols = c('hh_wo', 'hh_o')) %>% 
  mutate(
    unit = 'ug/l',
    
  )
  
nrwqc_oe <- read_html('https://www.epa.gov/wqc/national-recommended-water-quality-criteria-organoleptic-effects') %>% 
  html_table() %>% 
  pluck(., 1) %>% 
  `colnames<-`(., c('analyte', 'casrn', 'result')) %>% 
  mutate(
    unit = 'ug/l',
    
  )


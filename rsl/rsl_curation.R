{
  library(here)
  library(readxl)
  library(tidyverse)
  library(rio)
  #library(timeplyr)
  library(janitor)
  library(ComptoxR)
}

# THQ 1.0 -----------------------------------------------------------------
{
  temp <- tempfile(fileext = ".xlsx")
  download.file('https://epa-prgs.ornl.gov/chemicals/download/equis_rsl_output_HQ1.xlsx', temp, mode = "wb")
  if(dir.exists('temp')){unlink("temp", recursive = T)}
  
  dir.create(here('temp'))
  
  file.copy(from = temp,
            to = here('temp'))
  
  setwd(here('temp'))
  
  hq1_raw <- rio::import(list.files(path = '.', pattern = "\\.xlsx$", full.names = TRUE))
  unlink(temp, recursive = T)
  setwd(here())
  unlink("temp", recursive = T)
  rm(temp)
}

# THQ 0.1 -----------------------------------------------------------------
{
  temp <- tempfile(fileext = ".xlsx")
  download.file('https://epa-prgs.ornl.gov/chemicals/download/equis_rsl_output_HQpt1.xlsx', temp, mode = "wb")
  if(dir.exists('temp')){unlink("temp", recursive = T)}
  
  dir.create(here('temp'))
  
  file.copy(from = temp,
            to = here('temp'))
  
  setwd(here('temp'))
  
  hq01_raw <- rio::import(list.files(path = '.', pattern = "\\.xlsx$", full.names = TRUE))
  unlink(temp, recursive = T)
  setwd(here())
  unlink("temp", recursive = T)
  rm(temp)
}

rsl <- bind_rows(hq01 = hq01_raw, hq1 = hq1_raw, .id = 'level')
rm(hq1_raw, hq01_raw)

casrn <- rsl %>% 
  distinct(param_code)

casrn <- ComptoxR::ct_name(query = casrn$param_code, param = 'equal')


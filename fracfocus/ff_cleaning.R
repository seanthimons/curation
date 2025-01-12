#Packages----
{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(feather)
  library(ComptoxR)
}


#Downloads the FF db----
temp <- tempfile()
options(timeout = 300)
download.file('https://www.fracfocusdata.org/digitaldownload/FracFocusCSV.zip', temp)

if(dir.exists(here('temp'))){
  cli::cli_alert('Temp directory found!')
  unlink(here('temp'), recursive = TRUE)
  cli::cli_alert_success('Created temp dir')
  dir.create(here('temp'))
}else{
  cli::cli_alert_success('Created temp dir')
  dir.create(here('temp'))
}

unzip(temp, exdir = here('temp'))


#creates folders for the files----
{
  
##Disclosure----
if(dir.exists(here('disc'))){
  unlink(here('disc'), recursive = TRUE)
  dir.create(here('disc'))
}else{
  dir.create(here('disc'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'DisclosureList_[[:digit:]].csv$', full.names = TRUE)
file.copy(files_to_copy, here('disc'))
file.remove(files_to_copy)

##Water-----
if(dir.exists(here('water'))){
  unlink(here('water'), recursive = TRUE)
  dir.create(here('water'))
}else{
  dir.create(here('water'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'WaterSource_[[:digit:]].csv$', full.names = TRUE)
file.copy(files_to_copy, here('water'))
file.remove(files_to_copy)

##readme----
if(dir.exists(here('readme'))){
  unlink(here('readme'), recursive = TRUE)
  dir.create(here('readme'))
}else{
  dir.create(here('readme'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'readme csv.txt$', full.names = TRUE)
file.copy(files_to_copy, here('readme'))
file.remove(files_to_copy)

##frac records----
if(dir.exists(here('frac'))){
  unlink(here('frac'), recursive = TRUE)
  dir.create(here('frac'))
}else{
  dir.create(here('frac'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'FracFocusRegistry_\\d+.csv', full.names = TRUE)
file.copy(files_to_copy, here('frac'))
file.remove(files_to_copy)

rm(files_to_copy)
rm(temp)

}

#Stitches the db together-----

file_list <- list.files(path = here('fracfocus', 'FracFocusCSV'), pattern = '^FracFocusRegistry_\\d+.csv', full.names = TRUE)

frac_raw <- map(file_list, 
                     ~readr::read_csv(.x,
                                   col_types = cols(
                                     APINumber = col_character(),
                                     ClaimantCompany = col_character()
                                     ),
                                   na = c("", "NA", " ")),
                                  .id = "id",
                                  .progress = TRUE
                     ) %>%
  list_rbind() %>%
  clean_names()

write_feather(frac_raw,
              path = paste0(here('fracfocus'), '/frac_raw','_', Sys.Date())
              )

#Disclosure: Sites------

## Read in -----------------------------------------------------------------

file_list <- list.files(path = here('fracfocus', 'FracFocusCSV'), pattern = '^DisclosureList_\\d+.csv', full.names = TRUE)

disc_raw <- map(file_list, 
                ~readr::read_csv(.x,
                                 col_types = cols(
                                   APINumber = col_character()
                                 ),
                                 na = c("", "NA", " ")),
                .id = "id",
                .progress = TRUE
) %>%
  list_rbind() %>%
  clean_names()

disc <- disc_raw %>% 
  distinct(api_number, job_start_date, .keep_all = T) %>% 
  mutate(
    across(c(job_start_date,job_end_date), mdy_hms),
         job_diff = difftime(job_end_date,job_start_date, units = 'days'),
    jd = as.numeric(job_diff),
    jd_fix = case_when(
      jd >= 0 ~ TRUE,
      jd < 0 ~ FALSE)
         )


## Bad data ----------------------------------------------------------------

{
disc_bad <- list()

disc_bad$job_diff <- disc %>%
  filter(job_diff < 0) #removes bad dates

disc_bad$date <- disc %>%
  filter(job_start_date > Sys.Date()) #removes future jobs

disc_bad <- disc_bad %>%
  list_rbind(names_to = 'type') %>% 
  distinct()
}

# useful, but wait until the end? 
# frac_raw <- frac_raw %>% 
#   split(.$APINumber)

#FF cleaning----
##Meta data list for number of obs lost to bad data----
  {
    ff_meta_data <- vector(mode = 'list', length = 9L)
    ff_meta_data <- setNames(ff_meta_data, c('frac_raw',
                                           'ff_chemlist',
                                           'good_chem',
                                           'bad_chem',
                                           'bad_chem_cas_fail',
                                           'bad_chem_no_cas_match',
                                           'name_match',
                                           'bad_chem_no_name_match',
                                           'unique'))
  
    ff_meta_data$frac_raw <- nrow(frac_raw)
  }
  
  ###Chem list----
  
  chem_list <- frac_raw %>%
    count(ingredient_common_name, cas_number) %>%
    mutate(orig_cas = cas_number,
          cas_number = str_remove_all(cas_number,  "[^0-9]")
           ,cas_number = ifelse(cas_number == "", NA_character_, cas_number)
           ,cas_number = webchem::as.cas(cas_number)
           ) %>% 
    rowwise() %>% 
    filter(!is.na(cas_number)) %>% 
    arrange(desc(n)) %>%
    ungroup()

# CAS cleaning ------------------------------------------------------------


chem_raw_cas <- chem_list %>%
  distinct(cas_number) %>%
  as.list() %>% 
  map(., unname)

chem_raw_cas <- ComptoxR::ct_search(
  type = 'string',
  search_param = 'equal',
  query = chem_raw_cas$cas_number)

chem_cas <- chem_raw_cas %>% 
  arrange(searchValue, rank) %>% 
  select(dtxsid, preferredName, searchValue) %>% 
  distinct(searchValue, .keep_all = T)


##Product dictionary list-----
  
  product_family <- rio::import('products.xlsx')
  clean_names()
    mutate(
      spec_use = str_remove_all(spec_use, pattern = "\\(.*?\\)"),
      token = str_to_lower(spec_use),
      token = str_remove_all(token, pattern = "[^A-Za-z]")) %>% 
    rename(purpose = function_category,
           orig_purpose = description
           ) %>% 
    arrange(token) 
  
  

{
    library(rio)
    library(janitor)
    library(tidyverse)
    library(here)
    library(httr)
    library(rvest)
    library(polite)
    library(duckdb)
    library(duckplyr)
  
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


# Unzip --------------------------------------------------------------------

iwtt_dict <- rio::import_list(file = list.files(here('iwtt'), pattern = '.xlsx')) %>% 
  map(., clean_names)

dir.create(here('iwtt', 'raw'))

setwd(here('iwtt', 'raw'))

unzip(zipfile = here('iwtt', list.files(here('iwtt'), pattern = '.zip'))) 

unlink(here('iwtt', list.files(here('iwtt'), pattern = '.zip')))

# Load --------------------------------------------------------------------

iwtt_con <- dbConnect(
  duckdb(),
  dbdir = 'iwtt_db.duckdb'
  #dbdir = ":memory:"
  )

lof <- list.files(here('iwtt', 'raw'), pattern = '.csv')

map(lof, ~{
  
  df <- data.table::fread(
    input = here('iwtt', 'raw', .x),
    na.strings = c('NA')
    )%>% 
    mutate(
      across(where(is.character), stringi::stri_enc_tonative),
      across(where(is.character), ~na_if(.x, ""))
      ) %>% 
    janitor::clean_names()

  dbWriteTable(
    conn = iwtt_con,
    name = str_to_lower(
      str_remove(.x, pattern = ".csv")),
    value = df,
    overwrite = TRUE
    )
}, .progress = TRUE)

rm(lof)

dbListTables(iwtt_con)

unlink(here('iwtt', 'raw'), recursive = TRUE)

# Transform ---------------------------------------------------------------

tbl(iwtt_con, 'parameter') %>% glimpse()

## parameter ---------------------------------------------------------------

dbWriteTable(
  conn = iwtt_con, 
  name = "",
  value = left_join(
  tbl(iwtt_con, 'parameter') %>% 
    select(),
  tbl(iwtt_con, 'key_parameter_code') %>% 
    select(
      'paramid',
      'pollutant_search_term',
      'cas_nmbr',
      'category',
      'category_2'
    ),
  join_by('paramid')
  )
)

left_join(
  tbl(iwtt_con, 'parameter'),
  tbl(iwtt_con, 'key_parameter_code') %>% 
    select(
      'paramid',
      'pollutant_search_term',
      'cas_nmbr',
      'category',
      'category_2'
    ),
  join_by('paramid')
)

tbl(iwtt_con, '')

## treatment ---------------------------------------------------------------

#top level 
tbl(iwtt_con, 'treatment_system') %>%
  filter(psc_code == '435') %>% 
  glimpse()

#Order of treatment
tbl(iwtt_con, 'treatment_units')

tbl(iwtt_con, 'key_treatment_tech_codes') %>% 
  distinct(tt_code, .keep_all = TRUE) %>% 
  #select(-tt_id, -tt_variation) %>% 
  arrange(tt_category) %>% 
  print(n = Inf)


# reference info ----------------------------------------------------------

tbl(iwtt_con, 'key_naics')
tbl(iwtt_con, 'key_psc_sic_xwalk')
tbl(iwtt_con, 'reference_info') %>% glimpse()

tbl(iwtt_con, 'key_performstat') %>% glimpse()



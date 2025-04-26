# packages ----------------------------------------------------------------

{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(arrow)
  library(duckdb)
  library(duckplyr)
  library(plumber)

  # library(ComptoxR)

  setwd(here("ecotox"))
}


# download ----------------------------------------------------------------

resp <- GET(url = 'https://gaftp.epa.gov/ecotox/') %>%
  content(.) %>%
  html_elements(., 'a') %>%
  html_attr('href') %>%
  .[str_detect(., pattern = 'zip')] %>%
  data.frame(file = .) %>%
  mutate(
    date = str_remove_all(file, pattern = 'ecotox_ascii_'),
    date = str_remove_all(date, pattern = '.zip'),
    date = lubridate::as_date(date, format = "%m_%d_%Y"),
    latest = case_when(
      row_number() == 1 ~ TRUE,
      .default = FALSE
    )
  )

download.file(
  paste0('https://gaftp.epa.gov/ecotox/', resp$file[which.max(resp$date)]),
  destfile = here('ecotox', 'ecotox.zip')
)

unzip('ecotox.zip')

eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)

# main --------------------------------------------------------------------

unlink(list.files(pattern = 'release'))
unlink(list.files(pattern = 'ASCII|Ascii'))

walk(
  list.files(pattern = ".txt")[
    !str_detect(list.files(pattern = ".txt"), pattern = "release")
  ],
  function(x) {
    cli::cli_text(x)

    read_delim(
      file = x,
      delim = "|",
      col_types = cols(.default = col_character()),
      na = c("", "NA", 'NR', "NC", "", "-", "--", 'NONE', 'UKN', 'UKS'),
      locale = locale(encoding = "latin1")
    ) %>%
      janitor::remove_empty(., which = 'cols') %>%
      arrow::write_parquet(
        x = .,
        sink = paste0(str_remove(x, pattern = ".txt"), '.parquet')
      )

    unlink(x)
  },
  .progress = TRUE
)

walk(
  list.files(pattern = ".parquet"),
  function(x) {
    cli::cli_text(x)

    dbWriteTable(
      eco_con,
      str_remove(x, pattern = ".parquet"),
      read_parquet(x)
      #  , overwrite = TRUE
    )

    unlink(x)
  }
)

dbListTables(eco_con)

# validation --------------------------------------------------------------

setwd(here("ecotox", "validation"))

walk(
  list.files(pattern = ".txt"),
  function(x) {
    cli::cli_text(x)

    read_delim(
      file = x,
      delim = "|",
      col_types = cols(.default = col_character()),
      na = c("", "NA", 'NR', "NC", "", "-", "--", 'NONE', 'UKN', 'UKS'),
      locale = locale(encoding = "latin1")
    ) %>%
      janitor::remove_empty(., which = 'cols') %>%
      arrow::write_parquet(
        x = .,
        sink = paste0(str_remove(x, pattern = ".txt"), '.parquet')
      )

    unlink(x)
  }
)

walk(
  list.files(pattern = ".parquet"),
  function(x) {
    cli::cli_text(x)

    dbWriteTable(eco_con, str_remove(x, pattern = ".parquet"), read_parquet(x))

    unlink(x)
  }
)


# terms appendix ----------------------------------------------------------

GET(url = 'https://gaftp.epa.gov/ecotox/') %>%
  content(.) %>%
  html_elements(., 'a') %>%
  html_attr('href') %>%
  .[str_detect(., pattern = 'xlsx')] %>%
  paste0('https://gaftp.epa.gov/ecotox/', .) %>%
  download.file(
    url = .,
    destfile = 'ecotox_terms_appendix.xlsx',
    mode = 'wb'
  )

ecotox_appendix <- import_list(file = 'ecotox_terms_appendix.xlsx', skip = 2)

eco_toc <- ecotox_appendix %>%
  pluck(., 1) %>%
  pull(., Title) %>%
  janitor::make_clean_names() %>%
  paste0('app_', .)

ecotox_appendix <- ecotox_appendix %>%
  discard_at(., 1) %>%
  set_names(., eco_toc) %>%
  map(., janitor::clean_names)

iwalk(
  ecotox_appendix,
  function(x, y) {
    cli::cli_text(y)

    dbWriteTable(eco_con, y, x)
  }
)

unlink('ecotox_terms_appendix.xlsx')

# clean up ----------------------------------------------------------------

if (length(list.files(pattern = '.parquet')) > 0) {
  file.remove(list.files(pattern = '.parquet'))
}
  
dbWriteTable(eco_con, 'versions', resp)

setwd(here("ecotox"))

file.remove(here('ecotox', 'ecotox.zip'))

unlink(here('ecotox', 'validation'), recursive = TRUE)

rm(resp, ecotox_appendix, eco_toc, terms)

dbListTables(eco_con)

dbDisconnect(eco_con)

# deploy ------------------------------------------------------------------

#source(here("ecotox", "plumber.R"))

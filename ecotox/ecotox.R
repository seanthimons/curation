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
  # library(duckplyr)

  # library(ComptoxR)

  setwd(here("ecotox"))
}


# download ----------------------------------------------------------------

url <- 'https://gaftp.epa.gov/ecotox/'

resp <- GET(url = url) %>%
  content(.) %>% 
  html_elements(., 'a') %>% 
  html_attr('href') %>% 
  .[str_detect(., pattern = 'zip')]
  
tbl <- data.frame(file = resp) %>% 
  mutate(date = str_remove_all(file, pattern = 'ecotox_ascii_'), 
         date = str_remove_all(date, pattern = '.zip'),
         date = lubridate::as_date(date, format = "%m_%d_%Y"))

download.file(paste0(url, tbl$file[which.max(tbl$date)]), destfile = here('ecotox', 'ecotox.zip'))

unzip('ecotox.zip')

rm(tbl, resp, url)

con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)

# main --------------------------------------------------------------------

lof <- list.files(pattern = ".txt") %>%
  .[str_detect(., pattern = "release", negate = T)]

f_names <- str_remove_all(lof, pattern = ".txt")

map2(lof, f_names, function(x, y) {
  cat(y, "\n")
  
  dbWriteTable(con, y, read_delim(
    file = here("ecotox", x),
    delim = "|",
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "latin1"),
    progress = T
      ),
    overwrite = TRUE)
  
}, .progress = T)

map(lof, file.remove)

# validation --------------------------------------------------------------

setwd(here("ecotox", "validation"))

lof <- list.files(pattern = ".txt")

f_names <- str_remove_all(lof, pattern = ".txt")

map2(lof, f_names, function(x, y) {
  cat(y, "\n")
  
  dbWriteTable(con, y, read_delim(
    file = here("ecotox", "validation", x),
    delim = "|",
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "latin1"),
    progress = T
  ),
  overwrite = TRUE)
  
}, .progress = T)

map(lof, file.remove)

setwd(here("ecotox"))

unlink(here("ecotox", "validation"), recursive = T)

rm(f_names, lof)

file.remove(here('ecotox', 'ecotox.zip'))

DBI::dbDisconnect(con, shutdown = TRUE)
rm(con)
source(here("ecotox", "plumber.R"))

lapply(dbListConnections(drv = dbDriver("duckdb")), function(x) {dbDisconnect(conn = x)})

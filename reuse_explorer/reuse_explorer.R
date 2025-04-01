{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(jsonlite)
  library(fuzzyjoin)
  library(stringdist)

  setwd(here("reuse_explorer"))
}


# download ----------------------------------------------------------------

lf <- jsonlite::read_json("https://www.epa.gov/system/files/other-files/2021-10/documents.json") %>%
  map(., ~ {
    .x <- c(.x, dat = NA, head_url = NA)
    .x$State <- str_squish(.x$State)
    return(.x)
  }, .progress = TRUE)


# raw ---------------------------------------------------------------------


raw <- lf %>%
  map(., ~ {
    cli::cli_text(.x$State, ": ", .x$`Source of Water`, "; ", .x$`Reuse Application`, "\n")
    .x$head_url <-
      HEAD(.x$Url) %>%
      .$url %>%
      bow(.) %>%
      scrape(content = "text/html; charset=UTF-8") %>%
      html_element(., xpath = '//*[@id="table1"]')

    if (length(.x$head_url) != 0L) {
      .x$dat <- html_table(.x$head_url) %>%
        select(1:5) %>%
        set_names(c(
          "class",
          "source",
          "param",
          "spec",
          "req"
        ))

      .x$head_url <- NULL
      .x$Url <- NULL

      .x <- compact(.x)

      .x <- as_tibble(.x) %>%
        unnest(., cols = dat)
    } else {
      .x <- NULL
    }

    return(.x)
  }, .progress = TRUE) %>%
  compact() %>%
  list_rbind()

# TODO Explore fuzzy matches for params from TADA and SSWQS

compounds <- raw %>%
  count(param) %>%
  arrange(desc(n)) %>%
  filter(!is.na(param)) %>%
  mutate(
    idx_n = 1:n(),
    #orig_param = param,
    param = str_replace_all(param, pattern = '\\u00a0', replacement = " ") %>% str_squish(),
    #uni = stringi::stri_escape_unicode(param),
    #len = str_length(uni)
    ) %>% 
  select(-n) %>% 
  #arrange(param) %>% 
  #mutate(idx_alp = 1:n()) %>% #%>% mutate(across(where(is.numeric), as.character))
  distinct(param, .keep_all = TRUE)
  
# hashing -----------------------------------------------------------------

nwqs <- rio::import(here("final", "nwqs.RDS")) %>%
  distinct(preferredName, dtxsid)

#TEMP Need to import hashing functions here

 save.image('reuse.RData')

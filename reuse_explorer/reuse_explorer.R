{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(jsonlite)
  
  setwd(here("reuse_explorer"))
}


# download ----------------------------------------------------------------

lf <- jsonlite::read_json('https://www.epa.gov/system/files/other-files/2021-10/documents.json') %>%
  map(., ~{
    .x <- c(.x, dat = NA, head_url = NA)
    .x$State <- str_squish(.x$State)
    return(.x)
  },.progress = TRUE)


# raw ---------------------------------------------------------------------


raw <- lf %>% 
  map(., ~{
    #cat(.x$State, .x$`Source of Water`, .x$`Reuse Application`, '\n')
    .x$head_url <- 
      HEAD(.x$Url) %>%
      .$url %>%
      bow(.) %>%
      scrape(content="text/html; charset=UTF-8") %>%
      html_element(., xpath = '//*[@id="table1"]')
    
    if(length(.x$head_url) != 0L){
      
      .x$dat <- html_table(.x$head_url) %>% 
        select(1:5) %>% 
        set_names(c(
          'class',
                     'source',
                     'param',
                     'spec',
                     'req'))
         
      .x$head_url <- NULL
      .x$Url <- NULL
      
      .x <- compact(.x)
      
      .x <- as_tibble(.x) %>%
        unnest(., cols = dat)
      
    }else{
      .x <- NULL
    }
    
    return(.x)
  }, .progress = TRUE) %>% 
  compact() %>% list_rbind()

# TODO Explore fuzzy matches for params from TADA and SSWQS

raw %>% 
  count(param) %>% arrange(desc(n))




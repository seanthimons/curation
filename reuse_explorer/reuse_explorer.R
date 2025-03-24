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
  arrange(param) %>% 
  mutate(idx_alp = 1:n()) %>% #%>% mutate(across(where(is.numeric), as.character))
  distinct(param, .keep_all = TRUE)
  
# hashing -----------------------------------------------------------------

nwqs <- rio::import(here("final", "nwqs.RDS")) %>%
  distinct(preferredName, dtxsid)


hash_init <-
  map(
    c(
      "osa",
      "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"
      ),
    ~ {
      stringdist_join(compounds, compounds,
        by = c("param" = "param"),
        # mode = 'right',
        method = .x,
        # max_dist = 9,,
        ignore_case = TRUE,
        distance_col = "dist"
      ) # %>%
      # group_by(preferredName) %>%
      # filter(!is.na(dist)) %>%
      # slice_min(order_by = dist, n = 5)
    },
    .progress = T
  ) %>% 
  set_names(., c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"))

{
  hash_1 <-
    keep(hash_init, names(hash_init) %in% c("osa", "lv", "dl")) %>%
    list_rbind(names_to = "method") %>% 
    mutate(perfect = case_when(
      idx_n.x == idx_n.y ~ TRUE,
      .default = FALSE
    )) %>% 
    filter(perfect == FALSE, dist < 2, idx_n.x < idx_n.y) %>%
    select(-perfect)
  
  #  
  hash_2 <-
    keep(hash_init, names(hash_init) %in% c("lcs")) %>%
    list_rbind(names_to = "method") %>% 
    mutate(perfect = case_when(
      idx_n.x == idx_n.y ~ TRUE,
      .default = FALSE
    )) %>% 
    filter(perfect == FALSE, idx_n.x < idx_n.y) %>%
    select(-perfect)
    

  hash_3 <-
    keep(hash_init, names(hash_init) %in% c("cosine", "jw")) %>%
    list_rbind(names_to = "method") %>% 
    mutate(perfect = case_when(
      idx_n.x == idx_n.y ~ TRUE,
      .default = FALSE
    )) %>% 
    filter(
      perfect == FALSE,
      method == 'cosine' & dist <= 0.1 | method == 'jw' & dist <= 0.15
      ) %>%
    select(-perfect)
  
  hash_4 <-
    keep(hash_init, names(hash_init) %in% c("jaccard")) %>%
    list_rbind(names_to = "method") %>% 
    mutate(perfect = case_when(
      idx_n.x == idx_n.y ~ TRUE,
      .default = FALSE
    )) %>% 
    filter(
          perfect == FALSE,
          dist <= 0.19
    ) %>% select(-perfect)
  
  
  hash_5 <-
    keep(hash_init, names(hash_init) %in% c("hamming")) %>%
    list_rbind(names_to = "method") %>% 
    mutate(perfect = case_when(
      idx_n.x == idx_n.y ~ TRUE,
      .default = FALSE
    )) %>% 
    filter(
      perfect == FALSE, dist == 0, idx_n.x < idx_n.y)
  
  
}
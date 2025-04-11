
# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(tidyverse)
  library(httr2)
  #library(rvest)
  library(ComptoxR)
  #library(jsonlite)
  library(arrow)
  
  setwd(here('epa'))
  #load('epa.Rdata')
}

# 9.6
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=62e184ebe4b055edffbfc22b&page=0

# 9.5
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=66853bf7e4b0a7c65d177bc3&page=0

# 9.4 
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=645a5c33e4b08a6b39438b19&page=0

# 9.3/2
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=631759b4e4b04f6bb132461a&page=0


# Clowder files -----------------------------------------------------------

clowder_list <- request('https://clowder.edap-cluster.com/api/datasets/61147fefe4b0856fdc65639b/listAllFiles') %>%
  req_perform() %>% 
  resp_body_json()

tv_list <- clowder_list %>%
  map(~ .x[c("id", "filename")]) %>%
  keep(~ str_detect(.x$filename, "toxval_v9")) %>% 
  discard( ~str_detect(.x$filename, '.sql|README|qc_status|gz|with')) #%>% 
  # map(., ~pluck(., 'filename')) %>% 
  # unlist()

tv_ver <- tv_list %>% 
  map(., ~pluck(., 'filename')) %>% 
  unlist() %>% 
  str_replace_all(., 'toxval_all_res_', "") %>% 
  str_subset(., pattern = 'toxval_v\\d{2}_\\d?') %>% 
  str_extract(., pattern = 'v\\d{2}_\\d?') %>% 
  unique()

tv_grp <- tv_ver %>% 
  map(., function(ver){
    keep(tv_list, ~str_detect(.x$filename, pattern = ver))
  }) %>% set_names(tv_ver)

rm(tv_list, clowder_list)  


# raw ---------------------------------------------------------------------

map(
  names(tv_grp),
  ~dir.create(here('epa', 'toxval_raw', .x)))

# temp <- tv_grp %>% 
#   keep_at(., 'v92')

tv_grp %>% 
#temp %>% 
  imap(., ~{
    setwd(here('epa', 'toxval_raw', .y))
    map(., ~{
      cli::cli_alert(.x$filename)
        download.file(
          url = paste0('https://clowder.edap-cluster.com/api/files/', .x$id,'/blob'),
          destfile = .x$filename,
          mode = 'wb')
      })
    #list.files()
  })

#lof <- list.files(here('epa', 'toxval_raw'), recursive = TRUE)

tv_ver %>%
  .[1:4] %>% 
  imap(., ~{
    cli::cli_alert(.x)
    
    setwd(here('epa', 'toxval_raw', .x))
    
    raw <- list.files(getwd()) %>%
      map(., ~{
        readxl::read_excel(
          .x,
          col_types = c(
            "text"
          ),
          na = c("-", ""))
      }, .progress = TRUE) %>%
      list_rbind()

    write_parquet(raw, sink = here('final', paste0('toxval_',.x,'.parquet')))
  })


# packages ----------------------------------------------------------------
{
  library(here)
  library(tidyverse)
  library(httr2)
  library(arrow)
  library(rvest)
 
  setwd(here('norman')) 
  todor::todor_file()
}

#https://www.norman-network.com/nds/ecotox/docs/Fact-sheet-EQS-Derivation.pdf

# request -----------------------------------------------------------------

# ~94,000 compounds
norman_qual <- seq(from = 0, by = 10000, length.out = 10)
      
norman_qual <- 
  map(norman_qual, ~{
    nreq <- request("https://www.norman-network.com/nds/ecotox/qualityTargetIndexAjax.php") |>
      req_url_query(
        start = as.character(.x),
        length = '10000'
      ) %>% 
      req_headers(
        Accept = "application/json"
      ) %>% 
      req_perform()
    
    resp <- nreq %>%
      resp_body_string() %>% 
      jsonlite::fromJSON() %>% 
      pluck(., 'data') %>% 
      as_tibble() #%>% mutate(idx = row_number()-1)
    
  }, .progress = TRUE)

norman_q <- norman_qual %>% 
  list_rbind() %>% 
  `colnames<-`(c('nsid', 'preferredName', 'i', 'casrn')) %>% 
  select(-i) %>% 
  mutate(nsid = str_extract(nsid, pattern = "NS\\d+"),
         susid = str_extract(nsid, "\\d+") %>% as.numeric(),
         casrn = str_remove_all(casrn, pattern = 'CAS_RN|CAS RN|:') %>% str_squish(),
         casrn = na_if(casrn, "")
         )

write_rds(norman_q, file = 'norman_compounds.RDS')

rm(resp, nreq, norman_qual)

# PNECs -------------------------------------------------------------------

norman_resp <- norman_eco <- norman_q %>%
  pull(nsid) %>% 
  as.list() #%>% sample(20)

norman_resp <- norman_eco %>%
  map(., ~{
    
    request('https://www.norman-network.com/nds/api/ecotox/nsid/') %>% 
      req_url_path_append(.x) %>% 
      req_url_path_append('/JSON')
    
  }) %>% 
  req_perform_sequential(., on_error = 'continue')

norman_resp <- norman_resp %>% 
  map(., ~{
    
    .x <- resp_body_json(.x) %>% as_tibble()
    
  }, .progress = TRUE) %>% 
  set_names(names(norman_eco)) %>% 
  list_rbind()

norman_pnec <- norman_resp %>% 
  mutate(across(contains('PNEC'), ~as.numeric(.x)))

write_parquet(norman_pnec, sink = here('final', 'norman_pnec.parquet'))

# ecotox ------------------------------------------------------------------

norman_resp <- norman_eco <- norman_q %>%
  pull(susid) %>% 
  as.list() %>% 
  set_names(norman_q$nsid) %>% sample(10) %>% append(list('NS00002649' = 2649))

norman_resp <- norman_eco %>%
  map(., ~{
    
    request('https://www.norman-network.com/nds/ecotox/qualityTargetShow.php?') %>% 
      req_url_query(susID = .x)
    
  }) %>% 
  req_perform_sequential()

norman_resp <- norman_resp %>% 
  map(., ~{
    
    .x <- resp_body_html(.x) %>%
      html_table()
    
  }) %>% 
  set_names(names(norman_eco))


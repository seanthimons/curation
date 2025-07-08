# packages ----------------------------------------------------------------
{
  library(here)
  library(tidyverse)
  library(httr2)
  library(arrow)
  library(rvest)

  setwd(here('norman'))
  load('norman.RData')
}

#https://www.norman-network.com/nds/ecotox/docs/Fact-sheet-EQS-Derivation.pdf

# request -----------------------------------------------------------------

# ~94,000 compounds
norman_qual <- seq(from = 0, by = 10000, length.out = 10)

norman_qual <-
  map(
    norman_qual,
    ~ {
      nreq <- request(
        "https://www.norman-network.com/nds/ecotox/qualityTargetIndexAjax.php"
      ) |>
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
    },
    .progress = TRUE
  )

norman_q <- norman_qual %>%
  list_rbind() %>%
  `colnames<-`(c('nsid', 'preferredName', 'i', 'casrn')) %>%
  select(-i) %>%
  mutate(
    nsid = str_extract(nsid, pattern = "NS\\d+"),
    susid = str_extract(nsid, "\\d+") %>% as.numeric(),
    casrn = str_remove_all(casrn, pattern = 'CAS_RN|CAS RN|:') %>% str_squish(),
    casrn = na_if(casrn, "")
  )

write_tsv(norman_q, 'norman_compounds.tsv')

rm(resp, nreq, norman_qual)

# PNECs -------------------------------------------------------------------

norman_resp <- norman_eco <- norman_q %>%
  pull(nsid) %>%
  #str_subset(., pattern = 'NS00009623|NS00075864') %>%
  as.list() #%>% sample(20)

norman_resp <- norman_eco %>%
  map(
    .,
    ~ {
      request('https://www.norman-network.com/nds/api/ecotox/nsid/') %>%
        req_url_path_append(.x) %>%
        req_url_path_append('/JSON')
    }
  ) %>%
  req_perform_sequential(., on_error = 'continue')

norman_resp <- norman_resp %>%
  map(
    .,
    ~ {
      .x <- resp_body_json(.x) %>% as_tibble()
    },
    .progress = TRUE
  ) %>%
  set_names(names(norman_eco)) %>%
  list_rbind()

norman_pnec <- norman_resp %>%
  mutate(across(contains('PNEC'), ~ as.numeric(.x)))

write_parquet(norman_pnec, sink = here('final', 'norman_pnec.parquet'))

# ecotox ------------------------------------------------------------------

#NOTE Not needed; looks like ECOTOX has NORMAN data...

# norman_resp <- norman_eco <- norman_q %>%
#   pull(susid) %>%
#   as.list() %>%
#   set_names(norman_q$nsid) %>%
#   sample(10) %>%
#   append(
#     list(
#       'NS00002649' = 2649,
#       'NS00009623' = 9623,
#       'NS00075864' = 75864
#     ))
#
# norman_resp <- norman_eco %>%
#   map(., ~{
#
#     request('https://www.norman-network.com/nds/ecotox/qualityTargetShow.php?') %>%
#       req_url_query(susID = .x)
#
#   }) %>%
#   req_perform_sequential()
#
# norman_resp <- norman_resp %>%
#   map(., ~{
#
#     .x <- resp_body_html(.x) %>%
#       #doesn't seem to be any data aside from the first two entries...
#       html_table() %>%
#       keep_at(., 1:2)
#
#   }) %>%
#   set_names(names(norman_eco))
#
# norman_summary <- norman_resp %>%
#   map(., ~{
#     pluck(., 2) %>%
#       mutate(
#         across(everything(), as.character),
#         across(everything(), ~na_if(.x, "")),
#         across(everything(), ~na_if(.x, "n.a.")),
#         across(everything(), ~na_if(.x, "n.r."))
#       )
#   }) %>%
#   list_rbind(names_to = 'susid') %>%
#   janitor::clean_names() %>%
#   select(-editor, -pnec_id)
#
# norman_feed <- norman_resp %>%
#   map(., ~{
#     pluck(., 1) %>%
#       #removes weird unnname column
#       select(-c(10:13)) %>%
#       mutate(
#         across(everything(), as.character),
#         across(everything(), ~na_if(.x, "")),
#         across(everything(), ~na_if(.x, "n.a.")),
#         across(everything(), ~na_if(.x, "n.r."))
#         )
#       }) %>%
#   list_rbind(names_to = 'susid') %>%
#   janitor::clean_names() %>%
#   filter(str_detect(pnec_id, pattern = "Showing", negate = TRUE)) %>%
#   mutate(pne_cvalue = as.numeric(pne_cvalue))

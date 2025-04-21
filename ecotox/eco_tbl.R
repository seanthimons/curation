query <- ct_list('NEUROTOXINS') %>% 
  pluck(., 1, 'dtxsids') %>% 
  ct_details(query = .)

query_cas <- query %>% 
  pull(casrn) %>% 
  str_remove_all(., "-")

job::job({
  
  library(rgbif)
  
  sp <- list(ComptoxR::std_spec, ComptoxR::threat_spec) |> purrr::list_rbind(x = _)
  
  sp_dat <- sp$latin %>% 
    map(., ~{
    name_backbone(name = .x) %>% 
      as_tibble()
    }, .progress = TRUE) %>% 
    set_names(sp$latin) %>% 
    list_rbind(names_to = 'raw_search') %>% 
    filter(!is.na(usageKey)) %>% 
    inner_join(sp, ., join_by(latin == raw_search))
  
  hab_dat_raw <- map(sp_dat$usageKey, ~name_usage(key = .x, data = 'speciesProfiles'), .progress = TRUE) %>% 
    set_names(sp_dat$usageKey)
  
  hab_dat <- hab_dat_raw %>% 
    map(., ~{
      pluck(., 'data')
    }) %>% 
    compact() %>% 
    list_rbind(names_to = 'usageKey') %>% 
    select(
      usageKey, 
      marine, 
      freshwater
      #choosing to not use this as I don't need it? 
      #, terrestrial 
      #choosing to not use this since it is too freeform
      #,habitat
    ) %>% 
    pivot_longer(., cols = c(marine, freshwater), values_to = 'hab1', values_drop_na = TRUE) %>% 
    distinct() %>% 
    arrange(hab1) %>%
    distinct(usageKey, name, .keep_all = TRUE) %>% 
    pivot_wider(names_from = name, values_from = hab1, values_fill = FALSE) %>% 
    mutate(usageKey = as.integer(usageKey)) %>% 
    inner_join(
      sp_dat %>% 
        select(
          latin,
          common, 
          usageKey,
          canonicalName
          ),
      .,
      join_by(usageKey)) %>% 
    distinct() %>% 
    mutate(hab_chk = case_when(
      freshwater == TRUE & marine == TRUE ~ TRUE,
      .default = FALSE
    ))
  
  
  
  # library(httr2)
  # 
  # request("https://api.checklistbank.org/dataset/309120/nameusage/search") |>
  #   req_url_ery(
  #     content = "SCIENTIFIC_NAME",
  #     facet = "rank",
  #     facet = "issue",
  #     facet = "status",
  #     facet = "nomStatus",
  #     facet = "nomCode",
  #     facet = "nameType",
  #     facet = "field",
  #     facet = "authorship",
  #     facet = "authorshipYear",
  #     facet = "extinct",
  #     facet = "environment",
  #     facet = "origin",
  #     facet = "sectorMode",
  #     facet = "secondarySourceGroup",
  #     facet = "sectorDatasetKey",
  #     facet = "secondarySource",
  #     facet = "group",
  #     limit = "50",
  #     offset = "0",
  #     q = "Danio rerio",
  #     sortBy = "taxonomic",
  #     type = "EXACT"
  #   ) |>
  #   req_headers(
  #     `sec-ch-ua-platform` = '"Windows"',
  #     Referer = "https://www.checklistbank.org/",
  #     `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36",
  #     Accept = "application/json, text/plain, */*",
  #     `sec-ch-ua` = '"Google Chrome";v="135", "Not-A.Brand";v="8", "Chromium";v="135"',
  #     `sec-ch-ua-mobile` = "?0"
  #   ) |>
  #   req_perform()
  # 
  
  
  
  
})

#
eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)


#need to filter for exposure types + groups to satisfy the dictionary from PPDB


eco_risk_tbl <- tbl(eco_con, "tests") %>%
  filter(test_cas %in% query_cas) %>%
  inner_join(
    tbl(eco_con, "species")
    ,join_by('species_number')
  ) %>% 
  inner_join(
    tbl(eco_con, 'results'),
    join_by('test_id')
  ) %>%
  filter(
    endpoint %in% c(
      'EC50',
      'LC50',
      'LD50',
      'LOEC',
      'LOEL',
      'NOEC',
      'NOEL'
    ),
    effect %in% c('MOR'),
    conc1_unit %in% c('ug/L', 'mg/L', 'ppm', 'ppb'),
    obs_duration_unit %in% c('h', 'd', 'wk'),
    
  ) %>% 
  collect() %>% 
  mutate(
    result = as.numeric(conc1_mean),
    endpoint_group = case_when(
      str_detect(endpoint, 'LOEC|LOEL') ~ 'LOEC | LOEL',
      str_detect(endpoint, 'EC50|LD50|LC50') ~ 'EC50 | LD50 | LC50',
      str_detect(endpoint, 'NOEL|NOEC') ~ 'NOEL | NOEC'
    ),
    eco_group = case_when(
      str_detect(ecotox_group,'Insects/Spiders') ~ 'Insects/Spiders',
      str_detect(ecotox_group,'Flowers, Trees, Shrubs, Ferns') ~ 'Flowers, Trees, Shrubs, Ferns',
      str_detect(ecotox_group,'Fungi') ~ 'Fungi',
      str_detect(ecotox_group,'Algae') ~ 'Algae',
      str_detect(ecotox_group,'Fish') ~ 'Fish',
      str_detect(ecotox_group,'Crustaceans') ~ 'Crustaceans',
      str_detect(ecotox_group,'Invertebrates') ~ 'Invertebrates',
      str_detect(ecotox_group,'Worms') ~ 'Worms',
      str_detect(ecotox_group,'Molluscs') ~ 'Molluscs',
      str_detect(ecotox_group,'Birds') ~ 'Birds',
      str_detect(ecotox_group,'Mammals') ~ 'Mammals',
      str_detect(ecotox_group,'Amphibians') ~ 'Amphibians',
      str_detect(ecotox_group,'Reptiles') ~ 'Reptiles',
      str_detect(ecotox_group,'Moss, Hornworts') ~ 'Moss, Hornworts',
      #str_detect('') ~ '',
      .default = ecotox_group
    ),
    duration_value = as.numeric(obs_duration_mean),
    duration_unit = case_when(
      obs_duration_unit == 'h' ~ 'hours',
      obs_duration_unit == 'd' ~ 'days',
      obs_duration_unit == 'wk' ~ 'weeks'
    )
  ) %>% 
  convert_units(., value_column = 'result', unit_column = 'conc1_unit') %>% 
  convert_duration(., value_column = 'duration_value', unit_column = 'duration_unit')

eco_summary <- eco_risk_tbl %>% 
  group_by(
    test_cas,
    endpoint,
    eco_group,
    common_name, 
    latin_name,
    new_dur, 
    new_dur_unit
  ) %>% 
  distinct(common_name, latin_name)

  
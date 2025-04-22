query <- ct_list('NEUROTOXINS') %>% 
  pluck(., 1, 'dtxsids') %>% 
  ct_details(query = .)

query_cas <- query %>% 
  pull(casrn) %>% 
  str_remove_all(., "-")

# -------------------------------------------------------------------------


eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)


#need to filter for exposure types + groups to satisfy the dictionary from PPDB

eco_risk_tbl <- tbl(eco_con, "tests") %>%
  select(
    'test_id',
    'test_cas',
    'species_number',
    'exposure_type',
    'test_type'
  ) %>% 
  filter(
    test_cas %in% query_cas
    ) %>%
  inner_join(
    tbl(eco_con, "species") %>% 
      select(
        'species_number',
        'common_name',
        'latin_name',
        'ecotox_group'
      )
    ,join_by('species_number')
  ) %>% 
  inner_join(
    tbl(eco_con, 'results') %>% 
      select(
        'result_id', 
        'test_id', 
        'obs_duration_mean',
        'obs_duration_unit',
        'endpoint',
        'effect',
        'conc1_mean',
        'conc1_unit'
      ),
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
    conc1_unit %in% c('ug/L', 'mg/L', 'ppm', 'ppb', 'mg/kg', 'mg/kg/d'),
    obs_duration_unit %in% c('h', 'd', 'wk')
  ) %>% 
  inner_join(
    tbl(eco_con, 'app_exposure_types') %>% 
      select(
        'exposure_group', 
        'term') %>% 
      filter(exposure_group %in% c(
        'AQUA',
        'ENV',
        'ORAL',
        'TOP'
      )),
    join_by('exposure_type' == 'term')
  ) %>% 
  collect() %>% 
  select(
    -test_id, 
    -species_number,
    -exposure_type,
    -result_id
  ) %>% 
  filter(
    !is.na(conc1_mean),
    #not sure what the plus means...
    str_detect(conc1_mean, pattern = '\\+', negate = TRUE),
    !is.na(obs_duration_unit) & !is.na(obs_duration_mean)) %>%
  mutate(
    result = as.numeric(str_remove_all(conc1_mean, pattern = "\\*")),
    endpoint_group = case_when(
      str_detect(endpoint, 'EC05|LD05|LC05') ~ 'EC05 | LD05 | LC05',
      str_detect(endpoint, 'LOEC|LOEL') ~ 'LOEC | LOEL',
      str_detect(endpoint, 'EC25|LC25|LD25') ~ 'EC25 | LD25 | LC25',
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
      .default = ecotox_group
    ),
    duration_value = as.numeric(obs_duration_mean),
    duration_unit = case_when(
      obs_duration_unit == 'h' ~ 'hours',
      obs_duration_unit == 'd' ~ 'days',
      obs_duration_unit == 'wk' ~ 'weeks'
    )
  ) %>% 
  convert_duration(., 
                   value_column = 'duration_value',
                   unit_column = 'duration_unit'
                   ) 
  
  mutate(test_type = case_when(

# herts -------------------------------------------------------------------

    # eco_group == 'Mammals' & new_dur ==  ~ '',
    # 
    # eco_group == 'Worms' & new_dur == 336 & endpoint == 'LC50' ~ 'acute',
    # eco_group == 'Worms' & new_dur == 336 & endpoint == 'NOEC' ~ 'chronic',
    # 
    # eco_group == 'Birds' & new_dur <= 336  ~ 'acute',
    # eco_group == 'Birds' & new_dur > 336 ~ 'chronic',
    # 
    # eco_group == 'Algae' & new_dur == 72 ~ "acute",
    

# best guess --------------------------------------------------------------

    eco_group == 'Algae' & new_dur <= 96 ~ 'acute',
    eco_group == 'Algae' & new_dur > 144 ~ 'chronic',
    
    eco_group == 'Amphibians' & new_dur <=  96 ~ 'acute',
    eco_group == 'Amphibians' & new_dur > 96 ~ 'chronic',
    
    eco_group == 'Birds' & new_dur <=  192 ~ 'acute',
    eco_group == 'Birds' & new_dur >  192 ~ 'chronic',
    
    eco_group == 'Crustaceans' & new_dur <= 48 ~ 'acute',
    eco_group == 'Crustaceans' & new_dur >= 144 ~ 'chronic',
    
    eco_group == 'Fish' & new_dur <= 96 ~ 'acute',
    eco_group == 'Fish' & new_dur >= 144 ~ 'chronic',
    
    eco_group == 'Flowers, Trees, Shrubs, Ferns' & new_dur <=  ~ 'acute',
    eco_group == 'Flowers, Trees, Shrubs, Ferns' & new_dur > ~ 'chronic',
    
    eco_group == 'Insects/Spiders' & new_dur <=  ~ '',
    eco_group == 'Insects/Spiders' & new_dur >  ~ '',
    
    eco_group == 'Invertebrates' & new_dur <=  ~ '',
    eco_group == 'Invertebrates' & new_dur >  ~ '',
    
    eco_group == 'Mammals' & new_dur <=  ~ '',
    eco_group == 'Mammals' & new_dur >  ~ '',
    
    eco_group == 'Molluscs' & new_dur <=  ~ '',
    eco_group == 'Molluscs' & new_dur >  ~ '',
    
    eco_group == 'Reptiles' & new_dur <=  ~ '',
    eco_group == 'Reptiles' & new_dur >  ~ '',
    
    eco_group == 'Worms' & new_dur <=  ~ '',
    eco_group == 'Worms' & new_dur >  ~ '',
    
  ))


  convert_units(., value_column = 'result', unit_column = 'conc1_unit') %>% 

    
  #https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/technical-overview-ecological-risk-assessment-0    

# habitat -----------------------------------------------------------------

eco_species <- eco_risk_tbl %>% 
  distinct(common_name, latin_name)


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
  
})


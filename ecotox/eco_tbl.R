query <- ct_list('NEUROTOXINS') %>% 
  pluck(., 1, 'dtxsids') %>% 
  ct_details(query = .)

query <- ct_list('PESTHHBS') %>% 
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
    'test_type',
    'organism_lifestage'
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
  left_join(
    tbl(eco_con, 'app_exposure_types') %>% 
      select(
        'exposure_group', 
        'term') %>% 
      filter(exposure_group %in% c(
        'AQUA',
        'ENV',
        'ORAL',
        'TOP',
        'Unspecified',
        'UNK'
        
      )),
    join_by('exposure_type' == 'term')
  ) %>% 
  left_join(
    tbl(eco_con, 'lifestage_codes') %>% rename(org_lifestage = description),
    join_by(organism_lifestage == code)
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
    ),
    life_stage = case_when(
      str_detect(tolower(org_lifestage), tolower('Unspecified')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Adult')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Alevin')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Bud or Budding')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Blastula')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Bud blast stage')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Boot')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Cocoon')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Corm')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Copepodid')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Copepodite')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Cleavage stage')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Cyst')) ~ 'Dormant/Senescent',
      str_detect(tolower(org_lifestage), tolower('Egg')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Elver')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Embryo')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Exponential growth phase (log)')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Eyed egg or stage, eyed embryo')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('F0 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F1 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F11 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F2 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F3 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F6 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('F7 generation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Mature (full-bloom stage) organism')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Female gametophyte')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Fingerling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Flower opening')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Froglet')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Fry')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Gastrula')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Gestation')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Glochidia')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Gamete')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Lag growth phase')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Grain or seed formation stage')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Germinated seed')) ~ 'Dormant/Senescent',
      str_detect(tolower(org_lifestage), tolower('Heading')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Incipient bud')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Internode elongation')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Imago')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Immature')) ~ 'Subadult/Immature',
      str_detect(tolower(org_lifestage), tolower('Instar')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Intermolt')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Jointing')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Juvenile')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Lactational')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Egg laying')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Larva-pupa')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Prolarva')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Larva')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Mature')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Mature dormant')) ~ 'Dormant/Senescent',
      str_detect(tolower(org_lifestage), tolower('Megalopa')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Male gametophyte')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Morula')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Mid-neurula')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Molt')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Multiple')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Mysis')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Newborn')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Naiad')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Neonate')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('New, newly or recent hatch')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Neurala')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Not intact')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Not reported')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Nauplii')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Nymph')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Oocyte, ova')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Parr')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Mature, post-bloom stage (fruit trees)')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Pre-hatch')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Pre-molt')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Post-emergence')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Post-spawning')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Mature, pit-hardening stage (fruit trees)')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Post-hatch')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Post-molt')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Pre-, sub-, semi-, near adult, or peripubertal')) ~ 'Subadult/Immature',
      str_detect(tolower(org_lifestage), tolower('Post-smolt')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Pullet')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Post-nauplius')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Pollen, pollen grain')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Postpartum')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Prepupal')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Pre-larva')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Prebloom')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Pre-smolt')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Protolarvae')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Pupa')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Post-larva')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Pre-spawning')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Post-embryo')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Protozoea')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Rooted cuttings')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Rhizome')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Mature reproductive')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Rootstock')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Subadult')) ~ 'Subadult/Immature',
      str_detect(tolower(org_lifestage), tolower('Shoot')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Yolk sac larvae, sac larvae')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Senescence')) ~ 'Dormant/Senescent',
      str_detect(tolower(org_lifestage), tolower('Seed')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Scape elongation')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Sac fry, yolk sac fry')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Mature, side-green stage (fruit trees)')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Sexually immature')) ~ 'Subadult/Immature',
      str_detect(tolower(org_lifestage), tolower('Seedling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Sexually mature')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Smolt')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Sapling')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Sporeling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Sperm')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Spore')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Spat')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Swim-up')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Spawning')) ~ 'Reproductive',
      str_detect(tolower(org_lifestage), tolower('Stationary growth phase')) ~ 'Dormant/Senescent',
      str_detect(tolower(org_lifestage), tolower('Tadpole')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Tissue culture callus')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Tiller stage')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Tuber')) ~ 'Adult',
      str_detect(tolower(org_lifestage), tolower('Trophozoite')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Underyearling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Veliger')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Mature vegetative')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Virgin')) ~ 'Other/Unknown',
      str_detect(tolower(org_lifestage), tolower('Weanling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Young adult')) ~ 'Subadult/Immature',
      str_detect(tolower(org_lifestage), tolower('Yearling')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Young')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Young of year')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Zoea')) ~ 'Larva/Juvenile',
      str_detect(tolower(org_lifestage), tolower('Zygospore')) ~ 'Egg/Embryo',
      str_detect(tolower(org_lifestage), tolower('Zygote')) ~ 'Egg/Embryo',
      TRUE ~ 'Other/Unknown'  # Default
    ),
    life_stage = factor(
      life_stage, levels = c(
        'Egg/Embryo',
        'Larva/Juvenile',
        'Subadult/Immature',
        'Adult',
        'Reproductive',
        'Dormant/Senescent',
        'Other/Unknown'))
  ) %>% 
  convert_duration(., 
                   value_column = 'duration_value',
                   unit_column = 'duration_unit'
                   ) %>% 
  convert_units(., value_column = 'result', unit_column = 'conc1_unit')
  
# duration ----------------------------------------------------------------

  mutate(test_type = case_when(

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


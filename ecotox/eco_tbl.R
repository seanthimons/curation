# packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(tidyverse)
  library(arrow)
  library(duckdb)
  library(duckplyr)

  library(ComptoxR)

  setwd(here("ecotox"))

  source('queries.R')
}



# Documentation -----------------------------------------------------------

# https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/technical-overview-ecological-risk-assessment-0
# https://sitem.herts.ac.uk/aeru/ppdb/en/docs/2_5eco.pdf

eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)

# query <- ct_list('NEUROTOXINS') %>%
#   pluck(., 1, 'dtxsids') %>%
#   ct_details(query = .)

query <- ct_list(c('PRODWATER', 'FRACFOCUS')) %>% 
  ct_details(query = .)

query_cas <- ct_search(
  query = 'Spirodiclofen',
  search_method = 'equal',
  request_method = 'GET'
) %>%
  pull(casrn) %>%
  str_remove_all(., "-")

query_cas <- query %>%
  pull(casrn) %>%
  str_remove_all(., "-")

# -------------------------------------------------------------------------

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
      #NOTE need logic here on restricting on certain species or not
      #filter(species_number %in% eco_species) %>%
      select(
        species_number,
        common_name,
        latin_name,
        family,
        genus,
        species,
        ncbi_taxid,
        ecotox_group
      ),
    join_by('species_number')
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
    str_detect(
      endpoint,
      "^EC50|^LC50|^LD50|LR50|^LOEC|^LOEL|NOEC|NOEL|NR-ZERO"
    ),
    str_detect(effect, 'MOR|DVP|GRO|MPH'),
    conc1_unit %in%
      c(
        'ug/L',
        'mg/L',
        'ppm',
        'ppb',
        'mg/kg',
        'mg/kg/d',
        'mg/kg bdwt/d',
        'mg/kg diet',
        'g/bee',
        'grams per bee',
        'mg/bee',
        'milligrams per bee',
        'ug/bee',
        'micrograms per bee'
      ),
    obs_duration_unit %in% c('h', 'd', 'wk')
  ) %>%
  left_join(
    tbl(eco_con, 'app_exposure_types') %>%
      select(
        'exposure_group',
        'term'
      ) %>%
      filter(
        exposure_group %in%
          c(
            'AQUA',
            'ENV',
            'ORAL',
            'TOP',
            'Unspecified',
            'UNK'
          )
      ),
    join_by('exposure_type' == 'term')
  ) %>%
  left_join(
    tbl(eco_con, 'lifestage_codes') %>% rename(org_lifestage = description),
    join_by(organism_lifestage == code)
  ) %>%
  left_join(
    tbl(eco_con, 'lifestage_dictionary'),
    join_by(org_lifestage == org_lifestage)
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
    !is.na(obs_duration_unit) & !is.na(obs_duration_mean)
  ) %>%
  mutate(
    #Plus means comment, asterisk mean converted value
    result = as.numeric(str_remove_all(conc1_mean, pattern = "\\*|\\+")),
    effect = case_when(
      str_detect(effect, 'MOR') ~ "MOR",
      str_detect(effect, 'DVP|GRO|MPH') ~ "DVP_GRO_MPH",
      # str_detect(effect, 'GRO') ~ "GRO",
      # str_detect(effect, 'MPH') ~ "MPH"
    ),
    endpoint = case_when(
      endpoint == 'EC50' ~ 'EC50',
      endpoint == 'EC50*' ~ 'EC50',
      endpoint == 'EC50/' ~ 'EC50',
      endpoint == 'LC50' ~ 'LC50',
      endpoint == 'LC50*' ~ 'LC50',
      endpoint == 'LC50*/' ~ 'LC50',
      endpoint == 'LC50/' ~ 'LC50',
      endpoint == 'LD50' ~ 'LD50',
      endpoint == 'LD50/' ~ 'LD50',
      endpoint == 'LOEC' ~ 'LOEC',
      endpoint == 'LOEC/' ~ 'LOEC',
      endpoint == 'LOEL' ~ 'LOEL',
      endpoint == 'LOEL/' ~ 'LOEL',
      endpoint == 'LOELR' ~ 'LOEL',
      endpoint == 'NOEC' ~ 'NOEC',
      endpoint == 'NOEC/' ~ 'NOEC',
      endpoint == 'NOEL' ~ 'NOEL',
      endpoint == 'NOEL/' ~ 'NOEL',
      endpoint == 'NOELR' ~ 'NOEL',
      endpoint == 'NR-ZERO' ~ 'NR-ZERO',
      endpoint == 'NR-ZERO/' ~ 'NR-ZERO',
    ),

    # Eco grouping ------------------------------------------------------------

    eco_group = case_when(
      str_detect(family, 'Megachilidae|Apidae') ~ 'Bees',
      str_detect(ecotox_group, 'Insects/Spiders') ~ 'Insects/Spiders',
      str_detect(ecotox_group, 'Flowers, Trees, Shrubs, Ferns') ~
        'Flowers, Trees, Shrubs, Ferns',
      str_detect(ecotox_group, 'Fungi') ~ 'Fungi',
      str_detect(ecotox_group, 'Algae') ~ 'Algae',
      str_detect(ecotox_group, 'Fish') ~ 'Fish',
      str_detect(ecotox_group, 'Crustaceans') ~ 'Crustaceans',
      str_detect(ecotox_group, 'Invertebrates') ~ 'Invertebrates',
      str_detect(ecotox_group, 'Worms') ~ 'Worms',
      str_detect(ecotox_group, 'Molluscs') ~ 'Molluscs',
      str_detect(ecotox_group, 'Birds') ~ 'Birds',
      str_detect(ecotox_group, 'Mammals') ~ 'Mammals',
      str_detect(ecotox_group, 'Amphibians') ~ 'Amphibians',
      str_detect(ecotox_group, 'Reptiles') ~ 'Reptiles',
      str_detect(ecotox_group, 'Moss, Hornworts') ~ 'Moss, Hornworts',
      .default = ecotox_group
    ),
    duration_value = as.numeric(obs_duration_mean),
    duration_unit = case_when(
      obs_duration_unit == 'h' ~ 'hours',
      obs_duration_unit == 'd' ~ 'days',
      obs_duration_unit == 'wk' ~ 'weeks'
    ),
    harmonized_life_stage = case_when(
      is.na(org_lifestage) ~ 'Other/Unknown',
      .default = harmonized_life_stage
    ),
    harmonized_life_stage = factor(
      harmonized_life_stage,
      levels = c(
        'Egg/Embryo',
        'Larva/Juvenile',
        'Subadult/Immature',
        'Adult',
        'Reproductive',
        'Dormant/Senescent',
        'Other/Unknown'
      )
    )
  ) %>%
  convert_duration(
    .,
    value_column = 'duration_value',
    unit_column = 'duration_unit'
  ) %>%
  convert_units(., value_column = 'result', unit_column = 'conc1_unit') %>%

  # duration ----------------------------------------------------------------

  #filter(eco_group == 'Mammals' | eco_group == 'Birds' | eco_group == 'Fish') %>%
  mutate(
    test_type = case_when(
      # Mammals -----------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Mammals') &
        (effect == 'MOR') &
        (exposure_group == 'ORAL' | is.na(exposure_group)) &
        #(life_stage == 'Adult') &
        (new_unit == 'mg/kg') &
        (endpoint == 'LD50') ~
        'acute',

      (eco_group == 'Mammals') &
        (effect == 'MOR') &
        (exposure_group == 'ORAL' | is.na(exposure_group)) &
        #(life_stage == 'Adult') &
        (new_unit == 'mg/kg bdwt') &
        (endpoint == 'LD50') ~
        'acute',
      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Mammals') &
        (effect == 'MOR') &
        (exposure_group == 'ORAL' | is.na(exposure_group)) &
        #(life_stage == 'Adult') &
        (endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
        (new_unit == 'mg/kg/d') ~
        'chronic',

      # Birds -------------------------------------------------------------------
      #NOTE Needs better breakdown for reptiles and amphibians...
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Birds' |
        eco_group == 'Amphibians' |
        eco_group == 'Reptiles') &
        (effect == 'MOR') &
        (exposure_group == 'ORAL' | is.na(exposure_group)) &
        #(life_stage == 'Adult') &
        (new_unit == 'mg/kg') &
        (endpoint == 'LD50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Birds' |
        eco_group == 'Amphibians' |
        eco_group == 'Reptiles') &
        (effect == 'MOR') &
        (exposure_group == 'ORAL' | is.na(exposure_group)) &
        #(life_stage == 'Adult') &
        (endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
        (new_unit == 'mg/kg/d' | new_unit == 'mg/kg bdwt/d') ~
        'chronic',

      # Fish --------------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Fish') &
        (effect == 'MOR') &
        #(life_stage == 'Adult') &
        (new_dur == 96) &
        (new_unit == 'mg/L') &
        (endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Fish') &
        (effect == 'MOR') &
        #(life_stage == 'Adult') &
        (new_dur >= 144) &
        (new_unit == 'mg/L') &
        (endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
        'chronic',

      (eco_group == 'Fish') &
        (effect == 'MOR') &
        #(life_stage == 'Adult') &
        (new_dur == 504) &
        (new_unit == 'mg/L') &
        (endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
        'chronic',

      # Bees --------------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      # Could be refined later, but OPP doesn't isn't entirely clear
      (eco_group == 'Bees') &
        (effect == 'MOR') &
        (new_dur == 24 | new_dur == 28 | new_dur == 72) &
        (new_unit == 'ug/bee') &
        (endpoint == 'LD50' | endpoint == 'LC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Bees') &
        (effect == 'MOR') &
        (new_dur == 240) &
        (new_unit == 'ug/bee') &
        (endpoint == 'LD50' | endpoint == 'LC50') ~
        'chronic',

      # Insects -----------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Insects/Spiders') &
        (effect == 'MOR') &
        (new_dur == 24 | new_dur == 48 | new_dur == 72) &
        (new_unit == 'mg/L' | new_unit == 'mg/kg') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------

      (eco_group == 'Insects/Spiders') &
        (effect == 'MOR') &
        (new_dur == 504 | new_dur == 672) &
        (new_unit == 'mg/L' | new_unit == 'mg/kg') &
        (endpoint == 'NOEL' | endpoint == 'NOEC' | endpoint == 'NR-ZERO') ~
        'chronic',

      # Invertebrates -----------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
        (effect == 'MOR') &
        (new_dur == 24 | new_dur == 48 | new_dur == 72 | new_dur == 96) &
        (new_unit == 'mg/L' | new_unit == 'mg/kg') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
        (effect == 'MOR') &
        (new_dur == 504 | new_dur == 672) &
        (new_unit == 'mg/L' | new_unit == 'mg/kg') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'chronic',

      # Worms -------------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Worms') &
        (effect == 'MOR') &
        (new_dur == 336) &
        (new_unit == 'mg/kg') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Worms') &
        (effect == 'MOR') &
        (new_dur <= 336) &
        (new_unit == 'mg/kg') &
        (endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
        'chronic',

      # Crustaceans -----------------------------------------------------------------------
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Crustaceans') &
        (effect == 'MOR') &
        (new_dur <= 96) &
        (new_unit == 'mg/L') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Crustaceans') &
        (effect == 'MOR') &
        (new_dur >= 672) &
        (new_unit == 'mg/L') &
        (endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
        'chronic',

      # Algae -----------------------------------------------------------------------
      #NOTE Needs better, hard to find data for fungi and mosses etc
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        (new_dur <= 24 * 7) &
        (new_unit == 'mg/L') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        (new_dur == 96) &
        (new_unit == 'mg/L') &
        (endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
        'chronic',

      # Flowers, Trees, Shrubs, Ferns-----------------------------------------------------------------------
      #Note probably could be something like developemental etc...
      ## Acute -------------------------------------------------------------------
      (eco_group == 'Flowers, Trees, Shrubs, Ferns') &
        (new_dur <= 7 * 24) &
        (new_unit == 'mg/L') &
        (endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
        'acute',

      ## Chronic -----------------------------------------------------------------
      (eco_group == 'Flowers, Trees, Shrubs, Ferns') &
        (effect != 'MOR') &
        (endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
        'chronic',
    )
  ) %>%
  filter(!is.na(test_type))


# Binned table -----------------------------------------------------------


eco_risk_bin_tbl <- eco_risk_tbl %>%
  select(
    test_cas,
    test_type,
    eco_group,
    new_value
  ) %>%
  #filter(eco_group == 'Mammals') %>%
  mutate(
    bin = case_when(
      # Mammals -----------------------------------------------------------------

      eco_group == 'Mammals' & test_type == 'acute' & new_value < 10 ~ "VH",
      eco_group == 'Mammals' &
        test_type == 'acute' &
        between(new_value, 10, 50) ~
        "H",
      eco_group == 'Mammals' &
        test_type == 'acute' &
        between(new_value, 50, 500) ~
        "M",
      eco_group == 'Mammals' &
        test_type == 'acute' &
        between(new_value, 500, 2000) ~
        "L",
      eco_group == 'Mammals' & test_type == 'acute' & new_value > 2000 ~ "XL",

      eco_group == 'Mammals' & test_type == 'chronic' & new_value < 1 ~ "VH",
      eco_group == 'Mammals' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Mammals' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Mammals' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Mammals' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Birds -------------------------------------------------------------------
      eco_group == 'Birds' & test_type == 'acute' & new_value < 10 ~ "VH",
      eco_group == 'Birds' & test_type == 'acute' & between(new_value, 10, 50) ~
        "H",
      eco_group == 'Birds' &
        test_type == 'acute' &
        between(new_value, 50, 500) ~
        "M",
      eco_group == 'Birds' &
        test_type == 'acute' &
        between(new_value, 500, 2000) ~
        "L",
      eco_group == 'Birds' & test_type == 'acute' & new_value > 2000 ~ "XL",

      eco_group == 'Birds' & test_type == 'chronic' & new_value < 1 ~ "VH",
      eco_group == 'Birds' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Birds' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Birds' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Birds' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Fish -------------------------------------------------------------------

      eco_group == 'Fish' & test_type == 'acute' & new_value < 0.1 ~ "VH",
      eco_group == 'Fish' & test_type == 'acute' & between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Fish' & test_type == 'acute' & between(new_value, 1, 10) ~
        "M",
      eco_group == 'Fish' & test_type == 'acute' & between(new_value, 10, 100) ~
        "L",
      eco_group == 'Fish' & test_type == 'acute' & new_value > 100 ~ "XL",

      eco_group == 'Fish' & test_type == 'chronic' & new_value < 0.01 ~ "VH",
      eco_group == 'Fish' & test_type == 'chronic' & between(new_value, 1, 10) ~
        "H",
      eco_group == 'Fish' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Fish' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Fish' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Bees --------------------------------------------------------------------

      eco_group == 'Bees' & test_type == 'acute' & new_value < 0.1 ~ "VH",
      eco_group == 'Bees' & test_type == 'acute' & between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Bees' & test_type == 'acute' & between(new_value, 1, 10) ~
        "M",
      eco_group == 'Bees' & test_type == 'acute' & between(new_value, 10, 100) ~
        "L",
      eco_group == 'Bees' & test_type == 'acute' & new_value > 100 ~ "XL",

      eco_group == 'Bees' & test_type == 'chronic' & new_value < 0.01 ~ "VH",
      eco_group == 'Bees' & test_type == 'chronic' & between(new_value, 1, 10) ~
        "H",
      eco_group == 'Bees' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Bees' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Bees' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Insects -----------------------------------------------------------------

      eco_group == 'Insects' & test_type == 'acute' & new_value < 0.1 ~ "VH",
      eco_group == 'Insects' &
        test_type == 'acute' &
        between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Insects' &
        test_type == 'acute' &
        between(new_value, 1, 10) ~
        "M",
      eco_group == 'Insects' &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      eco_group == 'Insects' & test_type == 'acute' & new_value > 100 ~ "XL",

      eco_group == 'Insects' & test_type == 'chronic' & new_value < 0.01 ~ "VH",
      eco_group == 'Insects' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Insects' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Insects' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Insects' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Invertebrates -----------------------------------------------------------

      eco_group == 'Invertebrates' & test_type == 'acute' & new_value < 0.1 ~
        "VH",
      eco_group == 'Invertebrates' &
        test_type == 'acute' &
        between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Invertebrates' &
        test_type == 'acute' &
        between(new_value, 1, 10) ~
        "M",
      eco_group == 'Invertebrates' &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      eco_group == 'Invertebrates' & test_type == 'acute' & new_value > 100 ~
        "XL",

      eco_group == 'Invertebrates' & test_type == 'chronic' & new_value < 0.01 ~
        "VH",
      eco_group == 'Invertebrates' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Invertebrates' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Invertebrates' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Invertebrates' & test_type == 'chronic' & new_value > 1000 ~
        "XL",

      # Worms --------------------------------------------------------------------

      eco_group == 'Worms' & test_type == 'acute' & new_value < 0.1 ~ "VH",
      eco_group == 'Worms' & test_type == 'acute' & between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Worms' & test_type == 'acute' & between(new_value, 1, 10) ~
        "M",
      eco_group == 'Worms' &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      eco_group == 'Worms' & test_type == 'acute' & new_value > 100 ~ "XL",

      eco_group == 'Worms' & test_type == 'chronic' & new_value < 0.01 ~ "VH",
      eco_group == 'Worms' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Worms' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Worms' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Worms' & test_type == 'chronic' & new_value > 1000 ~ "XL",

      # Crustaceans -------------------------------------------------------------

      eco_group == 'Crustaceans' & test_type == 'acute' & new_value < 0.1 ~
        "VH",
      eco_group == 'Crustaceans' &
        test_type == 'acute' &
        between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Crustaceans' &
        test_type == 'acute' &
        between(new_value, 1, 10) ~
        "M",
      eco_group == 'Crustaceans' &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      eco_group == 'Crustaceans' & test_type == 'acute' & new_value > 100 ~
        "XL",

      eco_group == 'Crustaceans' & test_type == 'chronic' & new_value < 0.01 ~
        "VH",
      eco_group == 'Crustaceans' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Crustaceans' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Crustaceans' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Crustaceans' & test_type == 'chronic' & new_value > 1000 ~
        "XL",

      # Algae -------------------------------------------------------------------

      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'acute' &
        new_value < 0.1 ~
        "VH",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'acute' &
        between(new_value, 0.1, 1) ~
        "H",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'acute' &
        between(new_value, 1, 10) ~
        "M",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'acute' &
        new_value > 100 ~
        "XL",

      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'chronic' &
        new_value < 0.01 ~
        "VH",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      (eco_group == 'Algae' |
        eco_group == 'Fungi' |
        eco_group == 'Moss, Hornworts') &
        test_type == 'chronic' &
        new_value > 1000 ~
        "XL",

      # Flowers -----------------------------------------------------------------

      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'acute' &
        new_value < 0.1 ~
        "VH",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'acute' &
        between(new_value, 0.1, 1) ~
        "H",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'acute' &
        between(new_value, 1, 10) ~
        "M",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'acute' &
        between(new_value, 10, 100) ~
        "L",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'acute' &
        new_value > 100 ~
        "XL",

      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'chronic' &
        new_value < 0.01 ~
        "VH",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'chronic' &
        between(new_value, 1, 10) ~
        "H",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'chronic' &
        between(new_value, 10, 200) ~
        "M",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'chronic' &
        between(new_value, 200, 1000) ~
        "L",
      eco_group == 'Flowers, Trees, Shrubs, Ferns' &
        test_type == 'chronic' &
        new_value > 1000 ~
        "XL",

      .default = NA
    ),
    super_int = case_when(
      bin == 'XH' ~ 6,
      bin == 'VH' ~ 5,
      bin == 'H' ~ 4,
      bin == 'M' ~ 3,
      bin == 'L' ~ 2,
      bin == 'VL' ~ 1,
      bin == 'ND' ~ 0,
      is.na(bin) ~ 0,
      .default = 0
    )
  ) %>%
  filter(!is.na(bin)) %>%
  group_by(test_cas, test_type, eco_group) %>%
  mutate(
    super_bin = ceiling(
      mean(super_int)
    ),
    super_bin = case_when(
      super_bin >= 6 ~ 'XH',
      super_bin == 5 ~ 'VH',
      super_bin == 4 ~ 'H',
      super_bin == 3 ~ 'M',
      super_bin == 2 ~ 'L',
      super_bin == 1 ~ 'VL',
      super_bin == 0 ~ 'ND',
      is.na(super_bin) ~ 'ND'
    )
  ) %>%
  ungroup() %>%
  distinct(
    test_cas,
    test_type,
    eco_group,
    super_bin
  ) %>%

  #Note need to remove duplicates
  pivot_wider(
    .,
    id_cols = test_cas,
    names_from = c(eco_group, test_type),
    names_sort = TRUE,
    values_from = c(
      super_bin
    )
  )
select(
  test_cas,
  Birds_acute,
  Fish_acute,
  Fish_chronic,
  Mammals_acute,
  Mammals_chronic,
  Bees_acute
) %>%
  mutate(
    h_count = rowSums(
      across(everything(), ~ str_detect(.x, "H"))
    ),
    na_count = rowSums(!is.na(.))
  ) %>%
  arrange((h_count))

library(DT)
proc_export <- eco_risk_bin_tbl %>%
  DT::datatable(
    .,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20)
    )
  ) %>%
  formatStyle(names(eco_risk_bin_tbl), textAlign = "center") %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("XH", "darkred")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("VH", "#dc3545")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("H", "#fd7e14")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("M", "#ffc107")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("L", "#28a745")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("VL", "darkgreen")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("ND", "grey")
  )

proc_export

# testing -----------------------------------------------------------------

eco_risk_tbl %>%
  select(
    eco_group,
    family,
    genus,
    species,
    exposure_group,
    life_stage,
    test_type,
    endpoint,
    new_dur,
    new_unit
  ) %>%
  filter(
    eco_group == 'Invertebrates'
    #family == 'Megachilidae',
    #exposure_group == 'ORAL' | is.na(exposure_group),
    #life_stage == 'Adult',
    #endpoint == '',
    #new_unit == 'ug/bee',
    #new_dur == 240
  ) %>%
  group_by(
    family,
    endpoint,
    test_type,
    exposure_group,
    life_stage,
    new_unit
  ) %>%
  summarize(
    n = n(),
    min = min(new_dur),
    mean = mean(new_dur),
    max = max(new_dur)
  ) %>%
  print(n = Inf)

eco_risk_tbl %>%
  distinct(new_unit) %>%
  pull(1) %>%
  sort()

tbl(eco_con, 'results') %>%
  distinct(effect) %>%
  collect() %>%
  mutate(effect = str_remove_all(effect, pattern = "\\/")) %>%
  distinct() %>%
  left_join(
    .,
    tbl(eco_con, 'effect_codes') %>% collect(),
    join_by(effect == code)
  ) %>%
  arrange(description) %>%
  print(n = Inf)

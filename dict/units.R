# packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(stringdist)
  library(fuzzyjoin)
  library(todor)

  setwd(here('dict'))

}


# raw ---------------------------------------------------------------------

# https://apps.ecology.wa.gov/eim/help/ValidValues/

preferred_units <- rio::import(file = here('dict', 'Units.csv')) %>%
  janitor::clean_names()

unit_conv <- rio::import(file = here('dict', 'UOMConversion.csv')) %>%
  janitor::clean_names() %>%
  select(3:6)

unit_dict <- list(preferred_units = preferred_units, unit_conv = unit_conv)

saveRDS(unit_dict, file = here('final', 'unit_dict.RDS'))



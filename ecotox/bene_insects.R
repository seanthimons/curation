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
}

eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)

# insects -----------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(
      family,
      pattern = 'Coccinellidae|Cecidomyiidae|Reduviidae|Nabidae|Chrysopidae|Anthocoridae|Lampyridae|Cantharidae|Syrphidae|Tachinidae|Trichogrammatidae|Braconidae|Ichneumonidae|Mantidae|Megachilidae|Apidae'
    )
  )


# bees --------------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(family, pattern = 'Megachilidae|Apidae')
  )


# mammals -----------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(tax_order, pattern = 'Lagomorpha') |
      str_detect(family, pattern = 'Canidae|Muridae|Caviidae')
  )

# birds -------------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(tax_order, pattern = 'Passeriformes') |
      str_detect(family, pattern = 'Anatidae|Odontophoridae')
  )


# soil macro --------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    # worms
    str_detect(tax_order, pattern = 'Opisthopora')
  )


# aq invert ---------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(family, pattern = 'Daphniidae')
  )

# aq crust ----------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(family, pattern = 'Mysidae') |
      ecotox_group == 'CrustaceansStandard Test Species'
  )

# sed org -----------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid
  ) %>%
  filter(
    str_detect(family, pattern = 'Chironomidae')
  )


# fish --------------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    ecotox_group == 'FishStandard Test Species'
  ) %>%
  collect()


# algae -------------------------------------------------------------------

tbl(eco_con, 'species') %>%
  distinct(
    species_number,
    common_name,
    latin_name,
    class,
    tax_order,
    family,
    genus,
    species,
    ncbi_taxid,
    ecotox_group
  ) %>%
  filter(
    str_detect(common_name, 'Green algae|Green Algae') %>%
      str_detect(., 'Blue', negate = TRUE)
  ) %>%
  collect()

# super -------------------------------------------------------------------

eco_species_main <-
  tbl(eco_con, 'species') %>%
  filter(
    str_detect(
      family,
      pattern = 'Coccinellidae|Cecidomyiidae|Reduviidae|Nabidae|Chrysopidae|Anthocoridae|Lampyridae|Cantharidae|Syrphidae|Tachinidae|Trichogrammatidae|Braconidae|Ichneumonidae|Mantidae|Megachilidae|Apidae'
    ) |

      str_detect(tax_order, pattern = 'Lagomorpha') |
      str_detect(family, pattern = 'Canidae|Muridae|Caviidae') |

      str_detect(tax_order, pattern = 'Passeriformes') |
      str_detect(family, pattern = 'Anatidae|Odontophoridae') |

      str_detect(tax_order, pattern = 'Opisthopora') |

      str_detect(family, pattern = 'Daphniidae') |

      str_detect(family, pattern = 'Mysidae') |
      ecotox_group == 'CrustaceansStandard Test Species' |

      str_detect(family, pattern = 'Chironomidae') |

      ecotox_group == 'FishStandard Test Species' |

      str_detect(family, pattern = 'Lemnaceae') |

      str_detect(ecotox_group, pattern = 'Algae')
  ) %>%
  pull(species_number)

eco_species_other <-
  tbl(eco_con, 'species') %>%
  mutate(
    eco_group = case_when(
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
    )
  ) %>%
  filter(
    str_detect(
      eco_group,
      pattern = 'Fungi|Flowers|Molluscs|Amphibians|Reptiles|Moss, Hornworts'
    )
  ) %>%
  pull(species_number)

eco_species <- c(eco_species_main, eco_species_other)

tbl(eco_con, 'species') %>%
  filter(species_number %in% eco_species) %>%
  collect() %>%
  View()


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
  ) %>%
  distinct(conc1_unit) %>%
  arrange(conc1_unit) %>%
  filter(str_detect(conc1_unit, pattern = 'soil')) %>%
  collect() %>%
  print(n = Inf)

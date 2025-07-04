# packages ----------------------------------------------------------------

{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(arrow)
  library(duckdb)
  library(duckplyr)
  library(plumber)

  # library(ComptoxR)

  setwd(here("ecotox"))
}


# download ----------------------------------------------------------------
{
  cli::cli_alert_info('Downloading zip')
  resp <- GET(url = 'https://gaftp.epa.gov/ecotox/') %>%
    content(.) %>%
    html_elements(., 'a') %>%
    html_attr('href') %>%
    .[str_detect(., pattern = 'zip')] %>%
    data.frame(file = .) %>%
    mutate(
      date = str_remove_all(file, pattern = 'ecotox_ascii_'),
      date = str_remove_all(date, pattern = '.zip'),
      date = lubridate::as_date(date, format = "%m_%d_%Y")
    ) %>%
    arrange(desc(date)) %>%
    mutate(
      latest = case_when(
        row_number() == 1 ~ TRUE,
        .default = FALSE
      )
    )

  if (nrow(resp) == 0) {
    cli::cli_abort('Request failed for downloading data')
    rm(resp)
  }

  download.file(
    paste0('https://gaftp.epa.gov/ecotox/', resp$file[which.max(resp$date)]),
    destfile = here('ecotox', 'ecotox.zip')
  )

  unzip('ecotox.zip')

  eco_folder <- list.files(here('ecotox'))[str_detect(
    list.files(here('ecotox')),
    pattern = 'ecotox_ascii'
  )]

  file.rename(
    here('ecotox', eco_folder),
    here('ecotox', 'eco_files')
  )
}
# main --------------------------------------------------------------------
{
  setwd(here('ecotox', 'eco_files'))
  cli::cli_alert_info('Removing unneeded files')
  unlink(list.files(pattern = 'release'))
  unlink(list.files(pattern = 'ASCII|Ascii'))

  #eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)
  eco_con <- dbConnect(duckdb(), dbdir = ":memory:", read_only = FALSE)

  cli::cli_alert_info('Converting base files')
  walk(
    list.files(pattern = ".txt")[
      !str_detect(list.files(pattern = ".txt"), pattern = "release")
    ],
    function(x) {
      cli::cli_text(x)

      read_delim(
        file = x,
        delim = "|",
        col_types = cols(.default = col_character()),
        na = c("", "NA", 'NR', "NC", "", "-", "--", 'NONE', 'UKN', 'UKS'),
        locale = locale(encoding = "latin1")
      ) %>%
        janitor::remove_empty(., which = 'cols') %>%
        arrow::write_parquet(
          x = .,
          sink = paste0(str_remove(x, pattern = ".txt"), '.parquet')
        )

      unlink(x)
    },
    .progress = TRUE
  )

  cli::cli_alert_info('Writing files to db')
  walk(
    list.files(pattern = ".parquet"),
    function(x) {
      cli::cli_text(x)

      dbWriteTable(
        eco_con,
        str_remove(x, pattern = ".parquet"),
        read_parquet(x),
        overwrite = TRUE
      )

      unlink(x)
    }
  )

  dbListTables(eco_con)
}

# validation --------------------------------------------------------------
{
  cli::cli_alert_info('Converting validation files')
  setwd(here("ecotox", 'eco_files', "validation"))
  list.files()

  walk(
    list.files(pattern = ".txt"),
    function(x) {
      cli::cli_text(x)

      read_delim(
        file = x,
        delim = "|",
        col_types = cols(.default = col_character()),
        na = c("", "NA", 'NR', "NC", "", "-", "--", 'NONE', 'UKN', 'UKS'),
        locale = locale(encoding = "latin1")
      ) %>%
        janitor::remove_empty(., which = 'cols') %>%
        arrow::write_parquet(
          x = .,
          sink = paste0(str_remove(x, pattern = ".txt"), '.parquet')
        )

      unlink(x)
    }
  )

  walk(
    list.files(pattern = ".parquet"),
    function(x) {
      cli::cli_text(x)

      dbWriteTable(
        eco_con,
        str_remove(x, pattern = ".parquet"),
        read_parquet(x),
        overwrite = TRUE
      )

      unlink(x)
    }
  )
}

# terms appendix ----------------------------------------------------------
{
  setwd(here('ecotox'))
  cli::cli_alert_info('Downloading appendix terms')

  GET(url = 'https://gaftp.epa.gov/ecotox/') %>%
    content(.) %>%
    html_elements(., 'a') %>%
    html_attr('href') %>%
    .[str_detect(., pattern = 'xlsx')] %>%
    paste0('https://gaftp.epa.gov/ecotox/', .) %>%
    download.file(
      url = .,
      destfile = here('ecotox', 'ecotox_terms_appendix.xlsx'),
      mode = 'wb'
    )

  ecotox_appendix <- import_list(file = 'ecotox_terms_appendix.xlsx', skip = 2)

  eco_toc <- ecotox_appendix %>%
    pluck(., 1) %>%
    pull(., Title) %>%
    janitor::make_clean_names() %>%
    paste0('app_', .)

  ecotox_appendix <- ecotox_appendix %>%
    discard_at(., 1) %>%
    set_names(., eco_toc) %>%
    map(., janitor::clean_names)

  iwalk(
    ecotox_appendix,
    function(x, y) {
      cli::cli_text(y)
      #cli::cli_alert_info(x)

      dbWriteTable(eco_con, y, x, overwrite = TRUE)
    }
  )

  unlink('ecotox_terms_appendix.xlsx')
  unlink('eco_files', recursive = TRUE)
}

# Life stage harmonization -----------------------------------------------

{
  life_stage <- tribble(
    ~org_lifestage,
    ~harmonized_life_stage,
    'Unspecified',
    'Other/Unknown',
    'Adult',
    'Adult',
    'Alevin',
    'Larva/Juvenile',
    'Bud or Budding',
    'Other/Unknown',
    'Blastula',
    'Egg/Embryo',
    'Bud blast stage',
    'Adult',
    'Boot',
    'Adult',
    'Cocoon',
    'Adult',
    'Corm',
    'Adult',
    'Copepodid',
    'Larva/Juvenile',
    'Copepodite',
    'Larva/Juvenile',
    'Cleavage stage',
    'Egg/Embryo',
    'Cyst',
    'Dormant/Senescent',
    'Egg',
    'Egg/Embryo',
    'Elver',
    'Larva/Juvenile',
    'Embryo',
    'Egg/Embryo',
    'Exponential growth phase (log)',
    'Other/Unknown',
    'Eyed egg or stage, eyed embryo',
    'Egg/Embryo',
    'F0 generation',
    'Reproductive',
    'F1 generation',
    'Reproductive',
    'F11 generation',
    'Reproductive',
    'F2 generation',
    'Reproductive',
    'F3 generation',
    'Reproductive',
    'F6 generation',
    'Reproductive',
    'F7 generation',
    'Reproductive',
    'Mature (full-bloom stage) organism',
    'Adult',
    'Female gametophyte',
    'Reproductive',
    'Fingerling',
    'Larva/Juvenile',
    'Flower opening',
    'Reproductive',
    'Froglet',
    'Larva/Juvenile',
    'Fry',
    'Larva/Juvenile',
    'Gastrula',
    'Egg/Embryo',
    'Gestation',
    'Reproductive',
    'Glochidia',
    'Larva/Juvenile',
    'Gamete',
    'Reproductive',
    'Lag growth phase',
    'Other/Unknown',
    'Grain or seed formation stage',
    'Adult',
    'Germinated seed',
    'Dormant/Senescent',
    'Heading',
    'Adult',
    'Incipient bud',
    'Adult',
    'Internode elongation',
    'Adult',
    'Imago',
    'Adult',
    'Immature',
    'Subadult/Immature',
    'Instar',
    'Larva/Juvenile',
    'Intermolt',
    'Other/Unknown',
    'Jointing',
    'Adult',
    'Juvenile',
    'Larva/Juvenile',
    'Lactational',
    'Reproductive',
    'Egg laying',
    'Reproductive',
    'Larva-pupa',
    'Larva/Juvenile',
    'Prolarva',
    'Larva/Juvenile',
    'Larva',
    'Larva/Juvenile',
    'Mature',
    'Adult',
    'Mature dormant',
    'Dormant/Senescent',
    'Megalopa',
    'Larva/Juvenile',
    'Male gametophyte',
    'Reproductive',
    'Morula',
    'Egg/Embryo',
    'Mid-neurula',
    'Egg/Embryo',
    'Molt',
    'Other/Unknown',
    'Multiple',
    'Other/Unknown',
    'Mysis',
    'Larva/Juvenile',
    'Newborn',
    'Larva/Juvenile',
    'Naiad',
    'Larva/Juvenile',
    'Neonate',
    'Larva/Juvenile',
    'New, newly or recent hatch',
    'Larva/Juvenile',
    'Neurala',
    'Egg/Embryo',
    'Not intact',
    'Other/Unknown',
    'Not reported',
    'Other/Unknown',
    'Nauplii',
    'Larva/Juvenile',
    'Nymph',
    'Larva/Juvenile',
    'Oocyte, ova',
    'Egg/Embryo',
    'Parr',
    'Larva/Juvenile',
    'Mature, post-bloom stage (fruit trees)',
    'Adult',
    'Pre-hatch',
    'Other/Unknown',
    'Pre-molt',
    'Other/Unknown',
    'Post-emergence',
    'Adult',
    'Post-spawning',
    'Reproductive',
    'Mature, pit-hardening stage (fruit trees)',
    'Adult',
    'Post-hatch',
    'Other/Unknown',
    'Post-molt',
    'Other/Unknown',
    'Pre-, sub-, semi-, near adult, or peripubertal',
    'Subadult/Immature',
    'Post-smolt',
    'Larva/Juvenile',
    'Pullet',
    'Larva/Juvenile',
    'Post-nauplius',
    'Larva/Juvenile',
    'Pollen, pollen grain',
    'Reproductive',
    'Postpartum',
    'Reproductive',
    'Prepupal',
    'Larva/Juvenile',
    'Pre-larva',
    'Larva/Juvenile',
    'Prebloom',
    'Reproductive',
    'Pre-smolt',
    'Larva/Juvenile',
    'Protolarvae',
    'Larva/Juvenile',
    'Pupa',
    'Larva/Juvenile',
    'Post-larva',
    'Larva/Juvenile',
    'Pre-spawning',
    'Reproductive',
    'Post-embryo',
    'Other/Unknown',
    'Protozoea',
    'Larva/Juvenile',
    'Rooted cuttings',
    'Adult',
    'Rhizome',
    'Adult',
    'Mature reproductive',
    'Reproductive',
    'Rootstock',
    'Other/Unknown',
    'Subadult',
    'Subadult/Immature',
    'Shoot',
    'Adult',
    'Yolk sac larvae, sac larvae',
    'Larva/Juvenile',
    'Senescence',
    'Dormant/Senescent',
    'Seed',
    'Adult',
    'Scape elongation',
    'Adult',
    'Sac fry, yolk sac fry',
    'Larva/Juvenile',
    'Mature, side-green stage (fruit trees)',
    'Adult',
    'Sexually immature',
    'Subadult/Immature',
    'Seedling',
    'Larva/Juvenile',
    'Sexually mature',
    'Adult',
    'Smolt',
    'Larva/Juvenile',
    'Sapling',
    'Adult',
    'Sporeling',
    'Larva/Juvenile',
    'Sperm',
    'Reproductive',
    'Spore',
    'Other/Unknown',
    'Spat',
    'Adult',
    'Swim-up',
    'Larva/Juvenile',
    'Spawning',
    'Reproductive',
    'Stationary growth phase',
    'Dormant/Senescent',
    'Tadpole',
    'Larva/Juvenile',
    'Tissue culture callus',
    'Other/Unknown',
    'Tiller stage',
    'Adult',
    'Tuber',
    'Adult',
    'Trophozoite',
    'Other/Unknown',
    'Underyearling',
    'Larva/Juvenile',
    'Veliger',
    'Larva/Juvenile',
    'Mature vegetative',
    'Other/Unknown',
    'Virgin',
    'Other/Unknown',
    'Weanling',
    'Larva/Juvenile',
    'Young adult',
    'Subadult/Immature',
    'Yearling',
    'Larva/Juvenile',
    'Young',
    'Larva/Juvenile',
    'Young of year',
    'Larva/Juvenile',
    'Zoea',
    'Larva/Juvenile',
    'Zygospore',
    'Egg/Embryo',
    'Zygote',
    'Egg/Embryo'
  )

  dbWriteTable(eco_con, 'lifestage_dictionary', life_stage)
}

# clean up ----------------------------------------------------------------
{
  if (length(list.files(pattern = '.parquet')) > 0) {
    file.remove(list.files(pattern = '.parquet'))
  }

  dbWriteTable(eco_con, 'versions', resp, overwrite = TRUE)

  setwd(here("ecotox"))

  file.remove(here('ecotox', 'ecotox.zip'))

  unlink(here('ecotox', 'eco_files', 'validation'), recursive = TRUE)

  rm(resp, ecotox_appendix, eco_toc, eco_folder)

  dbListTables(eco_con)

  dbExecute(
    eco_con,
    "ATTACH 'ecotox.duckdb';
  COPY FROM DATABASE memory TO ecotox;
  DETACH ecotox;"
  )

  dbDisconnect(eco_con)
  rm(eco_con)
}
# deploy ------------------------------------------------------------------

#source(here("ecotox", "plumber.R"))

plumber::pr("plumber.R") %>% plumber::pr_run()

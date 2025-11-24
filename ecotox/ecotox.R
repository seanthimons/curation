# packages ----------------------------------------------------------------
{
  {
    install_booster_pack <- function(package, load = TRUE) {
      # Loop through each package
      for (pkg in package) {
        # Check if the package is installed
        if (!requireNamespace(pkg, quietly = TRUE)) {
          # If not installed, install the package
          install.packages(pkg)
        }
        # Load the package
        if (load) {
          library(pkg, character.only = TRUE)
        }
      }
    }

    if (file.exists('packages.txt')) {
      packages <- read.table('packages.txt')

      install_booster_pack(package = packages$Package, load = FALSE)

      rm(packages)
    } else {
      # Packages ----

      booster_pack <- c(
        ## IO ----
        'fs',
        'here',
        'janitor',
        'rio',
        'tidyverse',
        # 'data.table',
        #'mirai',
        #'targets',
        #'crew',

        ## DB ----
        'arrow',
        # 'nanoparquet',
        'duckdb',
        'duckplyr',
        'dbplyr',

        ## EDA ----
        'skimr',

        ## Web ----
        'rvest',
        'polite',
        'plumber',
        #	'plumber2', #Still experimental
        'httr',
        'httr2',

        ## Plot ----
        # 'paletteer',
        # 'ragg',
        # 'camcorder',
        # 'esquisse',
        # 'geofacet',
        # 'patchwork',
        # 'marquee',
        # 'ggiraph',
        # 'geomtextpath',
        # 'ggpattern',
        # 'ggbump',
        # 'gghighlight',
        # 'ggdist',
        # 'ggforce',
        # 'gghalves',
        # 'ggtext',
        # 'ggrepel',   # Suggested for non-overlapping labels
        # 'gganimate', # Suggested for animations
        # 'ggsignif',
        # 'ggTimeSeries',

        ## Modeling ----
        # 'tidymodels',

        ## Shiny ----
        # 'shiny',
        # 'bslib',
        # 'DT',
        # 'plotly',

        ## Reporting ----
        # 'quarto',
        # 'gt',

        ## Spatial ----
        # 'sf',
        # 'geoarrow',
        # 'duckdbfs',
        # 'duckspatial',
        # 'ducksf',
        # 'tidycensus', # Needs API
        # 'mapgl',
        # 'dataRetrieval', # Needs API
        # 'StreamCatTools',

        ## Misc ----
        # 'devtools',
        # 'usethis',
        # 'pak',
        'usethis',
        'remotes'
      )

      # ! Change load flag to load packages
      install_booster_pack(package = booster_pack, load = TRUE)
      rm(install_booster_pack, booster_pack)
    }
  }

  # Custom Functions ----

  geometric_mean <- function(x, na.rm = TRUE) {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }

  # `%ni%` <- Negate(`%in%`)

  # skim_count <- skim_with(
  # 	numeric = sfl(
  # 		n = length,
  # 		min = ~ min(.x, na.rm = T),
  # 		median = ~ median(.x, na.rm = T),
  # 		max = ~ max(.x, na.rm = T)
  # 	)
  # )

  # Camcorder ----

  # gg_record(
  # 	here::here('output'),
  # 	device = "png",
  # 	width = 10,
  # 	height = 7,
  # 	units = "in",
  # 	dpi = 320
  # )

  # Theme ----

  # theme_custom <- function() {
  # 	theme_minimal() +
  # 		theme(
  # 			plot.background = element_rect(colour = "white"),
  # 			panel.grid.major = element_blank(),
  # 			panel.grid.minor = element_blank(),
  # 			strip.background = element_rect(colour = "white"),
  # 			axis.text.x = element_text(angle = 90L)
  # 		)

  # library(ComptoxR)

  setwd(here("ecotox"))
}

# !!!!!! Deploy flag -------------------------------------------------------------

deploy = FALSE

# Checkpoint -------------------------------------------------------------

# Determine if a rebuild is necessary.
# A rebuild is needed if any file is missing, or if any existing file is older than 90 days.
{
  files_to_check <- c(
    "ecotox.duckdb"
  )

  files_exist_check <- file.exists(files_to_check)

  rebuild_is_needed <- if (!all(files_exist_check)) {
    cli::cli_alert_info(
      "One or more data files are missing. Rebuilding dataset."
    )
    tibble(files_to_check, files_exist_check) %>% print()
    TRUE
  } else {
    # Using mtime (modification time) is more robust for checking data freshness.
    file_ages_days <- difftime(
      Sys.time(),
      file.info(files_to_check)$mtime,
      units = "days"
    )
    if (any(file_ages_days > 180)) {
      if (interactive()) {
        cli::cli_alert_warning(
          "One or more data files are older than 180 days."
        )
        tibble(files_to_check, files_exist_check, file_ages_days) %>% print()

        if (usethis::ui_yeah("Do you want to rebuild the dataset?")) {
          cli::cli_alert_success("User approved. Rebuilding dataset.")
          TRUE
        } else {
          cli::cli_alert_abort("User declined. Skipping rebuild.")
          FALSE
        }
      } else {
        # In non-interactive mode, proceed with rebuild
        cli::cli_alert_warning("Data files are old. Rebuilding dataset.")
        TRUE
      }
    } else {
      cli::cli_alert_success(
        "All data files are present and up-to-date. Skipping rebuild."
      )
      tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
      rm(files_to_check, files_exist_check, file_ages_days)
      FALSE
    }
  }

  # Checks for new updates -------------------------------------------------

  if (file.exists('installed_version.txt')) {
    updates <- GET(url = 'https://gaftp.epa.gov/ecotox/') %>%
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
      ) %>%
      filter(latest == TRUE) %>%
      pull(date)

    installed <- read.table('installed_version.txt', header = TRUE) %>%
      filter(installed == TRUE) %>%
      mutate(
        date = lubridate::ymd(date)
      ) %>%
      pull(date)

    if (as.integer(difftime(updates, installed, units = "days")) > 0) {
      update_available <- TRUE
    } else {
      update_available <- FALSE
    }

    cli::cat_line()

    if (update_available) {
      rebuild_is_needed <- usethis::ui_yeah(
        'Update available! Do you want to update?'
      )
    }

    rm(installed, updates, update_available)
  }
}


# Rebuild ----------------------------------------------------------------

# If a rebuild is needed, run the data scraping and processing sections.
if (rebuild_is_needed) {
  unlink('ecotox.duckdb', recursive = TRUE, force = TRUE)
  unlink('installed_version.txt')

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

    ecotox_appendix <- import_list(
      file = 'ecotox_terms_appendix.xlsx',
      skip = 2
    )

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

  # Eco group --------------------------------------------------------------

  {
    # Create a dbplyr query object for the updated species table
    species_query <- tbl(eco_con, 'species') %>%
      mutate(
        eco_group = case_when(
          str_detect(family, 'Megachilidae|Apidae') ~ 'Bees',
          str_detect(ecotox_group, 'Insects/Spiders') ~ 'Insects/Spiders',
          str_detect(
            ecotox_group,
            'Flowers, Trees, Shrubs, Ferns'
          ) ~ 'Flowers, Trees, Shrubs, Ferns',
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
        standard_test_species = case_when(
          str_detect(ecotox_group, 'Standard') ~ TRUE,
          .default = FALSE
        ),

        invasive_species = case_when(
          str_detect(ecotox_group, 'Invasive') ~ TRUE,
          .default = FALSE
        ),
        endangered_threatened_species = case_when(
          str_detect(ecotox_group, 'Endangered') ~ TRUE,
          .default = FALSE
        )
      )

    # Render the dbplyr query to an SQL SELECT statement
    select_sql <- dbplyr::sql_render(species_query)

    # Construct the SQL statement to replace the table with the new data.
    # This is more efficient than pulling data into R and writing it back.
    overwrite_sql <- paste0("CREATE OR REPLACE TABLE species AS ", select_sql)

    # Execute the SQL command to overwrite the table in the database.
    dbExecute(eco_con, overwrite_sql)
  }
  # Dictionaries -----------------------------------------------------------

  ## Unit conversion --------------------------------------------------------
  {
    unit_result <-
      tibble::tribble(
        ~unit          , ~multiplier       , ~unit_conv          , ~type           ,
        "ag"           ,   1e-18           , "g"                 , "mass"          ,
        "fg"           ,   1e-15           , "g"                 , "mass"          ,
        "pg"           ,   1e-12           , "g"                 , "mass"          ,
        "ng"           ,   1e-09           , "g"                 , "mass"          ,
        "ug"           ,   1e-06           , "g"                 , "mass"          ,
        "mg"           ,       0.001       , "g"                 , "mass"          ,
        "g"            ,       1           , "g"                 , "mass"          ,
        "kg"           ,    1000           , "g"                 , "mass"          ,
        "kg N"         ,       1           , "kg N"              , "mass"          ,
        "t"            ,   1e+06           , "g"                 , "mass"          ,
        "ton"          ,   1e+06           , "g"                 , "mass"          ,
        "tons"         ,   1e+06           , "g"                 , "mass"          ,
        "quintal"      ,   1e+05           , "g"                 , "mass"          ,
        "q"            ,   1e+05           , "g"                 , "mass"          ,
        "pl"           ,   1e-12           , "l"                 , "volume"        ,
        "nl"           ,   1e-09           , "l"                 , "volume"        ,
        "ul"           ,   1e-06           , "l"                 , "volume"        ,
        "ml"           ,       0.001       , "l"                 , "volume"        ,
        "dl"           ,       0.1         , "l"                 , "volume"        ,
        "l"            ,       1           , "l"                 , "volume"        ,
        "lit"          ,       1           , "l"                 , "volume"        ,
        "hl"           ,     100           , "l"                 , "volume"        ,
        "pL"           ,   1e-12           , "l"                 , "volume"        ,
        "nL"           ,   1e-09           , "l"                 , "volume"        ,
        "uL"           ,   1e-06           , "l"                 , "volume"        ,
        "mL"           ,       0.001       , "l"                 , "volume"        ,
        "dL"           ,       0.1         , "l"                 , "volume"        ,
        "L"            ,       1           , "l"                 , "volume"        ,
        "hL"           ,     100           , "l"                 , "volume"        ,
        "bu"           ,      36.36872     , "l"                 , "volume"        ,
        "bushel"       ,      36.36872     , "l"                 , "volume"        ,
        "ppq"          ,   1e-06           , "ppb"               , "fraction"      ,
        "ppt"          ,       0.001       , "ppb"               , "fraction"      ,
        "ppb"          ,       1           , "ppb"               , "fraction"      ,
        "ppm"          ,    1000           , "ppb"               , "fraction"      ,
        "ppm-hour"     ,    1000           , "ppb/h"             , "fraction"      ,
        "ppm for 36hr" ,      27.77777778  , "ppb/h"             , "fraction"      ,
        "ppmv"         ,    1000           , "ppb"               , "fraction"      ,
        "ppmw"         ,    1000           , "ppb"               , "fraction"      ,
        "0/00"         ,   1e+06           , "ppb"               , "fraction"      ,
        "ptm"          ,       1           , "ppb"               , "fraction"      ,
        "no"           , NA                , NA                  , "amount"        ,
        "amol"         ,   1e-18           , "mol"               , "mol"           ,
        "fmol"         ,   1e-15           , "mol"               , "mol"           ,
        "pmol"         ,   1e-12           , "mol"               , "mol"           ,
        "nmol"         ,   1e-09           , "mol"               , "mol"           ,
        "umol"         ,   1e-06           , "mol"               , "mol"           ,
        "umoles"       ,   1e-06           , "mol"               , "mol"           ,
        "mumol"        ,   1e-09           , "mol"               , "mol"           ,
        "mmol"         ,       0.001       , "mol"               , "mol"           ,
        "cmol"         ,       0.01        , "mol"               , "mol"           ,
        "mol"          ,       1           , "mol"               , "mol"           ,
        "kmol"         ,    1000           , "mol"               , "mol"           ,
        "pM"           ,   1e-12           , "mol/l"             , "mol/volume"    ,
        "nM"           ,   1e-09           , "mol/l"             , "mol/volume"    ,
        "uM"           ,   1e-06           , "mol/l"             , "mol/volume"    ,
        "mM"           ,       0.001       , "mol/l"             , "mol/volume"    ,
        "M"            ,       1           , "mol/l"             , "mol/volume"    ,
        "molal"        ,       0.001       , "mol/g"             , "mol/mass"      ,
        "mOsm"         ,       0.001       , "Osm/l"             , "osmolarity"    ,
        "in"           ,       0.0254      , "m"                 , "length"        ,
        "yd"           ,       0.9144      , "m"                 , "length"        ,
        "ft"           ,       0.3048      , "m"                 , "length"        ,
        "linear ft"    ,       0.3048      , "m"                 , "length"        ,
        "rod"          ,       5.0292      , "m"                 , "length"        ,
        "um"           ,   1e-06           , "m"                 , "length"        ,
        "mm"           ,       0.001       , "m"                 , "length"        ,
        "cm"           ,       0.01        , "m"                 , "length"        ,
        "dm"           ,       0.1         , "m"                 , "length"        ,
        "m"            ,       1           , "m"                 , "length"        ,
        "km"           ,    1000           , "m"                 , "length"        ,
        "neq"          ,   1e-09           , "eq"                , "noscience"     ,
        "meq"          ,       0.001       , "eq"                , "noscience"     ,
        "ueq"          ,   1e-06           , "eq"                , "noscience"     ,
        "mm2"          ,   1e-06           , "m2"                , "area"          ,
        "cm2"          ,   1e-04           , "m2"                , "area"          ,
        "dm2"          ,       0.01        , "m2"                , "area"          ,
        "hm2"          ,   10000           , "m2"                , "area"          ,
        "m2"           ,       1           , "m2"                , "area"          ,
        "yd2"          ,       0.836127    , "m2"                , "area"          ,
        "ha"           ,   10000           , "m2"                , "area"          ,
        "hectare"      ,   10000           , "m2"                , "area"          ,
        "acre"         ,    4046.873       , "m2"                , "area"          ,
        "acres"        ,    4046.873       , "m2"                , "area"          ,
        "ac"           ,    4046.873       , "m2"                , "area"          ,
        "rod2"         ,      25.2929      , "m2"                , "area"          ,
        "mi2"          , 2589988.11        , "m2"                , "area"          ,
        "km2"          ,   1e+06           , "m2"                , "area"          ,
        "ft2"          ,       0.0929      , "m2"                , "area"          ,
        "sqft"         ,       0.0929      , "m2"                , "area"          ,
        "k sqft"       ,      92.9         , "m2"                , "area"          ,
        "feddan"       ,    4200           , "m2"                , "area"          ,
        "dn(Cyprus)"   ,    1338           , "m2"                , "area"          ,
        "dn(Std)"      ,    1000           , "m2"                , "area"          ,
        "%"            ,   1e+07           , "ppb"               , "fraction"      ,
        "â€°"      ,   1e+06           , "ppb"               , "fraction"      ,
        "d"            ,      24           , "h"                 , "time"          ,
        "day"          ,      24           , "h"                 , "time"          ,
        "h"            ,       1           , "h"                 , "time"          ,
        "hr"           ,       1           , "h"                 , "time"          ,
        "hour"         ,       1           , "h"                 , "time"          ,
        "mi"           ,       0.0166667   , "h"                 , "time"          ,
        "min"          ,       0.0166667   , "h"                 , "time"          ,
        "wk"           ,     168           , "h"                 , "time"          ,
        "mo"           ,     730           , "h"                 , "time"          ,
        "yr"           ,    8760           , "h"                 , "time"          ,
        "ft3"          ,       0.02831658  , "m3"                , "volume"        ,
        "mm3"          ,   1e-09           , "m3"                , "volume"        ,
        "cm3"          ,   1e-06           , "m3"                , "volume"        ,
        "dm3"          ,       0.001       , "m3"                , "volume"        ,
        "m3"           ,       1           , "m3"                , "volume"        ,
        "fl_oz"        ,       0.02957353  , "l"                 , "volume"        ,
        "pt"           ,       0.473176473 , "l"                 , "volume"        ,
        "oz"           ,      28.34952313  , "g"                 , "mass"          ,
        "gal"          ,       3.785411784 , "l"                 , "volume"        ,
        "ga"           ,       3.785411784 , "l"                 , "volume"        ,
        "qt"           ,       0.946352946 , "l"                 , "volume"        ,
        "lb"           ,     453.592       , "g"                 , "mass"          ,
        "lbs"          ,     453.592       , "g"                 , "mass"          ,
        "PSU"          ,       1           , "PSU"               , "noscience"     ,
        "--"           , NA                , NA                  , "nodata"        ,
        "NR"           , NA                , NA                  , "nodata"        ,
        "eu"           ,       1           , "eu"                , "noscience"     ,
        "EU"           ,       1           , "eu"                , "noscience"     ,
        "MBq"          ,   1e+06           , "Bq"                , "radioactivity" ,
        "kBq"          ,    1000           , "Bq"                , "radioactivity" ,
        "Bq"           ,       1           , "Bq"                , "radioactivity" ,
        "mBq"          ,       0.001       , "Bq"                , "radioactivity" ,
        "uBq"          ,   1e-06           , "Bq"                , "radioactivity" ,
        "Ci"           ,       3.7e+10     , "Bq"                , "radioactivity" ,
        "mCI"          ,       3.7e+07     , "Bq"                , "radioactivity" ,
        "mCi"          ,       3.7e+07     , "Bq"                , "radioactivity" ,
        "mCi mg"       , NA                , NA                  , "noscience"     ,
        "uCI"          ,   37000           , "Bq"                , "radioactivity" ,
        "uCi"          ,   37000           , "Bq"                , "radioactivity" ,
        "nCI"          ,      37           , "Bq"                , "radioactivity" ,
        "nCi"          ,      37           , "Bq"                , "radioactivity" ,
        "pCI"          ,       0.037       , "Bq"                , "radioactivity" ,
        "pCi"          ,       0.037       , "Bq"                , "radioactivity" ,
        "ICU"          ,       1           , "ICU"               , "noscience"     ,
        "USP"          ,       1           , "USP"               , "noscience"     ,
        "iu"           ,       1           , "iunit"             , "noscience"     ,
        "IU"           ,       1           , "iunit"             , "noscience"     ,
        "mIU"          , NA                , NA                  , NA              ,
        "fibers"       ,       1           , "fibers"            , "noscience"     ,
        "kJ"           ,    1000           , "J"                 , "energy"        ,
        "mS"           ,       0.001       , "S"                 , "electricity"   ,
        "dS"           ,       0.1         , "S"                 , "electricity"   ,
        "org"          ,       1           , "organism"          , "noscience"     ,
        "organi"       ,       1           , "organism"          , "noscience"     ,
        "v"            , NA                , NA                  , "volume"        ,
        "% v/v"        ,   1e+07           , "ppb"               , "fraction"      ,
        "cwt"          ,   45360           , "g"                 , "mass"          ,
        "w"            , NA                , NA                  , "noscience"     ,
        "% w/v"        ,   1e+07           , "ppb"               , "fraction"      ,
        "in dia"       , NA                , NA                  , "noscience"     ,
        "egg"          ,       1           , "egg"               , "noscience"     ,
        "pellets"      ,       1           , "pellets"           , "noscience"     ,
        "bee"          ,       1           , "bee"               , "noscience"     ,
        "fish"         ,       1           , "fish"              , "noscience"     ,
        "dpm"          ,       0.01666667  , "Bq"                , "radioactivity" ,
        "sd"           ,       1           , "seed"              , "noscience"     ,
        "seed"         ,       1           , "seed"              , "noscience"     ,
        "seeds"        ,       1           , "seed"              , "noscience"     ,
        "cntr"         ,       1           , "container"         , "noscience"     ,
        "plot"         ,       1           , "plot"              , "noscience"     ,
        "cpm"          ,       1           , "cpm"               , "noscience"     ,
        "mound"        ,       1           , "mound"             , "noscience"     ,
        "mouse unit"   ,       1           , "mouse unit"        , "noscience"     ,
        "disk"         ,       1           , "disk"              , "noscience"     ,
        "cc"           ,       1           , "cocoon"            , "noscience"     ,
        "cell"         ,       1           , "cell"              , "noscience"     ,
        "dose"         ,       1           , "dose"              , "noscience"     ,
        "em"           ,       1           , "embryo"            , "noscience"     ,
        "granules"     ,       1           , "granule"           , "noscience"     ,
        "lf"           ,       1           , "leaf"              , "noscience"     ,
        "tank"         ,       1           , "tank"              , "noscience"     ,
        "tbsp"         ,       1           , "tablespoon"        , "noscience"     ,
        "Tbsp"         ,       1           , "tablespoon"        , "noscience"     ,
        "tsp"          ,       1           , "teaspoon"          , "noscience"     ,
        "u"            ,       1           , "unit"              , "noscience"     ,
        "U"            ,       1           , "unit"              , "noscience"     ,
        "unit"         ,       1           , "unit"              , "noscience"     ,
        "units"        ,       1           , "unit"              , "noscience"     ,
        "U of fl"      ,       1           , "unit fluorescence" , "noscience"     ,
        "ML"           ,       1           , "male"              , "noscience"     ,
        "N"            ,       1           , "Normal"            , "noscience"     ,
        "RA"           ,       1           , "ratio"             , "noscience"     ,
        "ug-atoms"     ,       1           , "ug-atoms"          , "noscience"     ,
        "u-atoms"      ,       1           , "u-atoms"           , "noscience"     ,
        "PIg"          ,       1           , "PIg"               , "noscience"     ,
        "g d"          ,       1           , NA                  , "noscience"     ,
        "ng eq"        ,       1           , NA                  , "noscience"     ,
        "6 in pots"    ,       1           , "6_in pot"          , "noscience"
      )

    dbWriteTable(eco_con, 'app_unit_conversion', unit_result, overwrite = TRUE)

    rm(unit_result)
  }

  ## Unit symbol ------------------------------------------------------------
  {
    unit_symbols <-
      tibble::tribble(
        ~symbol    , ~name                                  ,
        "CEC"      , "soil.cation.exchange"                 ,
        "DT"       , "digestivetract"                       ,
        "100% O2"  , "100%O2"                               ,
        "H2O"      , "water"                                ,
        "TI"       , "tissue"                               ,
        "ae"       , "acidequivalents"                      ,
        "agar"     , "agar"                                 ,
        "ai"       , "activeingredient"                     ,
        "bdwt"     , "bodyweight"                           ,
        "blood"    , "blood"                                ,
        "bt"       , "bait"                                 ,
        "body wt"  , "bodyweight"                           ,
        "bw"       , "bodyweight"                           ,
        "bwt"      , "bodyweight"                           ,
        "caliper"  , "caliper"                              ,
        "circ"     , "circular"                             ,
        'canopy'   , 'canopy'                               ,
        "dbh"      , "diameterbreastheight"                 ,
        "dia"      , "diameter"                             ,
        "diet"     , "diet"                                 ,
        "disk"     , "disk"                                 ,
        "dry wght" , "dryweight"                            ,
        "dw"       , "dry weight"                           ,
        "dry"      , "dry"                                  ,
        "dry_diet" , "drydiet"                              ,
        #"egg","egg",
        "eu"       , "experimentalunit"                     ,
        "fd"       , "food"                                 ,
        #"fish","fish",
        "food"     , "food"                                 ,
        "humus"    , "humus"                                ,
        "ht"       , 'plant height'                         ,
        "ld"       , "lipid"                                ,
        "lipid"    , "lipid"                                ,
        "litter"   , "litter"                               ,
        "linear"   , 'linear'                               ,
        "mat"      , "material"                             ,
        "media"    , "media"                                ,
        "om"       , "organicmatter"                        ,
        "org"      , "organism"                             ,
        "pair"     , "pair"                                 ,
        "pellet"   , "pellet"                               ,
        "plt"      , "pellet"                               ,
        'pro'      , 'protein'                              ,
        'protein'  , 'protein'                              ,
        #"sd","seed",
        #"seed","seed",
        "soil"     , "soil"                                 ,
        "solv"     , 'solvent'                              ,
        "solvent"  , "solvent"                              ,
        'soln'     , 'solution'                             ,
        "tubers"   , "tubers"                               ,
        "tkdi"     , 'trunk diameter at 1.5 m above ground' ,
        "wet wght" , "wetweight"                            ,
        "wet_bdwt" , "wetbodyweight"                        ,
        "wet"      , "wet"                                  ,
        "wet wt"   , "wetweight"                            ,
        "wt"       , "wet"                                  ,
        'wght'     , 'weight'
      ) %>%
      bind_rows(mutate(., symbol = toupper(symbol)))

    dbWriteTable(eco_con, 'unit_symbols', unit_symbols, overwrite = TRUE)

    rm(unit_symbols)
  }

  ## Duration conversion ----------------------------------------------------
  {
    duration_conversion <- tbl(eco_con, 'duration_unit_codes') %>%
      mutate(
        base_unit = case_when(
          str_detect(tolower(description), "minute") ~ "minutes",
          str_detect(tolower(description), "second") ~ "seconds",
          str_detect(tolower(description), "hour") ~ "hours",
          str_detect(tolower(description), "day") ~ "days",
          str_detect(tolower(description), "week") ~ "weeks",
          str_detect(tolower(description), "month") ~ "months",
          str_detect(tolower(description), "year") ~ "years",
          .default = NA
        ),
        # Converts to hours
        conversion_factor_duration = case_when(
          str_detect(tolower(description), "minute") ~ 1 / 60,
          str_detect(tolower(description), "second") ~ 1 / 3600,
          str_detect(tolower(description), "hour") ~ 1,
          str_detect(tolower(description), "day") ~ 24,
          str_detect(tolower(description), "week") ~ 24 * 7,
          # Average month
          str_detect(tolower(description), "month") ~ 24 * 30.43685,
          .default = 1
        ),
        cur_unit_duration = case_when(
          !is.na(base_unit) ~ 'h',
          .default = code
        )
      ) %>%
      collect()

    dbWriteTable(eco_con, 'duration_conversion', duration_conversion, overwrite = TRUE)

    rm(duration_conversion)
  }

  ## Test-Result Duration dictionary  ------------------------------------------------------------------
  {
    #fmt: table
    test_result_duration_dictionary <- tribble(
      # ! table returns test type based upon eco_group, effect, exposure_group, unit, endpoint and duration in hours
      ~eco_group                             , ~test_type , ~effect               , ~exposure_group , ~unit                        , ~endpoint                    , ~duration                            ,
      # Mammals
      "Mammals"                              , "acute"    , "MOR"                 , c("ORAL", NA)   , c("mg/kg", "mg/kg bdwt")     , "LD50"                       , NULL                                 ,
      "Mammals"                              , "chronic"  , "MOR"                 , c("ORAL", NA)   , "mg/kg/d"                    , c("NOEL", "NR-ZERO")         , NULL                                 ,
      # Birds, Amphibians, Reptiles
      c("Birds", "Amphibians", "Reptiles")   , "acute"    , "MOR"                 , c("ORAL", NA)   , "mg/kg"                      , "LD50"                       , NULL                                 ,
      c("Birds", "Amphibians", "Reptiles")   , "chronic"  , "MOR"                 , c("ORAL", NA)   , c("mg/kg/d", "mg/kg bdwt/d") , c("NOEL", "NR-ZERO")         , NULL                                 ,
      # Fish
      "Fish"                                 , "acute"    , "MOR"                 , NULL            , "mg/L"                       , c("LD50", "EC50", "LC50")    , expr(new_dur == 96)                  ,
      "Fish"                                 , "chronic"  , "MOR"                 , NULL            , "mg/L"                       , c("LD50", "EC50", "LC50")    , expr(new_dur >= 144)                 ,
      "Fish"                                 , "chronic"  , "MOR"                 , NULL            , "mg/L"                       , c("NOEC", "NOEL", "NR-ZERO") , expr(new_dur == 504)                 ,
      # Bees
      "Bees"                                 , "acute"    , "MOR"                 , NULL            , "ug/bee"                     , c("LD50", "LC50")            , expr(new_dur %in% c(24, 28, 72))     ,
      "Bees"                                 , "chronic"  , "MOR"                 , NULL            , "ug/bee"                     , c("LD50", "LC50")            , expr(new_dur == 240)                 ,
      # Insects/Spiders
      "Insects/Spiders"                      , "acute"    , "MOR"                 , NULL            , c("mg/L", "mg/kg")           , c("LD50", "LC50", "EC50")    , expr(new_dur %in% c(24, 48, 72))     ,
      "Insects/Spiders"                      , "chronic"  , "MOR"                 , NULL            , c("mg/L", "mg/kg")           , c("NOEL", "NOEC", "NR-ZERO") , expr(new_dur %in% c(504, 672))       ,
      # Invertebrates, Molluscs
      c("Invertebrates", "Molluscs")         , "acute"    , "MOR"                 , NULL            , c("mg/L", "mg/kg")           , c("LD50", "LC50", "EC50")    , expr(new_dur %in% c(24, 48, 72, 96)) ,
      c("Invertebrates", "Molluscs")         , "chronic"  , "MOR"                 , NULL            , c("mg/L", "mg/kg")           , c("LD50", "LC50", "EC50")    , expr(new_dur %in% c(504, 672))       ,
      # Worms
      "Worms"                                , "acute"    , "MOR"                 , NULL            , "mg/kg"                      , c("LD50", "LC50", "EC50")    , expr(new_dur == 336)                 ,
      "Worms"                                , "chronic"  , "MOR"                 , NULL            , "mg/kg"                      , c("NOEC", "NOEL", "NR-ZERO") , expr(new_dur <= 336)                 ,
      # Crustaceans
      "Crustaceans"                          , "acute"    , "MOR"                 , NULL            , "mg/L"                       , c("LD50", "LC50", "EC50")    , expr(new_dur <= 96)                  ,
      "Crustaceans"                          , "chronic"  , "MOR"                 , NULL            , "mg/L"                       , c("NOEC", "NOEL", "NR-ZERO") , expr(new_dur >= 672)                 ,
      # Algae, Fungi, Moss, Hornworts
      c("Algae", "Fungi", "Moss, Hornworts") , "acute"    , NULL                  , NULL            , "mg/L"                       , c("LD50", "LC50", "EC50")    , expr(new_dur <= 24 * 7)              ,
      c("Algae", "Fungi", "Moss, Hornworts") , "chronic"  , NULL                  , NULL            , "mg/L"                       , c("NOEC", "NOEL", "NR-ZERO") , expr(new_dur == 96)                  ,
      # Flowers, Trees, Shrubs, Ferns
      "Flowers, Trees, Shrubs, Ferns"        , "acute"    , NULL                  , NULL            , "mg/L"                       , c("LD50", "LC50", "EC50")    , expr(new_dur <= 7 * 24)              ,
      "Flowers, Trees, Shrubs, Ferns"        , "chronic"  , expr(effect != "MOR") , NULL            , NULL                         , c("NOEC", "NOEL", "NR-ZERO") , NULL
    ) %>% 
      # Serialize list and expression columns to character strings for DB storage
      # First, explicitly deparse any calls or expressions into character strings
      mutate(across(
      where(is.list),
      ~ map_chr(.x, function(item) {
        if (is.null(item)) {
          NA_character_
        } else if (is.call(item) || is.expression(item) || is.symbol(item)) {
          deparse(item)
        } else {
          paste(na.omit(item), collapse = "|")
        }
      })
    ))

    dbWriteTable(eco_con, 'dict_test_result_duration', test_result_duration_dictionary, overwrite = TRUE)

    rm(test_result_duration_dictionary)
  }

  ## Risk-Score dictionary ----------------------------------------------------------------------

  {
    #fmt: table
    risk_binning_rules <- tribble(
      ~eco_group      , ~test_type , ~bin , ~lower_bound , ~upper_bound ,
      # Mammals
      "Mammals"       , "acute"    , "VH" , -Inf         ,   10         ,
      "Mammals"       , "acute"    , "H"  ,   10         ,   50         ,
      "Mammals"       , "acute"    , "M"  ,   50         ,  500         ,
      "Mammals"       , "acute"    , "L"  ,  500         , 2000         ,
      "Mammals"       , "acute"    , "XL" , 2000         , Inf          ,
      "Mammals"       , "chronic"  , "VH" , -Inf         ,    1         ,
      "Mammals"       , "chronic"  , "H"  ,    1         ,   10         ,
      "Mammals"       , "chronic"  , "M"  ,   10         ,  200         ,
      "Mammals"       , "chronic"  , "L"  ,  200         , 1000         ,
      "Mammals"       , "chronic"  , "XL" , 1000         , Inf          ,
      # Birds
      "Birds"         , "acute"    , "VH" , -Inf         ,   10         ,
      "Birds"         , "acute"    , "H"  ,   10         ,   50         ,
      "Birds"         , "acute"    , "M"  ,   50         ,  500         ,
      "Birds"         , "acute"    , "L"  ,  500         , 2000         ,
      "Birds"         , "acute"    , "XL" , 2000         , Inf          ,
      "Birds"         , "chronic"  , "VH" , -Inf         ,    1         ,
      "Birds"         , "chronic"  , "H"  ,    1         ,   10         ,
      "Birds"         , "chronic"  , "M"  ,   10         ,  200         ,
      "Birds"         , "chronic"  , "L"  ,  200         , 1000         ,
      "Birds"         , "chronic"  , "XL" , 1000         , Inf          ,
      # Fish
      "Fish"          , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Fish"          , "acute"    , "H"  ,    0.1       ,    1         ,
      "Fish"          , "acute"    , "M"  ,    1         ,   10         ,
      "Fish"          , "acute"    , "L"  ,   10         ,  100         ,
      "Fish"          , "acute"    , "XL" ,  100         , Inf          ,
      "Fish"          , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Fish"          , "chronic"  , "H"  ,    1         ,   10         ,
      "Fish"          , "chronic"  , "M"  ,   10         ,  200         ,
      "Fish"          , "chronic"  , "L"  ,  200         , 1000         ,
      "Fish"          , "chronic"  , "XL" , 1000         , Inf          ,
      # Bees
      "Bees"          , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Bees"          , "acute"    , "H"  ,    0.1       ,    1         ,
      "Bees"          , "acute"    , "M"  ,    1         ,   10         ,
      "Bees"          , "acute"    , "L"  ,   10         ,  100         ,
      "Bees"          , "acute"    , "XL" ,  100         , Inf          ,
      "Bees"          , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Bees"          , "chronic"  , "H"  ,    1         ,   10         ,
      "Bees"          , "chronic"  , "M"  ,   10         ,  200         ,
      "Bees"          , "chronic"  , "L"  ,  200         , 1000         ,
      "Bees"          , "chronic"  , "XL" , 1000         , Inf          ,
      # Insects
      "Insects"       , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Insects"       , "acute"    , "H"  ,    0.1       ,    1         ,
      "Insects"       , "acute"    , "M"  ,    1         ,   10         ,
      "Insects"       , "acute"    , "L"  ,   10         ,  100         ,
      "Insects"       , "acute"    , "XL" ,  100         , Inf          ,
      "Insects"       , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Insects"       , "chronic"  , "H"  ,    1         ,   10         ,
      "Insects"       , "chronic"  , "M"  ,   10         ,  200         ,
      "Insects"       , "chronic"  , "L"  ,  200         , 1000         ,
      "Insects"       , "chronic"  , "XL" , 1000         , Inf          ,
      # Invertebrates
      "Invertebrates" , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Invertebrates" , "acute"    , "H"  ,    0.1       ,    1         ,
      "Invertebrates" , "acute"    , "M"  ,    1         ,   10         ,
      "Invertebrates" , "acute"    , "L"  ,   10         ,  100         ,
      "Invertebrates" , "acute"    , "XL" ,  100         , Inf          ,
      "Invertebrates" , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Invertebrates" , "chronic"  , "H"  ,    1         ,   10         ,
      "Invertebrates" , "chronic"  , "M"  ,   10         ,  200         ,
      "Invertebrates" , "chronic"  , "L"  ,  200         , 1000         ,
      "Invertebrates" , "chronic"  , "XL" , 1000         , Inf          ,
      # Worms
      "Worms"         , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Worms"         , "acute"    , "H"  ,    0.1       ,    1         ,
      "Worms"         , "acute"    , "M"  ,    1         ,   10         ,
      "Worms"         , "acute"    , "L"  ,   10         ,  100         ,
      "Worms"         , "acute"    , "XL" ,  100         , Inf          ,
      "Worms"         , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Worms"         , "chronic"  , "H"  ,    1         ,   10         ,
      "Worms"         , "chronic"  , "M"  ,   10         ,  200         ,
      "Worms"         , "chronic"  , "L"  ,  200         , 1000         ,
      "Worms"         , "chronic"  , "XL" , 1000         , Inf          ,
      # Crustaceans
      "Crustaceans"   , "acute"    , "VH" , -Inf         ,    0.1       ,
      "Crustaceans"   , "acute"    , "H"  ,    0.1       ,    1         ,
      "Crustaceans"   , "acute"    , "M"  ,    1         ,   10         ,
      "Crustaceans"   , "acute"    , "L"  ,   10         ,  100         ,
      "Crustaceans"   , "acute"    , "XL" ,  100         , Inf          ,
      "Crustaceans"   , "chronic"  , "VH" , -Inf         ,    0.01      ,
      "Crustaceans"   , "chronic"  , "H"  ,    1         ,   10         ,
      "Crustaceans"   , "chronic"  , "M"  ,   10         ,  200         ,
      "Crustaceans"   , "chronic"  , "L"  ,  200         , 1000         ,
      "Crustaceans"   , "chronic"  , "XL" , 1000         , Inf
    )

    dbWriteTable(eco_con, 'dict_risk_binning', risk_binning_rules, overwrite = TRUE)

    rm(risk_binning_rules)
  }

  # Full Unit conversion tables --------------------------------------------

  {
    units_intermediate <- tbl(eco_con, 'results') %>%
      select(orig = conc1_unit, test_id) %>%
      inner_join(
        .,
        tbl(eco_con, 'tests') %>%
          select(
            test_id,
            test_cas,
            species_number,
            reference_number,
            organism_habitat
          ),
        join_by('test_id')
      ) %>%
      inner_join(
        .,
        tbl(eco_con, 'references') %>%
          select(reference_number, publication_year),
        join_by('reference_number')
      ) %>%
      group_by(orig, organism_habitat) %>%
      summarize(
        n = n(),
        cas_n = n_distinct(test_cas),
        species_n = n_distinct(species_number),
        ref_n = n_distinct(reference_number),
        date_n = n_distinct(publication_year),
        ref_date = max(
          sql("TRY_CAST(REPLACE(publication_year, 'xx', '15') AS NUMERIC)"),
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      arrange(
        desc(n),
        desc(cas_n),
        desc(species_n),
        desc(ref_n),
        #	desc(ref_date)
      ) %>%
      # ! NOTE: Removes infrequent units ----
      #filter(n <= 2 & ref_n <= 2) %>%
      #filter(!is.na(orig) & n > 1 & ref_n > 1) %>%
      collect() %>%
      select(
        #	-n,
        -cas_n,
        species_n,
        ref_n,
        -date_n,
        -ref_date,
      ) %>%
      mutate(
        idx = 1:n(),
        # One-off injections
        raw = str_replace_all(
          orig,
          c(
            "1k" = "1000",
            'mgdrydiet' = 'mg dry_diet',
            'gwetbdwt' = 'g wet_bdwt',
            '6 in pots' = '6inpots',
            'u-atoms' = 'u_atoms',
            'ug-atoms' = 'ug_atoms',
            "0/00" = "ppt",
            '\\bppmw\\b' = 'ppm',
            '\\bppmv\\b' = 'ppm',
            '\\bppm w/w\\b' = 'ppm',
            '\\bml\\b' = 'mL',
            '\\bul\\b' = 'uL',
            '\\bof\\b' = "",
            '\\bmi\\b' = 'min',
            ' for ' = "/",
            'fl oz' = 'fl_oz',
            "ppt v/v" = 'mL/L',
            'ppm w/v' = 'mg/L',
            "-" = "/"
          )
        ) %>%
          str_squish(),
        raw = str_replace_all(
          raw,
          {
            # Create a regex pattern for whole-word matching of symbols.
            # Symbols are sorted by length (desc) to prioritize longer matches
            # (e.g., 'mg/L' over 'g').
            # Lookarounds '(?<!\w)' and '(?!\w)' ensure that symbols are not
            # part of other words.
            # 'str_escape' is used to handle special characters in symbols.
            tbl(eco_con, 'unit_symbols') %>%
              collect() %>%
              arrange(-stringr::str_length(symbol)) %>%
              pull(symbol) %>%
              stringr::str_escape() %>%
              paste0("(?<!\\w)", ., "(?!\\w)") %>%
              stringr::str_flatten(., "|")
          },
          replacement = ""
        ) %>%
          str_squish() %>%
          # NEW: Add a space between numbers and letters where it is missing.
          # e.g., "25kg" -> "25 kg"
          str_replace_all(
            .,
            pattern = "(?<=\\d)(?=[a-zA-Z])",
            replacement = " "
          ) %>%
          str_replace_all(
            .,
            c(
              "/ " = "/",
              '% ' = '%_'
            )
          ) %>%
          # The regex replaces a space with an underscore if it is preceded by a number
          # and followed by either a letter or another number.
          # e.g., "100 g" -> "100_g"
          # e.g., "100 2" -> "100_2"
          str_replace_all(
            .,
            pattern = "(\\b\\d*\\.?\\d+) (?=[[:alpha:]]|\\d)",
            replacement = "\\1_"
          ) %>%
          str_replace_all(
            .,
            c(
              " in " = "",
              " in" = "",
              ' ' = "/",
              "//" = "/",
              "%_v/v" = "%_v_v",
              "%_w/v" = "%_w_v",
              "%_w/w" = "%_w_w",
              "%_g/g" = "%_w_w",
              '6inpots' = '6 in pots'
            )
          ) %>%
          str_squish() %>%
          str_remove(., "/$"),

        has_number = str_detect(raw, pattern = '/\\d+'),
        suffix = str_extract_all(
          orig,
          {
            # Create a regex pattern for whole-word matching of symbols.
            # This is the same robust pattern generation used for the `raw` column
            # to ensure only full symbols are matched.
            tbl(eco_con, 'unit_symbols') %>%
              collect() %>%
              arrange(-stringr::str_length(symbol)) %>%
              pull(symbol) %>%
              stringr::str_escape() %>%
              paste0("(?<!\\w)", ., "(?!\\w)") %>%
              stringr::str_flatten(., "|")
          }
        ),
        suffix = map_chr(suffix, ~ paste(.x, collapse = " ")),
        u = raw
      ) %>%
      separate_wider_delim(
        u,
        delim = "/",
        names_sep = "_",
        too_few = 'align_start'
      ) %>%
      mutate(
        across(dplyr::starts_with('u'), ~ na_if(.x, "")),
        part_counts = rowSums(!is.na(select(., dplyr::starts_with('u'))))
      ) %>%
      relocate(part_counts, .after = has_number) %>%
      pivot_longer(
        .,
        cols = dplyr::starts_with('u'),
        names_to = 'name'
      ) %>%
      mutate(
        value = case_when(
          value == "%_" ~ "%",
          value == "%_v_v" ~ "% v/v",
          value == "%_w_v" ~ "% w/v",
          value == "%_w_w" ~ "% w/v",
          value == 'u_atoms' ~ 'u-atoms',
          value == 'ug_atoms' ~ 'ug-atoms',
          .default = value
        ),
        num_mod = str_extract(value, pattern = "\\b\\d*\\.?\\d+_") %>%
          str_remove_all(., pattern = "_") %>%
          as.numeric(),
        value = str_remove_all(value, pattern = "\\b\\d*\\.?\\d+_"),
        value = str_squish(value)
      ) %>%
      # ! Join against dictionary, update here----
      left_join(
        .,
        unit_result,
        join_by(value == unit)
      ) %>%
      pivot_wider(
        .,
        names_from = name,
        values_from = value:type
      ) %>%
      # Replace NA with 1 in num_mod and multiplier columns
      mutate(across(matches("^(num_mod|mult)"), ~ if_else(is.na(.x), 1, .x))) %>%
      # Dynamically calculate the conversion factor
      rowwise() %>%
      mutate(
        # The `conversion` is calculated for each unit string (row). It assumes
        # the unit is a ratio (e.g., mg/L, g/ha/day), where the first part is
        # the numerator and subsequent parts form the denominator.

        # Calculate the numerator's conversion value. This is the conversion
        # multiplier of the first unit part (e.g., 'mg' in 'mg/L') adjusted
        # by any numerical modifier extracted from the unit string (e.g., '10' in '10g').
        numer = c_across(starts_with("multiplier_u_"))[1] *
          c_across(starts_with("num_mod_u_"))[1],

        # Calculate the conversion values for all denominator parts. This includes
        # all unit parts after the first one (e.g., 'L' in 'mg/L'), each also
        # adjusted by any numerical modifiers.
        denoms = list(
          c_across(starts_with("multiplier_u_"))[-1] *
            c_across(starts_with("num_mod_u_"))[-1]
        ),

        conversion_factor = {
          #   # If the unit has denominator parts, divide the numerator's value by the
          #   # product of all denominator values. 'purrr::reduce' is used to multiply
          #   # all denominator components together. If there is no denominator, the
          #   # final conversion factor is simply the numerator's value.
          if (length(denoms) > 0) {
            numer / purrr::reduce(denoms, `*`)
          } else {
            numer
          }
        }
      ) %>%
      ungroup() %>%
      # Dynamically create cur_unit and cur_unit_type
      unite("cur_unit", starts_with("unit_conv_u_"), sep = "/", na.rm = TRUE) %>%
      unite("cur_unit_type", starts_with("type_u_"), sep = "/", na.rm = TRUE) %>%
      mutate(
        unit_domain = case_when(
          # --- Rule 1: Invalid or Uncategorized Units (Highest Priority) ---
          # Catch anything with "noscience" or empty strings first.
          str_detect(cur_unit_type, "noscience") | cur_unit_type == "" ~
            "Invalid / Uncategorized",

          # --- Rule 2: Dosing Rates (Amount / Normalization / Time) ---
          # These are the most specific, so they must come before simpler rates or concentrations.
          str_ends(cur_unit_type, "/time") & str_count(cur_unit_type, "/") == 2 ~
            "Dosing Rate",

          # --- Rule 3: Application Rates (Amount / Area) ---
          cur_unit_type %in% c("mass/area", "volume/area", "mol/area") ~
            "Application Rate",

          # --- Rule 4: Concentrations (Amount / Volume or Amount / Mass) ---
          # Liquid-based concentrations
          cur_unit_type %in% c("mass/volume", "mol/volume", "fraction/volume") ~
            "Concentration (Liquid)",
          # Matrix-based concentrations (e.g., in soil, tissue)
          cur_unit_type %in% c("mass/mass", "mol/mass", "volume/mass") ~
            "Concentration (Matrix)",

          # --- Rule 5: Simple Rates (Amount / Time) ---
          cur_unit_type %in% c("mass/time", "volume/time", "fraction/time") ~
            "Rate",

          # --- Rule 6: Dimensionless Ratios ---
          cur_unit_type %in% c("fraction", "volume/volume") ~ "Ratio / Fraction",

          # --- Rule 7: Radioactivity ---
          # Catches all variations like "radioactivity/volume", "radioactivity/mass", etc.
          str_starts(cur_unit_type, "radioactivity") ~ "Radioactivity",

          # --- Rule 8: Linear Density (Amount / Length) ---
          cur_unit_type %in% c("mass/length", "volume/length") ~ "Linear Density",

          # --- Rule 9: Simple Fundamental Quantities ---
          cur_unit_type == "mass" ~ "Mass",
          cur_unit_type == "volume" ~ "Volume",
          cur_unit_type == "mol" ~ "Amount (molar)",
          cur_unit_type == "length" ~ "Length",
          cur_unit_type == "time" ~ "Time",

          # --- Rule 10: Catch-all for Other Valid but Complex Types ---
          # This will group any remaining complex but valid units.
          TRUE ~ "Other Complex Unit"
        )
      )

    unit_conversion <- units_intermediate %>%
      select(
        orig,
        cur_unit_result = cur_unit,
        suffix,
        cur_unit_type,
        conversion_factor_unit = conversion_factor,
        unit_domain
      ) %>%
      distinct(orig, .keep_all = TRUE)

    dbWriteTable(eco_con, 'z_unit_intermediate', units_intermediate, overwrite = TRUE)
    dbWriteTable(eco_con, 'unit_conversion', unit_conversion, overwrite = TRUE)

    rm(units_intermediate, unit_conversion)
  }

  ## Life stage harmonization -----------------------------------------------
  # fmt: table
  {
    life_stage <- tribble(
      ~org_lifestage                                   , ~harmonized_life_stage ,
      'Unspecified'                                    , 'Other/Unknown'        ,
      'Adult'                                          , 'Adult'                ,
      'Alevin'                                         , 'Larva/Juvenile'       ,
      'Bud or Budding'                                 , 'Other/Unknown'        ,
      'Blastula'                                       , 'Egg/Embryo'           ,
      'Bud blast stage'                                , 'Adult'                ,
      'Boot'                                           , 'Adult'                ,
      'Cocoon'                                         , 'Adult'                ,
      'Corm'                                           , 'Adult'                ,
      'Copepodid'                                      , 'Larva/Juvenile'       ,
      'Copepodite'                                     , 'Larva/Juvenile'       ,
      'Cleavage stage'                                 , 'Egg/Embryo'           ,
      'Cyst'                                           , 'Dormant/Senescent'    ,
      'Egg'                                            , 'Egg/Embryo'           ,
      'Elver'                                          , 'Larva/Juvenile'       ,
      'Embryo'                                         , 'Egg/Embryo'           ,
      'Exponential growth phase (log)'                 , 'Other/Unknown'        ,
      'Eyed egg or stage, eyed embryo'                 , 'Egg/Embryo'           ,
      'F0 generation'                                  , 'Reproductive'         ,
      'F1 generation'                                  , 'Reproductive'         ,
      'F11 generation'                                 , 'Reproductive'         ,
      'F2 generation'                                  , 'Reproductive'         ,
      'F3 generation'                                  , 'Reproductive'         ,
      'F6 generation'                                  , 'Reproductive'         ,
      'F7 generation'                                  , 'Reproductive'         ,
      'Mature (full-bloom stage) organism'             , 'Adult'                ,
      'Female gametophyte'                             , 'Reproductive'         ,
      'Fingerling'                                     , 'Larva/Juvenile'       ,
      'Flower opening'                                 , 'Reproductive'         ,
      'Froglet'                                        , 'Larva/Juvenile'       ,
      'Fry'                                            , 'Larva/Juvenile'       ,
      'Gastrula'                                       , 'Egg/Embryo'           ,
      'Gestation'                                      , 'Reproductive'         ,
      'Glochidia'                                      , 'Larva/Juvenile'       ,
      'Gamete'                                         , 'Reproductive'         ,
      'Lag growth phase'                               , 'Other/Unknown'        ,
      'Grain or seed formation stage'                  , 'Adult'                ,
      'Germinated seed'                                , 'Dormant/Senescent'    ,
      'Heading'                                        , 'Adult'                ,
      'Incipient bud'                                  , 'Adult'                ,
      'Internode elongation'                           , 'Adult'                ,
      'Imago'                                          , 'Adult'                ,
      'Immature'                                       , 'Subadult/Immature'    ,
      'Instar'                                         , 'Larva/Juvenile'       ,
      'Intermolt'                                      , 'Other/Unknown'        ,
      'Jointing'                                       , 'Adult'                ,
      'Juvenile'                                       , 'Larva/Juvenile'       ,
      'Lactational'                                    , 'Reproductive'         ,
      'Egg laying'                                     , 'Reproductive'         ,
      'Larva-pupa'                                     , 'Larva/Juvenile'       ,
      'Prolarva'                                       , 'Larva/Juvenile'       ,
      'Larva'                                          , 'Larva/Juvenile'       ,
      'Mature'                                         , 'Adult'                ,
      'Mature dormant'                                 , 'Dormant/Senescent'    ,
      'Megalopa'                                       , 'Larva/Juvenile'       ,
      'Male gametophyte'                               , 'Reproductive'         ,
      'Morula'                                         , 'Egg/Embryo'           ,
      'Mid-neurula'                                    , 'Egg/Embryo'           ,
      'Molt'                                           , 'Other/Unknown'        ,
      'Multiple'                                       , 'Other/Unknown'        ,
      'Mysis'                                          , 'Larva/Juvenile'       ,
      'Newborn'                                        , 'Larva/Juvenile'       ,
      'Naiad'                                          , 'Larva/Juvenile'       ,
      'Neonate'                                        , 'Larva/Juvenile'       ,
      'New, newly or recent hatch'                     , 'Larva/Juvenile'       ,
      'Neurala'                                        , 'Egg/Embryo'           ,
      'Not intact'                                     , 'Other/Unknown'        ,
      'Not reported'                                   , 'Other/Unknown'        ,
      'Nauplii'                                        , 'Larva/Juvenile'       ,
      'Nymph'                                          , 'Larva/Juvenile'       ,
      'Oocyte, ova'                                    , 'Egg/Embryo'           ,
      'Parr'                                           , 'Larva/Juvenile'       ,
      'Mature, post-bloom stage (fruit trees)'         , 'Adult'                ,
      'Pre-hatch'                                      , 'Other/Unknown'        ,
      'Pre-molt'                                       , 'Other/Unknown'        ,
      'Post-emergence'                                 , 'Adult'                ,
      'Post-spawning'                                  , 'Reproductive'         ,
      'Mature, pit-hardening stage (fruit trees)'      , 'Adult'                ,
      'Post-hatch'                                     , 'Other/Unknown'        ,
      'Post-molt'                                      , 'Other/Unknown'        ,
      'Pre-, sub-, semi-, near adult, or peripubertal' , 'Subadult/Immature'    ,
      'Post-smolt'                                     , 'Larva/Juvenile'       ,
      'Pullet'                                         , 'Larva/Juvenile'       ,
      'Post-nauplius'                                  , 'Larva/Juvenile'       ,
      'Pollen, pollen grain'                           , 'Reproductive'         ,
      'Postpartum'                                     , 'Reproductive'         ,
      'Prepupal'                                       , 'Larva/Juvenile'       ,
      'Pre-larva'                                      , 'Larva/Juvenile'       ,
      'Prebloom'                                       , 'Reproductive'         ,
      'Pre-smolt'                                      , 'Larva/Juvenile'       ,
      'Protolarvae'                                    , 'Larva/Juvenile'       ,
      'Pupa'                                           , 'Larva/Juvenile'       ,
      'Post-larva'                                     , 'Larva/Juvenile'       ,
      'Pre-spawning'                                   , 'Reproductive'         ,
      'Post-embryo'                                    , 'Other/Unknown'        ,
      'Protozoea'                                      , 'Larva/Juvenile'       ,
      'Rooted cuttings'                                , 'Adult'                ,
      'Rhizome'                                        , 'Adult'                ,
      'Mature reproductive'                            , 'Reproductive'         ,
      'Rootstock'                                      , 'Other/Unknown'        ,
      'Subadult'                                       , 'Subadult/Immature'    ,
      'Shoot'                                          , 'Adult'                ,
      'Yolk sac larvae, sac larvae'                    , 'Larva/Juvenile'       ,
      'Senescence'                                     , 'Dormant/Senescent'    ,
      'Seed'                                           , 'Adult'                ,
      'Scape elongation'                               , 'Adult'                ,
      'Sac fry, yolk sac fry'                          , 'Larva/Juvenile'       ,
      'Mature, side-green stage (fruit trees)'         , 'Adult'                ,
      'Sexually immature'                              , 'Subadult/Immature'    ,
      'Seedling'                                       , 'Larva/Juvenile'       ,
      'Sexually mature'                                , 'Adult'                ,
      'Smolt'                                          , 'Larva/Juvenile'       ,
      'Sapling'                                        , 'Adult'                ,
      'Sporeling'                                      , 'Larva/Juvenile'       ,
      'Sperm'                                          , 'Reproductive'         ,
      'Spore'                                          , 'Other/Unknown'        ,
      'Spat'                                           , 'Adult'                ,
      'Swim-up'                                        , 'Larva/Juvenile'       ,
      'Spawning'                                       , 'Reproductive'         ,
      'Stationary growth phase'                        , 'Dormant/Senescent'    ,
      'Tadpole'                                        , 'Larva/Juvenile'       ,
      'Tissue culture callus'                          , 'Other/Unknown'        ,
      'Tiller stage'                                   , 'Adult'                ,
      'Tuber'                                          , 'Adult'                ,
      'Trophozoite'                                    , 'Other/Unknown'        ,
      'Underyearling'                                  , 'Larva/Juvenile'       ,
      'Veliger'                                        , 'Larva/Juvenile'       ,
      'Mature vegetative'                              , 'Other/Unknown'        ,
      'Virgin'                                         , 'Other/Unknown'        ,
      'Weanling'                                       , 'Larva/Juvenile'       ,
      'Young adult'                                    , 'Subadult/Immature'    ,
      'Yearling'                                       , 'Larva/Juvenile'       ,
      'Young'                                          , 'Larva/Juvenile'       ,
      'Young of year'                                  , 'Larva/Juvenile'       ,
      'Zoea'                                           , 'Larva/Juvenile'       ,
      'Zygospore'                                      , 'Egg/Embryo'           ,
      'Zygote'                                         , 'Egg/Embryo'
    )

    dbWriteTable(eco_con, 'lifestage_dictionary', life_stage)
  }

  ## Effects table super-group ----------------------------------------------

  {
    effect_conversion <-
      tbl(eco_con, 'app_effect_groups') %>%
      mutate(
        effect_group = str_sub(group_effect_term_s, 1, 3)
      ) %>%
      rename(
        term = group_effect_term_s,
        effect_description = description,
        effect_definition = definition
      ) %>%
      collect() %>%
      separate_longer_delim(cols = c(term, effect_description), delim = "/") %>%
      separate_longer_delim(cols = c(term, effect_description), delim = ",") %>%
      mutate(
        across(
          c(effect_description, term),
          ~ str_squish(.x)
        )
      ) %>%
      distinct() %>%
      arrange(effect_group)

    effect_conversion <- effect_conversion %>%
      inner_join(
        effect_conversion %>%
          select(term, super_effect_description = effect_description) %>%
          distinct(term, .keep_all = TRUE),
        by = join_by(
          effect_group == term
        )
      )

    dbWriteTable(eco_con, 'effect_groups_dictionary', effect_conversion, overwrite = TRUE)

    rm(effect_conversion)
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

    # Persist the in-memory database to a single file on disk.
    # This is an efficient way to transfer the contents of the in-memory database
    # (`:memory:`) to a persistent file (`ecotox.duckdb`).
    dbExecute(
      eco_con,
      "ATTACH 'ecotox.duckdb' AS ecotox;
       COPY FROM DATABASE memory TO ecotox;
       DETACH ecotox;"
    )

    tbl(eco_con, 'versions') %>%
      collect() %>%
      as_tibble() %>%
      rename(installed = latest) %>%
      write_tsv(file = 'installed_version.txt')

    dbDisconnect(eco_con)
    rm(eco_con)
  }

  rm(
    files_to_check,
    files_exist_check,
    file_ages_days,
    rebuild_is_needed,
    overwrite_sql,
    select_sql,
    species_query,
    life_stage
  )
}
# deploy ------------------------------------------------------------------

#source(here("ecotox", "plumber.R"))
if (deploy) {
  cli::cli_alert_success('Deploying API + Documentation')
  plumber::pr("plumber.R") %>% plumber::pr_run()

  rm(deploy)
} else {
  cli::cli_alert_success('Deploying local connection')
  eco_con <- dbConnect(duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)

  source("plumber.R")

  rm(deploy)
}

rm(rebuild_is_needed)

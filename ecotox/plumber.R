# packages ----------------------------------------------------------------
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
      'mirai',
      # 'targets',
      # 'crew',

      ## DB ----
      'arrow',
      'nanoparquet',
      'duckdb',
      'duckplyr',
      'dbplyr',

      ## EDA ----
      'skimr',

      ## Web ----
      'rvest',
      'plumber',
      #	'plumber2', #Still experimental
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
      'remotes'
    )

    # ! Change load flag to load packages
    install_booster_pack(package = booster_pack, load = TRUE)
    rm(install_booster_pack, booster_pack)
  }

  # Custom Functions ----

  `%ni%` <- Negate(`%in%`)

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
  # }

  # Options ----

  options("plumber.port" = 5555)

  setwd(here("ecotox"))
}
# Checkpoint -------------------------------------------------------------

# Determine if a rebuild is necessary.
{
  if (
    !all(
      file.exists(
        c("ecotox.duckdb")
      )
    )
  ) {
    cli::cli_abort("One or more data files are missing.")
  }
}

# functions ---------------------------------------------------------------

#' Convert Duration Units Flexibly
#'
#' @description
#' This function takes a data frame with duration values and their corresponding
#' units and converts them into a standardized 'duration' object using the
#' lubridate package. It then calculates the duration in a desired target unit.
#'
#' @param data A data frame containing the data to be processed.
#' @param value_column The name of the column (as a string) containing numeric
#'   duration values.
#' @param unit_column The name of the column (as a string) containing character
#'   unit strings (e.g., "days", "weeks", "hours").
#' @param output_unit The desired output unit for the duration (e.g., "hours", "minutes").
#'
#' @return A data frame with a new column `duration_obj` (a lubridate duration object)
#'   and `new_dur` (the numeric value in the specified `output_unit`).
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # Create a sample data frame
#' df <- tibble::tribble(
#'   ~time_val, ~time_unit,
#'   10,        "days",
#'   2,         "weeks",
#'   48,        "hours",
#'   3600,      "seconds"
#' )
#'
#' convert_duration(df, "time_val", "time_unit", output_unit = "hours")
#' convert_duration(df, "time_val", "time_unit", output_unit = "minutes")
#'
convert_duration <- function(
  data,
  value_column,
  unit_column,
  output_unit = "hours"
) {
  data %>%
    mutate(
      duration_obj = lubridate::duration(
        !!sym(value_column),
        units = !!sym(unit_column)
      ),
      new_dur = lubridate::as.duration(duration_obj) /
        lubridate::duration(1, output_unit),
      new_dur_unit = output_unit
    )
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

weighted_average <- function(values, weights) {
  sum(values * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

# documentation ----------------------------------------------------------

#* @apiTitle Testing API for EcoTox

#* Health Check
#* @get /health-check
health <- function() {
  db <- list.files(pattern = "\\.duckdb$")

  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = "ecotox.duckdb",
    read_only = FALSE
  )
  on.exit(dbDisconnect(con))

  list(
    # timestamp = Sys.time(),
    db = list.files(pattern = "\\.duckdb$"),
    created = tbl(con, "versions") %>%
      filter(latest == TRUE) %>%
      pull(date),
    db_size_mb = round(file.size(db) / (1024 * 1024), 2)
  )
}

#* Full list of chemicals currently in EcoTox
#* @get /inventory
inventory <- function() {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- tbl(con, 'chemicals') %>% collect()
  return(result)
}

#* Glimpse a table in the database
#* @get /glimpse/<table_name:str>
#* @param table_name:str
tbl_glimpse <- function(table) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- tbl(con, table) %>%
    head(n = 10) %>%
    # collect() will execute the query and pull the data into a local tibble.
    # This is necessary because the connection is closed when the function exits.
    collect() %>%
    glimpse()
}

#* Get a list of available tables within database
#* @get /all_tbls
all_tbls <- function() {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  DBI::dbListTables(con)
}

#* Retrieve column names for a given table
#* @get /fields/<table_name:str>
#* @param table_name:str
fields <- function(table_name) {
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = "ecotox.duckdb",
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con))

  result <- DBI::dbListFields(con, table_name)

  return(result)
}


#* Retrieve table from database by name
#* @get /tables/<table_name:str>
#* @param table_name:str
get_tbl <- function(table_name) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- tbl(con, table_name) %>% collect()
  return(result)
}


#* Retrieve data from database by CASRN, species, endpoint, or ecotox group
#* @param casrn A list of CASRNs to query
#* @param common_name A list of common species names to query
#* @param latin_name A list of latin species names to query
#* @param endpoint A list of endpoints to query
#* @param eco_group A list of ecotox groups to query
#* @param invasive boolean to filter by invasive species
#* @param standard boolean to filter by standard species
#* @param threatened boolean to filter by threatened species
#* @post /results
post_results <- function(
  casrn = NULL,
  common_name = NULL,
  latin_name = NULL,
  endpoint = NULL,
  eco_group = NULL,
  invasive = FALSE,
  standard = FALSE,
  threatened = FALSE,
  test_cols = NULL,
  results_cols = NULL
) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  # At least one query parameter must be provided to avoid returning the whole database
  if (
    is.null(casrn) &&
      is.null(common_name) &&
      is.null(latin_name) &&
      is.null(endpoint) &&
      is.null(eco_group) &&
      !invasive &&
      !standard &&
      !threatened
  ) {
    cli::cli_abort(
      "At least one query parameter (casrn, common_name, latin_name, endpoint, ecotox_group, invasive, standard, threatened) must be provided."
    )
  }

  # Base tables ----
  tests_tbl <- tbl(con, "tests")
  species_tbl <- tbl(con, "species")

  base_test_cols <- c(
    'test_id',
    'test_cas',
    'species_number',
    'exposure_type',
    'test_type',
    'organism_lifestage'
  )

  base_result_cols <- c(
    'result_id',
    'test_id',
    'obs_duration_mean',
    'obs_duration_min',
    'obs_duration_max',
    'obs_duration_unit',
    'endpoint',
    'effect',
    'measurement',
    'conc1_type',
    'conc1_mean_op', # ! op denotes operator for QA/QC purposes
    'conc1_unit',
    'conc1_mean',
    'conc1_min',
    'conc1_max'
  )

  # Filtering for test columns + addtional special columns ----
  result_query <- tests_tbl %>%
    select(
      all_of(base_test_cols), # Strict: these MUST exist
      any_of(test_cols) # Flexible: adds user columns if valid
    ) %>%
    inner_join(
      species_tbl %>%
        select(
          species_number,
          common_name,
          latin_name,
          family,
          genus,
          species,
          eco_group,
          standard_test_species,
          invasive_species,
          endangered_threatened_species
        ),
      join_by('species_number')
    )

  # Chemical filtering ----
  if (!is.null(casrn) && length(casrn) > 0) {
    # Cleans casrns to format expected by database
    casrn_cleaned <- unique(casrn) %>%
      str_remove_all(., pattern = '-')

    result_query <- result_query %>%
      filter(test_cas %in% casrn_cleaned)
  }

  result_query <- result_query %>%
    left_join(
      tbl(con, 'chemicals') %>%
        select(
          cas_number,
          chemical_name,
          dtxsid,
          chemical_group = ecotox_group
        ),
      by = join_by(test_cas == cas_number)
    )

  # Filter by species using common and/or latin names ----
  # If both are provided, records matching either will be returned (OR condition).
  if (
    (!is.null(common_name) && length(common_name) > 0) ||
      (!is.null(latin_name) && length(latin_name) > 0)
  ) {
    species_filter_expr <- list()
    if (!is.null(common_name) && length(common_name) > 0) {
      species_filter_expr <- c(
        species_filter_expr,
        list(expr(.data$common_name %in% !!common_name))
      )
    }
    if (!is.null(latin_name) && length(latin_name) > 0) {
      species_filter_expr <- c(
        species_filter_expr,
        list(expr(.data$latin_name %in% !!latin_name))
      )
    }
    # Combine with OR
    combined_expr <- Reduce(function(a, b) expr(!!a | !!b), species_filter_expr)
    result_query <- result_query %>% filter(!!combined_expr)
  }
  # Filter for eco_group ----
  if (!is.null(eco_group) && length(eco_group) > 0) {
    eg <- eco_group

    result_query <- result_query %>%
      filter(eco_group %in% eg)
  }

  # Add species characteristic filters ----
  if (invasive) {
    result_query <- result_query %>% filter(invasive_species == TRUE)
  }
  if (standard) {
    result_query <- result_query %>% filter(standard_test_species == TRUE)
  }
  if (threatened) {
    result_query <- result_query %>%
      filter(endangered_threatened_species == TRUE)
  }

  # Filters for additonal results columns ----
  result_query <- result_query %>%
    inner_join(
      tbl(con, 'results') %>%
        select(
          all_of(base_result_cols),
          any_of(results_cols) # ! Adds 'bcf' or others here
        ),
      join_by('test_id')
    )

  # Endpoint regex ----
  if (!is.null(endpoint) && length(endpoint) > 0) {
    switch(
      paste(endpoint, collapse = " "),
      "all" = {
        # cli::cli_alert('All endpoints selected')
        # No filtering needed, so skip the filter step
      },
      "default" = {
        # cli::cli_alert('Default endpoints selected')

        # NOTE Looks for a verified set of endpoints that are useful
        endpoint_regex <- "^EC50|^LC50|^LD50|LR50|^LOEC|^LOEL|NOEC|NOEL$|NR-ZERO|NR-LETH|AC50|\\(log\\)EC50|\\(log\\)LC50|\\(log\\)LOEC"

        result_query <- result_query %>%
          filter(str_detect(endpoint, endpoint_regex))
      },
      {
        # cli::cli_alert('Custom endpoints selected')
        endpoint_regex <- paste(endpoint, collapse = "|")
        result_query <- result_query %>%
          filter(str_detect(endpoint, endpoint_regex))
      }
    )
  }

  # Final cleaning + coercion ----
  result_query <- result_query %>%
    mutate(
      common_name = str_squish(common_name),
      exposure_type = str_remove_all(exposure_type, pattern = "\\/"),
      measurement = str_remove_all(measurement, pattern = "\\/"),
      endpoint = str_remove_all(endpoint, pattern = "\\*|\\/"),
      effect = str_remove_all(effect, pattern = '~|/')
    ) %>%
    left_join(
      tbl(con, 'app_exposure_types') %>%
        select(
          exposure_group,
          term,
          exposure_description = description,
          exposure_definition = definition
        ),
      by = join_by('exposure_type' == 'term')
    ) %>%
    left_join(
      tbl(con, 'app_exposure_type_groups') %>%
        select(
          term,
          exposure_name = description
        ),
      by = join_by(
        'exposure_group' == 'term'
      )
    ) %>%
    left_join(
      tbl(con, 'app_effect_groups_and_measurements') %>%
        select(
          measurement_term = measurement_term,
          measurement_name = measurement_name,
          effect_code = effect_code,
          effect_name = effect
        ),
      by = join_by(
        'effect' == 'effect_code',
        'measurement' == 'measurement_term'
      )
    ) %>%
    left_join(
      tbl(con, 'effect_groups_dictionary'),
				by = join_by(measurement == term)
    ) %>%
    left_join(
      tbl(con, 'lifestage_codes') %>% rename(org_lifestage = description),
      by = join_by(organism_lifestage == code)
    ) %>%
    left_join(
      tbl(con, 'lifestage_dictionary'),
      by = join_by(org_lifestage == org_lifestage)
    ) %>%
    # left_join(
    # 	tbl(con, ''),
    # 	by = join_by( == )
    #) %>%
    # select(
    #   -test_id,
    #   -species_number,
    #   -exposure_type,
    #   -result_id
    # ) %>%
    mutate(
      # + = comment
      # `*` = converted value
      # ~ =
      # / =
      across(
        c(obs_duration_mean, obs_duration_min, obs_duration_max),
        ~ sql(sprintf("TRY_CAST(REGEXP_REPLACE(%s, '[\\*\\+]|\\s', '', 'g') AS DOUBLE)", cur_column()))
      ),
      # Explicitly cast concentration columns to DOUBLE to handle scientific notation
      across(
        c(conc1_mean, conc1_min, conc1_max),
        ~ sql(sprintf("TRY_CAST(REGEXP_REPLACE(%s, '[\\*\\+]|\\s', '', 'g') AS DOUBLE)", cur_column()))
      )
    ) %>%
    select(-test_id, -result_id, -species_number)

  # Adding conversion dictionaries -----------------------------------------

  result <- result_query %>%
    left_join(
      .,
      tbl(eco_con, 'unit_conversion'),
      join_by(conc1_unit == orig)
    ) %>%
    left_join(
      .,
      tbl(eco_con, 'duration_conversion'),
      join_by(obs_duration_unit == code)
    ) %>%
    left_join(
      .,
      tbl(eco_con, 'effect_conversion'),
      join_by(effect == term)
    ) %>%
    select(-effect_definition) %>%
    mutate(
      #endpoint = str_remove_all(endpoint, pattern = '~|/'),
      #coalesce(conc1_mean, conc1_min, conc1_max),
      result = case_when(
        is.na(conc1_mean) ~ geometric.mean(c(conc1_min, conc1_max), na.rm = TRUE),
        .default = conc1_mean,
      ),
      final_result = result * conversion_factor_unit,
      duration = coalesce(
        obs_duration_mean,
        obs_duration_min,
        obs_duration_max
      ) %>%
        as.numeric(),
      final_duration = duration * conversion_factor_duration
      #.keep = 'unused'
    ) %>%
    collect()

  return(result)
}

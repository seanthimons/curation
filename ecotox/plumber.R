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

#' Convert Concentration Units to Standardized Formats
#'
#' @description
#' This function standardizes concentration values from various units to a common
#' base unit within a dataframe. It creates two new columns, `new_value` and
#' `new_unit`, to store the converted values and their corresponding standardized units.
#'
#' The function handles two main categories of conversions:
#' 1.  Aqueous concentrations (`ug/L`, `ppb`, `ppm`) are standardized to 'mg/L'.
#' 2.  Dose-per-bee metrics (`g/bee`, `mg/bee`, `ug/bee` and their full-text
#'     equivalents) are standardized to 'ug/bee'.
#'
#' Units that do not fall into these categories are passed through without modification.
#'
#' @param data A data frame containing the data to be processed.
#' @param value_column The name of the column (as a string) containing numeric
#'   concentration values. This is evaluated using non-standard evaluation.
#' @param unit_column The name of the column (as a string) containing character
#'   unit strings corresponding to the values. This is also evaluated using
#'   non-standard evaluation.
#'
#' @return A data frame with two additional columns: `new_value` (the converted
#'   numeric value) and `new_unit` (the standardized character unit).
#'
convert_units <- function(data, value_column, unit_column) {
  data %>%
    mutate(
      new_value = case_when(
        #Need to find a way of grabbing active ingredient values...

        !!sym(unit_column) == "ug/L" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppb" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppm" ~ !!sym(value_column),
        !!sym(unit_column) %in% c("g/bee", "grams per bee") ~
          !!sym(value_column) * 1e6,
        !!sym(unit_column) %in% c("mg/bee", "milligrams per bee") ~
          !!sym(value_column) * 1000,
        !!sym(unit_column) %in% c("ug/bee", "micrograms per bee") ~
          !!sym(value_column),
        TRUE ~ !!sym(value_column)
      ),
      new_unit = case_when(
        !!sym(unit_column) == "ug/L" ~ "mg/L",
        !!sym(unit_column) == "ppb" ~ "mg/L",
        !!sym(unit_column) == "ppm" ~ "mg/L",
        !!sym(unit_column) %in%
          c(
            "g/bee",
            "grams per bee",
            "mg/bee",
            "milligrams per bee",
            "ug/bee",
            "micrograms per bee"
          ) ~
          "ug/bee",
        TRUE ~ !!sym(unit_column)
      )
    )
}

convert_duration <- function(data, value_column, unit_column) {
  data %>%
    mutate(
      new_dur = case_when(
        !!sym(unit_column) == "days" ~ !!sym(value_column) * 24,
        !!sym(unit_column) == "weeks" ~ !!sym(value_column) * 7 * 24,
        TRUE ~ !!sym(value_column)
      ),
      new_dur_unit = "hours"
    )
}

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
#* @param ecotox_group A list of ecotox groups to query
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
  threatened = FALSE
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
    stop(
      "At least one query parameter (casrn, common_name, latin_name, endpoint, ecotox_group, invasive, standard, threatened) must be provided."
    )
  }

  # Base tables
  tests_tbl <- tbl(con, "tests")
  species_tbl <- tbl(con, "species")

  # Apply filters to base tables if parameters are provided
  if (!is.null(casrn) && length(casrn) > 0) {
    # Cleans casrns to format expected by database
    casrn <- unique(casrn) %>%
      str_remove_all(., pattern = '-')

    tests_tbl <- tests_tbl %>% filter(test_cas %in% casrn)
  }

  # Filter by species using common and/or latin names.
  # If both are provided, records matching either will be returned (OR condition).
  if (
    !is.null(common_name) &&
      length(common_name) > 0 &&
      !is.null(latin_name) &&
      length(latin_name) > 0
  ) {
    species_tbl <- species_tbl %>%
      filter(
        .data$common_name %in% common_name | .data$latin_name %in% latin_name
      )
  } else if (!is.null(common_name) && length(common_name) > 0) {
    species_tbl <- species_tbl %>%
      filter(.data$common_name %in% common_name)
  } else if (!is.null(latin_name) && length(latin_name) > 0) {
    species_tbl <- species_tbl %>%
      filter(.data$latin_name %in% latin_name)
  }

  # Add species characteristic filters
  if (invasive) {
    species_tbl <- species_tbl %>% filter(invasive_species == TRUE)
  }

  if (standard) {
    species_tbl <- species_tbl %>% filter(standard_test_species == TRUE)
  }

  if (threatened) {
    species_tbl <- species_tbl %>% filter(endangered_threatened_species == TRUE)
  }

  result_query <- tests_tbl %>%
    select(
      'test_id',
      'test_cas',
      'species_number',
      'exposure_type',
      'test_type',
      'organism_lifestage'
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
          ncbi_taxid,
          ecotox_group
        ),
      join_by('species_number')
    )

  # NOTE: The eco_group is derived here for filtering purposes before data is
  # collected. It is calculated again after collection to ensure consistency.
  if (!is.null(eco_group) && length(eco_group) > 0) {
    result_query <- result_query %>%
      filter(eco_group %in% ecotox_group)
  }

  # Build endpoint regex for filtering
  endpoint_regex <- if (!is.null(endpoint) && length(endpoint) > 0) {
    # ensure endpoints are matched from the start of the string
    paste0("^", endpoint, collapse = "|")
  } else {
    # default endpoints if none are provided
    "^EC50|^LC50|^LD50|LR50|^LOEC|^LOEL|NOEC|NOEL$|NR-ZERO"
  }

  result <- result_query %>%
    inner_join(
      tbl(con, 'results') %>%
        select(
          'result_id',
          'test_id',
          'obs_duration_mean',
          'obs_duration_unit',
          'endpoint',
          'effect',
          'conc1_mean',
          'conc1_min',
          'conc1_max',
          'conc1_unit'
        ),
      join_by('test_id')
    ) %>%
    collect()
  # filter(
  # 	str_detect(
  # 		endpoint,
  # 		endpoint_regex
  # 	),
  # 	str_detect(effect, 'MOR|DVP|GRO|MPH'),
  # 	# conc1_unit %in%
  # 	# 	c(
  # 	# 		'ug/L',
  # 	# 		'mg/L',
  # 	# 		'ppm',
  # 	# 		'ppb',
  # 	# 		'mg/kg',
  # 	# 		'mg/kg/d',
  # 	# 		'mg/kg bdwt/d',
  # 	# 		'mg/kg diet',
  # 	# 		'g/bee',
  # 	# 		'grams per bee',
  # 	# 		'mg/bee',
  # 	# 		'milligrams per bee',
  # 	# 		'ug/bee',
  # 	# 		'micrograms per bee'
  # 	# 	),
  # 	# obs_duration_unit %in% c('h', 'd', 'wk')
  # ) %>%
  # left_join(
  # 	tbl(con, 'app_exposure_types') %>%
  # 		select(
  # 			'exposure_group',
  # 			'term'
  # 		) %>%
  # 		filter(
  # 			exposure_group %in%
  # 				c(
  # 					'AQUA',
  # 					'ENV',
  # 					'ORAL',
  # 					'TOP',
  # 					'Unspecified',
  # 					'UNK'
  # 				)
  # 		),
  # 	join_by('exposure_type' == 'term')
  # ) %>%
  # left_join(
  # 	tbl(con, 'lifestage_codes') %>% rename(org_lifestage = description),
  # 	join_by(organism_lifestage == code)
  # ) %>%
  # left_join(
  # 	tbl(con, 'lifestage_dictionary'),
  # 	join_by(org_lifestage == org_lifestage)
  # ) %>%
  # collect() %>%
  # select(
  # 	-test_id,
  # 	-species_number,
  # 	-exposure_type,
  # 	-result_id
  # ) %>%
  # filter(
  # 	!is.na(conc1_mean),
  # 	!is.na(obs_duration_unit) & !is.na(obs_duration_mean)
  # ) %>%
  # mutate(
  # 	#Plus means comment, asterisk mean converted value
  # 	result = as.numeric(str_remove_all(conc1_mean, pattern = "\\*|\\+")),
  # 	effect = case_when(
  # 		str_detect(effect, 'MOR') ~ "MOR",
  # 		str_detect(effect, 'DVP|GRO|MPH') ~ "DVP_GRO_MPH",
  # 		# str_detect(effect, 'GRO') ~ "GRO",
  # 		# str_detect(effect, 'MPH') ~ "MPH"
  # 	),
  # 	endpoint = str_remove_all(endpoint, pattern = "\\*|\\/")

  # 	# case_when(
  # 	# 	endpoint == 'EC50' ~ 'EC50',
  # 	# 	endpoint == 'EC50*' ~ 'EC50',
  # 	# 	endpoint == 'EC50/' ~ 'EC50',
  # 	# 	endpoint == 'LC50' ~ 'LC50',
  # 	# 	endpoint == 'LC50*' ~ 'LC50',
  # 	# 	endpoint == 'LC50*/' ~ 'LC50',
  # 	# 	endpoint == 'LC50/' ~ 'LC50',
  # 	# 	endpoint == 'LD50' ~ 'LD50',
  # 	# 	endpoint == 'LD50/' ~ 'LD50',
  # 	# 	endpoint == 'LOEC' ~ 'LOEC',
  # 	# 	endpoint == 'LOEC/' ~ 'LOEC',
  # 	# 	endpoint == 'LOEL' ~ 'LOEL',
  # 	# 	endpoint == 'LOEL/' ~ 'LOEL',
  # 	# 	endpoint == 'LOELR' ~ 'LOEL',
  # 	# 	endpoint == 'NOEC' ~ 'NOEC',
  # 	# 	endpoint == 'NOEC/' ~ 'NOEC',
  # 	# 	endpoint == 'NOEL' ~ 'NOEL',
  # 	# 	endpoint == 'NOEL/' ~ 'NOEL',
  # 	# 	endpoint == 'NOELR' ~ 'NOEL',
  # 	# 	endpoint == 'NR-ZERO' ~ 'NR-ZERO',
  # 	# 	endpoint == 'NR-ZERO/' ~ 'NR-ZERO',
  # 	# ),

  # 	duration_value = as.numeric(obs_duration_mean),
  # 	duration_unit = case_when(
  # 		obs_duration_unit == 'h' ~ 'hours',
  # 		obs_duration_unit == 'd' ~ 'days',
  # 		obs_duration_unit == 'wk' ~ 'weeks'
  # 	),
  # 	harmonized_life_stage = case_when(
  # 		is.na(org_lifestage) ~ 'Other/Unknown',
  # 		.default = harmonized_life_stage
  # 	),
  # 	harmonized_life_stage = factor(
  # 		harmonized_life_stage,
  # 		levels = c(
  # 			'Egg/Embryo',
  # 			'Larva/Juvenile',
  # 			'Subadult/Immature',
  # 			'Adult',
  # 			'Reproductive',
  # 			'Dormant/Senescent',
  # 			'Other/Unknown'
  # 		)
  # 	)
  # ) %>%
  # convert_duration(
  # 	.,
  # 	value_column = 'duration_value',
  # 	unit_column = 'duration_unit'
  # ) %>%
  # convert_units(., value_column = 'result', unit_column = 'conc1_unit')

  return(result)
}

# Packages ----------------------------------------------------------------

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
      #	'data.table',
      'mirai',
      # 'targets',
      # 'crew',

      ## DB ----
      #  'arrow',
      'nanoparquet',
      'duckdb',
      'duckplyr',
      #  'dbplyr',

      ## EDA ----
      'skimr',

      ## Web ----
      'rvest',
      'polite',
      #	'plumber',
      # 'plumber2', #Still experimental
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
      # 'ggrepel', # Suggested for non-overlapping labels
      # 'gganimate', # Suggested for animations
      # 'ggsignif',
      # 'ggTimeSeries',
      # 'tidyheatmaps',

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

  # if(!dir.exists(here::here('output'))) {
  #   dir.create(here::here('output'))
  # }

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

  setwd(here::here('threat_build'))

	library(ComptoxR)
}


# functions ---------------------------------------------------------------

#' Query All Database Tables for Distinct Column Combinations
#'
#' This function iterates through all tables in the `threat_db` database
#' connection, finds the unique combinations of the specified columns in each
#' table, and then combines them into a single, unique tibble.
#'
#' @details The function relies on a database connection object named `threat_db`
#'   being available in the calling environment. It uses `purrr::map` to iterate
#'   through all tables, `dplyr::distinct` to find unique rows for the selected
#'   variables, and `dplyr::list_rbind` to combine the results. A final
#'   `dplyr::distinct` call ensures uniqueness across all combined tables.
#'
#' @param ... Unquoted column names (using tidy evaluation) to select for
#'   finding distinct combinations. These are passed to `dplyr::distinct()`.
#'
#' @return A single `tibble` containing the unique combinations of the specified
#'   columns found across all tables in `threat_db`.
#'
#' @examples
#' \dontrun{
#' # Assuming 'threat_db' is an active database connection:
#' # Get all unique DTXSID and NAME combinations from all tables.
#' all_unique_chemicals <- query_db(DTXSID, NAME)
#' }
query_db <- function(...) {
  variables <- rlang::ensyms(...)

  dbListTables(threat_db) %>%
    map(
      .,
      ~ {
        tbl(threat_db, .x) %>%
          distinct(
            !!!variables
          ) %>%
          collect()
      },
      .progress = TRUE
    ) %>%
    list_rbind() %>%
    distinct()
}

#' Pull and Combine Columns from All Tables in a Database
#'
#' This function queries every table within the `threat_db` database connection,
#' selects specified columns from each, and combines the results into a single
#' tibble.
#'
#' @details The function relies on a database connection object named `threat_db`
#'   being available in the calling environment. It iterates through all tables
#'   listed by `dbListTables(threat_db)`. A progress bar is displayed during
#'   the mapping process.
#'
#' @param ... Unquoted column names (using tidy evaluation) to select from each
#'   table. These are passed to `dplyr::select()`.
#'
#' @return A single `tibble` containing the selected columns from all tables,
#'   row-bound together.
#'
#' @examples
#' \dontrun{
#' # Assuming 'threat_db' is an active database connection
#' # Pulls the DTXSID and SOURCE columns from all tables in threat_db
#' all_sources <- pull_table(DTXSID, SOURCE)
#' }
pull_table <- function(...) {
  variables <- rlang::ensyms(...)

  dbListTables(threat_db) %>%
    map(
      .,
      ~ {
        tbl(threat_db, .x) %>%
          select(!!!variables) %>%
          collect()
      },
      .progress = TRUE
    ) %>%
    list_rbind()
}



# init --------------------------------------------------------------------

if (file.exists('threat.duckdb')) {
  #threat_db <- dbConnect(duckdb(), dbdir = 'threat.duckdb', read_only = FALSE)
  #dbListTables(threat_db)
} else {
  # Dynamically find the latest 'toxval' file based on version in the filename.
  # This avoids hardcoding filenames and makes the script more robust to updates.
  final_lof <- list.files(here('final'), pattern = '^toxval.*\\.parquet$') %>%
    tibble(filename = .) %>%
    mutate(
      version_str = str_extract(filename, 'v\\d+_\\d*'),
      version_num = str_remove(version_str, 'v') %>%
        str_replace('_', '.') %>%
        as.numeric()
    ) %>%
    # Arrange by version number descending to get the latest version first
    arrange(desc(version_num)) %>%
    # Select the filename with the highest version
    slice_head(n = 3) %>%
    # Pull the filename as a character vector
    pull(filename)

  #str_subset(.,pattern = 'toxval_v96_1.parquet|toxval_v96_0.parquet|toxval_v95_.parquet')
  #str_subset(., pattern = 'toxval')

  #final_lof

  threat_db <-
    dbConnect(duckdb(), dbdir = ":memory:", read_only = FALSE)
  #dbConnect(duckdb(), dbdir = 'threat.duckdb', read_only = FALSE)

  final_lof %>%
    walk(
      .,
      function(x) {
        table_name <- str_remove(x, pattern = '\\.parquet$')
        file_path <- here('final', x)

        cli::cli_alert_info('Ingesting {x} into table {table_name}')

        # 1. tbl() creates a lazy reference to the Parquet file using its path.
        # 2. compute() executes the read and materializes the data into a persistent table.
        # The `.overwrite = TRUE` argument handles the table replacement logic cleanly.
        lazy_tbl <- tbl(
          threat_db,
          sql(sprintf("SELECT * FROM read_parquet('%s')", file_path))
        )
        compute(
          lazy_tbl,
          name = table_name,
          temporary = FALSE
        )
      },
      .progress = TRUE
    )

  rm(final_lof)
  dbListTables(threat_db)
}

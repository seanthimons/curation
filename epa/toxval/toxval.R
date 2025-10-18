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
      #  'duckdb',
      #  'duckplyr',
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

  setwd(here('epa', 'toxval'))
}

# Clowder files -----------------------------------------------------------

clowder_list <- request(
  'https://clowder.edap-cluster.com/api/datasets/61147fefe4b0856fdc65639b/listAllFiles'
) %>%
  req_perform() %>%
  resp_body_json()

folder_list <- clowder_list %>%
  map(~ .x[c("id", "filename", "folders")]) %>%
  keep(~ !is.null(.$folders) && length(.$folders) > 0)

tv_list <- clowder_list %>%
  map(~ .x[c("id", "filename")]) %>%
  keep(~ str_detect(.x$filename, "toxval_v9")) %>%
  discard(~ str_detect(.x$filename, '.sql|README|gz|with'))
#discard(~ str_detect(.x$filename, '.sql|README|qc_status|gz|with')) #%>%
# map(., ~pluck(., 'filename')) %>%
# unlist()

tv_ver <- tv_list %>%
  map(., ~ pluck(., 'filename')) %>%
  unlist() %>%
  str_replace_all(., 'toxval_all_res_', "") %>%
  str_subset(., pattern = 'toxval_v\\d{2}_\\d?') %>%
  str_extract(., pattern = 'v\\d{2}_\\d?') %>%
  unique()

tv_grp <- tv_ver %>%
  map(., function(ver) {
    keep(tv_list, ~ str_detect(.x$filename, pattern = ver))
  }) %>%
  set_names(tv_ver)

tv_source_ver <- tv_ver %>%
  str_replace(., "^(v)(\\d)(\\d)", "\\1\\2_\\3") %>%
  set_names(tv_ver)

tv_source_list <- clowder_list %>%
  map(~ .x[c("id", "filename", 'contentType')]) %>%
  keep(~ str_detect(.x$contentType, "sheet")) %>%
  keep(~ str_detect(.x$filename, "source_info")) %>%
  keep(~ str_detect(.x$filename, paste(tv_source_ver, collapse = "|")))

rm(tv_list, clowder_list)


# raw ---------------------------------------------------------------------

# Map over each version and creates a directory if it doesn't exist
new_ver <- map(
  names(tv_grp),
  ~ {
    if (!fs::dir_exists(here('epa', 'toxval', 'toxval_raw', .x))) {
      cli::cli_alert_info(paste('Creating directory for', .x))
      fs::dir_create(here('epa', 'toxval', 'toxval_raw', .x))
      .x <- .x
    } else {
      NULL
    }
  }
) %>%
  compact()

# Keeps only new versions to download, presumes that older versions are already downloaded
tv_grp <- tv_grp %>%
  keep_at(., names(tv_grp) %in% new_ver)

tv_ver <- tv_ver %>%
  keep(., . %in% new_ver)

if (length(new_ver) == 0) {
  cli::cli_abort('No new versions to download')
}

# Download ---------------------------------------------------------------

tv_grp %>%
  iwalk(
    .,
    ~ {
      setwd(here('epa', 'toxval', 'toxval_raw', .y))
      walk(
        .,
        ~ {
          cli::cli_alert(.x$filename)
          download.file(
            url = paste0(
              'https://clowder.edap-cluster.com/api/files/',
              .x$id,
              '/blob'
            ),
            destfile = .x$filename,
            mode = 'wb'
          )
        }
      )
      #list.files()
    }
  )

#lof <- list.files(here('epa', 'toxval_raw'), recursive = TRUE)

tv_ver %>%
  iwalk(
    .,
    ~ {
      cli::cli_alert(paste0("Building: ", .x))

      setwd(here('epa', 'toxval', 'toxval_raw', .x))

      raw <- list.files(getwd()) %>%
        map(
          .,
          ~ {
            readxl::read_excel(
              .x,
              col_types = c(
                "text"
              ),
              na = c("-", "")
            ) %>%
              janitor::clean_names()
          },
          .progress = TRUE
        ) %>%
        list_rbind()

      cli::cli_alert_success(paste0('Dumping: toxval_', .x, '.parquet'))
      nanoparquet::write_parquet(
        raw,
        file = here('final', paste0('toxval_', .x, '.parquet'))
      )
    },
    .progress = TRUE
  )


# Source info ------------------------------------------------------------

tv_source_list %>%
  walk(
    .,
    ~ {
      setwd(here('epa', 'toxval', 'source_info'))

      cli::cli_alert(.x$filename)
      download.file(
        url = paste0(
          'https://clowder.edap-cluster.com/api/files/',
          .x$id,
          '/blob'
        ),
        destfile = .x$filename,
        mode = 'wb'
      )
    }
  )

# Packages ----------------------------------------------------------------

{
	library(here)
	library(rio)
	library(janitor)
	library(tidyverse)
	library(httr2)
	library(nanoparquet)

	setwd(here('epa'))
}

{
	setwd(here('final'))

	files_to_check <- list.files(here('final'), pattern = 'dsstox')

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
			cli::cli_alert_warning(
				"One or more data files are older than 90 days. Rebuilding dataset."
			)
			tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
			TRUE
		} else {
			cli::cli_alert_success(
				"All data files are present and up-to-date. Skipping rebuild."
			)
			tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
			rm(files_to_check, files_exist_check, file_ages_days)
			FALSE
		}
	}
	setwd(here('epa'))
}


# Rebuild ----------------------------------------------------------------

# If a rebuild is needed, run the data scraping and processing sections.
if (rebuild_is_needed) {
	# Clowder files -----------------------------------------------------------

	clowder_list <- request(
		'https://clowder.edap-cluster.com/api/datasets/61147fefe4b0856fdc65639b/listAllFiles'
	) %>%
		req_perform() %>%
		resp_body_json()

	#Should only return the files
	dss_list <- clowder_list %>%
		map(., ~ .x[c("id", "filename")]) %>%
		keep(
			.,
			~ str_detect(.x$filename, "DSSTox_") &
				str_detect(.x$filename, ".zip") &
				any(str_detect(.x$filename, month.abb)) &
				any(str_detect(
					.x$filename,
					seq(lubridate::year(Sys.Date()) - 2, lubridate::year(Sys.Date())) %>%
						as.character()
				))
		)

	dss_ver <- dss_list %>%
		map(., ~ pluck(., 'filename')) %>%
		unlist() %>%
		str_replace_all(., 'DSSTox_', "") %>%
		str_remove_all(., ".zip") %>%
		unique()

	dss_grp <- dss_ver %>%
		map(., function(ver) {
			keep(dss_list, ~ str_detect(.x$filename, pattern = ver))
		}) %>%
		set_names(dss_ver)

	rm(dss_list, clowder_list)

	# raw ---------------------------------------------------------------------

	dir.create(here('epa', 'dsstox_raw'), showWarnings = FALSE)

	walk(
		names(dss_grp),
		~ dir.create(here('epa', 'dsstox_raw', .x), showWarnings = FALSE)
	)

	dss_grp %>%
		#temp %>%
		imap(
			.,
			~ {
				setwd(here('epa', 'dsstox_raw', .y))
				map(
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
			}
		)

	# ! TEMP
	dss_ver <- 'Feb_2024'

	dss_ver %>%
		iwalk(
			.,
			~ {
				cli::cli_alert(.x)
				setwd(here("epa", "dsstox_raw", .x))

				zip_files <- list.files(
					here('epa', 'dsstox_raw', .x),
					pattern = "\\.zip$",
					full.names = TRUE
				)

				if (length(zip_files) > 0) {
					map(
						zip_files,
						~ {
							unzip(.x, exdir = here('epa', 'dsstox_raw', .x))
						}
					)
					file.remove(zip_files)
				}

				raw <- list.files(
					here('epa', 'dsstox_raw', .x),
					pattern = "\\.xlsx?$",
					full.names = TRUE
				) %>%
					map(
						.,
						~ {
							readxl::read_excel(
								.x,
								col_types = c(
									"text", #dtxsid
									"text", #pref
									"text", #casrn
									"text", #inchi
									"text", #iupac
									"text", #smiles
									'text', #mol formula
									'skip', #avg mass
									'skip', #mono mass
									"skip", #QSAR
									"skip", #MS
									"text" # identifier
								),
								na = c("-", "")
							)
						},
						.progress = TRUE
					) %>%
					list_rbind() %>%
					mutate(
						ident_upper = str_to_upper(IDENTIFIER)
					) %>%
					tidyr::separate_longer_delim(
						.,
						cols = c(IDENTIFIER, ident_upper),
						delim = "|"
					) %>%
					# NOTE: Removes rows where the CASRN is in the ID columns (double entry), but keeps bad CASRNs.
					filter(
						CASRN != IDENTIFIER &
							CASRN != ident_upper
					) %>%
					pivot_longer(
						.,
						cols = c(
							PREFERRED_NAME,
							CASRN,
							MOLECULAR_FORMULA,
							INCHIKEY,
							IUPAC_NAME,
							SMILES,
							IDENTIFIER,
							ident_upper
						),
						names_to = 'parent_col',
						values_to = "values",
						values_drop_na = TRUE
					) %>%
					distinct() %>%
					arrange(factor(
						parent_col,
						levels = c(
							"PREFERRED_NAME",
							"CASRN",
							"MOLECULAR_FORMULA",
							"INCHIKEY",
							"IUPAC_NAME",
							"SMILES",
							"ident_upper",
							"IDENTIFIER"
						)
					))

				# NOTE Swapped to nanoparquet for speed
				nanoparquet::write_parquet(
					x = raw,
					file = here("final", paste0("dsstox_", .x, ".parquet"))
				)
			}
		)

	#unlink(here('epa', 'dsstox_raw'), recursive = TRUE)
	rm(rebuild_is_needed)
} else {
	(rm(rebuild_is_needed))
}

# DuckDB ------------------------------------------------------------------

library(duckdb)

# For large datasets, it's better to use a file-backed database
# instead of an in-memory one to avoid exhausting RAM.

dsstox_db <- DBI::dbConnect(
	duckdb::duckdb(),
	dbdir = here('final', 'dsstox.duckdb'),
	read_only = FALSE
)

# Clean up previous table if it exists to avoid errors on re-run
dbExecute(dsstox_db, "DROP TABLE IF EXISTS dsstox")

# Use a glob pattern to read all matching parquet files directly into one table.
# This is more efficient than iterating and loading one by one.
parquet_files_glob <- here("final", "dsstox_*.parquet")

dbExecute(
	dsstox_db,
	paste0(
		"CREATE TABLE dsstox AS SELECT * FROM read_parquet('",
		parquet_files_glob,
		"')"
	)
)

rm(parquet_files_glob)

dss_query <- function(query) {
	tbl(dsstox_db, 'dsstox') %>%
		filter(values == query) %>%
		collect()
}

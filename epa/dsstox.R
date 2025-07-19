# Packages ----------------------------------------------------------------

{
	library(here)
	library(rio)
	library(janitor)
	library(tidyverse)
	library(httr2)
	#library(rvest)
	#library(ComptoxR)
	#library(jsonlite)
	library(arrow)

	setwd(here('epa'))
	#load('epa.Rdata')
}


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

map(
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

dss_ver %>%
	imap(
		.,
		~ {
			cli::cli_alert(.y)
			setwd(here("epa", "dsstox_raw", .y))

			zip_files <- list.files(getwd(), pattern = "\\.zip$", full.names = TRUE)

			if (length(zip_files) > 0) {
				map(
					zip_files,
					~ {
						unzip(.x, exdir = getwd())
					}
				)
				file.remove(zip_files)
			}

			raw <- list.files(getwd(), pattern = "\\.xlsx?$", full.names = TRUE) %>%
				map(
					.,
					~ {
						readxl::read_excel(
							.x,
							col_types = "text",
							na = c("-", "")
						)
					},
					.progress = TRUE
				) %>%
				list_rbind()

			write_parquet(
				raw,
				sink = here("final", paste0("dsstox_", .y, ".parquet"))
			)
		}
	)

unlink(here('epa', 'dsstox_raw'), recursive = TRUE)

# DuckDB ------------------------------------------------------------------

dsstox_db <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

list.files(
	here("final"),
	pattern = "dsstox_.*\\.parquet$",
	full.names = TRUE
) %>% 
	walk(
		.,
		~ {
			table_name <- basename(.x) %>%
				str_remove("\\.parquet$") %>% 
				str_extract(., pattern = 'dsstox') #%>% print(.)

			duckdb::dbWriteTable(dsstox_db, table_name, nanoparquet::read_parquet(.x))
		}
	)

#isotope_regex_pattern <- paste0("(^\\d+[A-Z][a-z]?)|", "(^\\[\\d+[A-Z][a-z]?\\](?:[1-9]\\d*)?$)|", "((?<![A-Za-z])D(?![a-z]))|", "((?<![A-Za-z])T(?![a-z]))")

isotope_regex_pattern <- paste0(
  "(^\\d+[A-Z][a-z]?)|",
  "(^\\[\\d+[A-Z][a-z]?\\](?:[1-9]\\d*)?$)|",
  "(\\b[D|T]\\b)"
)

iso <- tbl(dsstox_db, 'dsstox') %>% 
	filter(str_detect(SMILES, isotope_regex_pattern)) %>% 
	collect()

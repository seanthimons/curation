# Packages ----------------------------------------------------------------

{
	library(here)
	library(rio)
	library(nanoparquet)
	library(janitor)
	library(tidyverse)
	library(ComptoxR)
	library(arrow)
	library(duckdb)
	library(duckplyr)

	setwd(here('threat_build'))
}


# functions ---------------------------------------------------------------

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
	final_lof <- list.files(here('final')) %>%
		str_subset(., pattern = '.RDS|bayes|treatment', negate = TRUE) %>%
		#TODO Adjust this for better filtering or preference
		str_subset(., pattern = 'toxval_v96_1.parquet')
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

				# This dplyr/dbplyr approach is a more idiomatic R alternative to writing raw SQL.
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

	dbListTables(threat_db)
}

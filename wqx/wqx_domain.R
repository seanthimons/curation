# packages ----------------------------------------------------------------

{
	library(here)
	library(rio)
	library(tidyverse)
	library(janitor)
	library(stringdist)
	library(fuzzyjoin)
	library(duckdb)

	setwd(here('wqx'))
}

con <- dbConnect(duckdb(), dbdir = ":memory:")

list.files(here('wqx', 'wqx_raw'), full.names = TRUE) %>%
	walk(
		~ {
			table_name <- tools::file_path_sans_ext(basename(.x))
			data <- rio::import(.x) %>%
				select(-Domain)

			dbWriteTable(con, table_name, as_tibble(data), overwrite = TRUE)
		},
		.progress = TRUE
	)

dbListTables(con)

wqx_table <- function(table) {
	tbl(con, table)
}


wqx_table('Characteristic') %>% glimpse()
wqx_table('Characteristic Alias') %>% glimpse()

wqx_table('Characteristic') %>%
	filter(str_detect(Name, 'Oil and Grease')) %>%
	select(Name, `SRS ID`, `Comparable Name`)

wqx_table('Characteristic Alias') %>%
	filter('Characteristic Name' == 'Oil and grease')

dbDisconnect(con, shutdown = TRUE)

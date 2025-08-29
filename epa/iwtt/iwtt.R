# Packages ----------------------------------------------------------------

{
	library(rio)
	library(janitor)
	library(tidyverse)
	library(here)
	library(httr)
	library(rvest)
	library(polite)
	library(duckdb)
	library(duckplyr)

	setwd(here('iwtt'))
}

if (file.exists('iwtt_db.duckdb')) {
	iwtt_con <- dbConnect(
		duckdb(),
		dbdir = 'iwtt_db.duckdb'
		#dbdir = ":memory:"
	)

	dbListTables(iwtt_con)
} else {
	# Download ----------------------------------------------------------------

	#ZIP
	read_html_live(url = 'https://watersgeo.epa.gov/iwtt/download-database') %>%
		html_elements(., xpath = '//*[@id="root"]/div/div/section/section/ul') %>%
		html_elements(., 'a') %>%
		html_attr('href') %>%
		str_remove_all(., "./assets/") %>%
		map(
			.,
			~ {
				download.file(
					url = paste0('https://watersgeo.epa.gov/iwtt/assets/', .x),
					destfile = .x,
					mode = 'wb'
				)
			}
		)

	#treatment technology descriptions

	dbWriteTable(
		iwtt_con,
		name = 'treatment_descriptions',
		value = read_html_live(
			url = 'https://watersgeo.epa.gov/iwtt/treatment-technologies'
		) %>%
			html_elements(
				.,
				xpath = '//*[@id="root"]/div/div/section/section/table'
			) %>%
			html_table() %>%
			pluck(., 1) %>%
			mutate(across(everything(), ~ na_if(.x, 'N/A'))) %>%
			clean_names(),
		overwrite = TRUE
	)

	# Unzip --------------------------------------------------------------------

	dir.create(here('iwtt', 'raw'))

	setwd(here('iwtt', 'raw'))

	unzip(zipfile = here('iwtt', list.files(here('iwtt'), pattern = '.zip')))

	unlink(here('iwtt', list.files(here('iwtt'), pattern = '.zip')))

	# Load --------------------------------------------------------------------

	iwtt_con <- dbConnect(
		duckdb(),
		dbdir = 'iwtt_db.duckdb'
		#dbdir = ":memory:"
	)

	lof <- list.files(here('iwtt', 'raw'), pattern = '.csv')

	map(
		lof,
		~ {
			df <- data.table::fread(
				input = here('iwtt', 'raw', .x),
				na.strings = c('NA')
			) %>%
				mutate(
					across(where(is.character), stringi::stri_enc_tonative),
					across(where(is.character), ~ na_if(.x, ""))
				) %>%
				janitor::clean_names()

			dbWriteTable(
				conn = iwtt_con,
				name = str_to_lower(
					str_remove(.x, pattern = ".csv")
				),
				value = df,
				overwrite = TRUE
			)
		},
		.progress = TRUE
	)

	rm(lof)

	unlink(here('iwtt', 'raw'), recursive = TRUE)

	iwtt_dict <- rio::import_list(
		file = list.files(here('iwtt'), pattern = '.xlsx')
	) %>%
		discard_at(., 1) %>%
		map(., clean_names) %>%
		map(., remove_empty) %>%
		set_names(., c('table_metadata', 'value_metadata'))

	imap(
		iwtt_dict,
		~ {
			dbWriteTable(
				conn = iwtt_con,
				name = .y,
				value = .x,
				overwrite = TRUE
			)
		},
		.progress = TRUE
	)

	dbListTables(iwtt_con)

	rm(iwtt_dict)

	unlink(list.files(path = here('iwtt'), pattern = '.xlsx'))

	#Quality check

	dbListTables(iwtt_con)
}

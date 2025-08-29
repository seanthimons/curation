# packages ----------------------------------------------------------------
{
	library(here)
	library(tidyverse)
	library(lubridate)
	library(janitor)
	library(rvest)
	library(httr)
	library(polite)
	library(rio)
	library(ComptoxR)

	setwd(here('pt'))

	`%ni%` <- Negate(`%in%`)
}


# DSSTox db --------------------------------------------------------------

{
	library(duckdb)

	isotope_regex_pattern <- paste0(
		"(^\\d+[A-Z][a-z]?)|",
		"(^\\[\\d+[A-Z][a-z]?\\](?:[1-9]\\d*)?$)|",
		"(\\b[D|T]\\b)"
	)

	if (file.exists(here('final', 'dsstox.duckdb'))) {
		con <- DBI::dbConnect(
			duckdb::duckdb(),
			dbdir = here('final', 'dsstox.duckdb'),
			read_only = FALSE
		)
	} else {
		con <- DBI::dbConnect(
			duckdb::duckdb(),
			dbdir = here('final', 'dsstox.duckdb'),
			read_only = FALSE
		)

		# Clean up previous table if it exists to avoid errors on re-run
		dbExecute(con, "DROP TABLE IF EXISTS dsstox")

		# Use a glob pattern to read all matching parquet files directly into one table.
		# This is more efficient than iterating and loading one by one.
		parquet_files_glob <- here("final", "dsstox_*.parquet")

		dbExecute(
			con,
			paste0(
				"CREATE TABLE dsstox AS SELECT * FROM read_parquet('",
				parquet_files_glob,
				"')"
			)
		)

		# NOTE adjusted due to duckdb limitations on regex pattern

		rm(db_file, parquet_files_glob)
	}
}

# PT list  ---------------------------------------------------------------

pt <- list()

# oxidation states --------------------------------------------------------

ind_html <-
	polite::bow(
		'https://en.wikipedia.org/wiki/Oxidation_state#List_of_oxidation_states_of_the_elements'
	) %>%
	polite::scrape(.) %>% # scrape web page
	rvest::html_nodes("table.wikitable") %>% # pull out specific table
	rvest::html_table()

#TODO Perhaps add a Roman Numeral column?

pt$oxidation_state <- ind_html[[3]] %>%
	.[4:121, 1:19] %>%
	setNames(., LETTERS[1:19]) %>%
	as_tibble() %>%
	mutate(
		I = as.character(I),
		A = as.numeric(A)
	) %>%
	mutate(across(B:S, ~ na_if(., ""))) %>%
	pivot_longer(
		.,
		cols = D:R,
		values_to = 'oxidation_state',
		values_drop_na = T
	) %>%
	select(-name) %>%
	setNames(., c('number', 'element', 'symbol', 'group', 'oxs')) %>%
	mutate(
		ox = case_when(
			stringr::str_detect(oxs, "\\+") ~ "+",
			stringr::str_detect(oxs, "\\u2212") ~ "-",
			.default = " "
		),
		oxs = stringr::str_remove_all(oxs, "[[:symbol:]]"),
		oxidation_state = paste0(oxs, ox) %>% str_trim(),
		search = case_when(
			# TODO Add proper parsing for "2+" -> "++"", "2-" -> "--"
			oxidation_state == '0' ~ symbol,
			oxidation_state == '1-' ~ paste0(symbol, ox),
			oxidation_state == '1+' ~ paste0(symbol, ox),
			.default = paste0(symbol, oxidation_state)
		)
	) %>%
	select(!c(oxs:ox))

# ? NOTE Holding off on this for now, little to no benefit.
# pt_elements <- pt$oxidation_state %>%
# 	select(symbol) %>%
# 	distinct()

# ox_dsstox <- tbl(dsstox_db, 'dsstox') %>%
# 	select(DTXSID, MOLECULAR_FORMULA, SMILES, CASRN) %>%
# 	collect() %>%
# 	inner_join(., pt_elements, join_by(MOLECULAR_FORMULA == symbol))
# 	filter(!str_detect(SMILES, pattern = "\\+|\\-"))

rm(ind_html)

# Table -------------------------------------------------------------------

pt$elements <- bow(
	'https://en.wikipedia.org/wiki/List_of_chemical_elements'
) %>%
	scrape(content = "text/html; charset=UTF-8") %>%
	html_nodes(".wikitable") %>%
	html_table() %>%
	pluck(1) %>%
	unname() %>%
	tibble::as_tibble(.name_repair = 'universal') %>%
	select(1:3) %>%
	set_names(
		.,
		c(
			'Number',
			'Symbol',
			'Name'
		)
	) %>%
	slice(., 2:nrow(.)) %>%
	mutate(Number = as.integer(Number))


element_list <- ct_list('ELEMENTS') %>%
	ct_details(., projection = 'all') %>%
	select(dtxsid, molFormula, inchikey, smiles)

pt$elements <-
	left_join(
		pt$elements,
		element_list,
		join_by(Symbol == molFormula)
	) %>%
	mutate(Number = as.character(Number))

# pt$elements %>%
# 	filter(is.na(dtxsid)) %>%
# 	select(Symbol) %>%
# 	dbWriteTable(dsstox_db, 'pt_smiles',., temporary = TRUE, overwrite = TRUE)

# pt_smiles <- tbl(dsstox_db, 'pt_smiles')

# pt_dsstox <- tbl(dsstox_db, 'dsstox') %>%
# 	inner_join(., pt_smiles, join_by(values == Symbol)) %>%
# 	arrange(factor(parent_col, levels = c('MOLECULAR_FORMULA', 'ident_upper', 'IDENTIFIER')))
# 	collect()
# 	filter(!str_detect(SMILES, pattern = "\\+|\\-"))

# Nuclides and isotopes ----------------------------------------------------------------

# nuclides <- bow('https://en.wikipedia.org/wiki/List_of_radioactive_nuclides_by_half-life') %>%
#   scrape(content="text/html; charset=UTF-8") %>%
#   html_nodes(".wikitable") %>% html_table() %>%
#   map(., ~select(., 'isotope')) %>%
#   list_rbind() %>%
#   pluck(1) %>%
#   as_tibble() %>%
#   #removes citations + comments
#   mutate(., value = str_remove_all(value, pattern  = regex('\\[(.*?)\\]|\\((.*?)\\)|\\{(.*?)\\}'))) %>%
#   filter(str_detect(value, pattern = '-')) %>%
#   separate_wider_delim(., cols = 'value', delim = '-', names = c('elements', 'iso_num')) %>%
#   filter(!str_detect(iso_num, '[[:punct:]]')) %>%
#   mutate(elements = snakecase::to_sentence_case(elements)) %>%
#   left_join(., pt$elements %>% select(Symbol:Name), by = join_by(elements == Name)) %>%
#   mutate(.,
#          isotopes = paste0(elements, '-', iso_num),
#          isotope_symbol = paste0(Symbol, '-', iso_num),
#          isotope_mass_symbol = paste0(iso_num, '-', Symbol),
#          .keep = 'none')
#
# stable <- bow('https://en.wikipedia.org/wiki/List_of_nuclides') %>%
#   scrape(content="text/html; charset=UTF-8") %>%
#   html_nodes(".wikitable") %>% html_table() %>%
#   discard_at(., 1) %>%
#   keep_at(., 1:3) %>%
#   map(., ~select(., c('A', 'Z'))) %>%
#   list_rbind() %>%
#   mutate(Z = as.character(Z)) %>%
#   left_join(., select(pt$elements, c(Number, Symbol, Name)), join_by('Z' == 'Number')) %>%
#   mutate(isotopes = paste0(Name,'-', A),
#          isotope_symbol = paste0(Symbol,'-', A),
#          isotope_mass_symbol = paste0(A,'-', Symbol), .keep = 'none')
#
# pt$isotopes <- bind_rows(stable, nuclides) %>%
#   distinct(isotopes, .keep_all = T)

nuclides <- bow('https://en.wikipedia.org/wiki/Table_of_nuclides') %>%
	scrape(content = "text/html; charset=UTF-8") %>%
	html_nodes(".wikitable") %>%
	html_table() %>%
	pluck(., 3) %>%
	.[-1, -1] %>%
	`colnames<-`(., NULL) %>%
	as_tibble(., .name_repair = 'universal_quiet') %>%
	mutate_all(~ na_if(., "")) %>%
	pivot_longer(
		.,
		cols = everything(),
		names_to = 'col',
		values_to = 'iso',
		values_drop_na = T
	) %>%
	filter(iso %ni% pt$elements$Number, iso %ni% pt$elements$Symbol) %>%
	mutate(
		Z = str_extract_all(iso, pattern = "\\d+(?:\\.\\d+)?"),
		element = str_extract_all(iso, pattern = "[^0-9.]+")
	) %>%
	select(-iso, -col) %>%
	unnest(., cols = c(Z, element)) %>%
	inner_join(
		.,
		pt$elements %>% select(Symbol:Name),
		join_by(element == Symbol)
	) %>%
	mutate(
		smiles = paste0("[", Z, element, "]"),
		#sysname = paste0("(~", Z, "~", element, ")", Name)
	)

dbWriteTable(con, 'nucs', nuclides, temporary = TRUE, overwrite = TRUE)

dss_nuc <- tbl(con, 'nucs')

# ! NOTE left-join here instead of the inner join for future DSSTOX purposes.
nuc_good <- left_join(
	dss_nuc,
	tbl(con, 'dsstox') %>%
		filter(
			!str_detect(parent_col, 'MOLECULAR_FORMULA') #& !str_detect(parent_col, pattern = "\\+|\\-"))
		),
	join_by(smiles == values)
) %>%
	select(-parent_col) %>%
	arrange(Z) %>%
	collect()

# get_dupes(temp, smiles) %>%
# 	select(-dupe_count, -Z, -element) %>%
# 	print(n = Inf)
# #	write.csv('dupes.csv')

# new_nucs <- anti_join(
# 		dss_nuc,
# 		tbl(dsstox_db, 'dsstox') %>%
# 			filter(
# 				str_detect(parent_col, 'MOLECULAR_FORMULA') #& !str_detect(parent_col, pattern = "\\+|\\-"))
# 			)
# 		,join_by(smiles == values)
# 	) %>%
# 	collect()

# tbl(dsstox_db, 'dsstox') %>%
# 	filter(DTXSID == 'DTXSID70109751')

pt$isotopes <- nuc_good

# Export ------------------------------------------------------------------

dbDisconnect(con)

rio::export(pt, file = here('final', 'pt.RDS'))

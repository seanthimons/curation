# packages ----------------------------------------------------------------

{
	library(here)
	library(plumber)
	library(tidyverse)
	library(duckdb)
	library(duckplyr)
	library(DBI)
	library(magrittr)

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
	) {cli::cli_abort("One or more data files are missing.")}
}

# functions ---------------------------------------------------------------

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

#* Get a list of available tables within database
#* @get /all_tbls
all_tbls <- function() {
	con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
	on.exit(dbDisconnect(con))

	result <- DBI::dbListObjects(con) %>%
		.$table %>%
		map(
			.,
			~ pluck(., 'name') %>%
				unname()
		) %>%
		unlist()

	return(result)
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

# TODO Finish this
#* Retrieve data from database by CASRN
#* @param query A list of CASRNs to query
#* @post /results
post_results <- function(query) {
	con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
	on.exit(dbDisconnect(con))

	result <- tbl(con, "tests") %>%
		select(
			'test_id',
			'test_cas',
			'species_number',
			'exposure_type',
			'test_type',
			'organism_lifestage'
		) %>%
		filter(
			test_cas %in% query
		) %>%
		inner_join(
			tbl(con, "species") %>%
				# ! NOTE need logic here on restricting on certain species or not
				#filter(species_number %in% eco_species) %>%
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
		) %>%
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
					'conc1_unit'
				),
			join_by('test_id')
		) %>%
		filter(
			str_detect(
				endpoint,
				"^EC50|^LC50|^LD50|LR50|^LOEC|^LOEL|NOEC|NOEL|NR-ZERO"
			),
			str_detect(effect, 'MOR|DVP|GRO|MPH'),
			conc1_unit %in%
				c(
					'ug/L',
					'mg/L',
					'ppm',
					'ppb',
					'mg/kg',
					'mg/kg/d',
					'mg/kg bdwt/d',
					'mg/kg diet',
					'g/bee',
					'grams per bee',
					'mg/bee',
					'milligrams per bee',
					'ug/bee',
					'micrograms per bee'
				),
			obs_duration_unit %in% c('h', 'd', 'wk')
		) %>%
		left_join(
			tbl(con, 'app_exposure_types') %>%
				select(
					'exposure_group',
					'term'
				) %>%
				filter(
					exposure_group %in%
						c(
							'AQUA',
							'ENV',
							'ORAL',
							'TOP',
							'Unspecified',
							'UNK'
						)
				),
			join_by('exposure_type' == 'term')
		) %>%
		left_join(
			tbl(con, 'lifestage_codes') %>% rename(org_lifestage = description),
			join_by(organism_lifestage == code)
		) %>%
		left_join(
			tbl(con, 'lifestage_dictionary'),
			join_by(org_lifestage == org_lifestage)
		) %>%
		collect() %>%
		select(
			-test_id,
			-species_number,
			-exposure_type,
			-result_id
		) %>%
		filter(
			!is.na(conc1_mean),
			!is.na(obs_duration_unit) & !is.na(obs_duration_mean)
		) %>%
		mutate(
			#Plus means comment, asterisk mean converted value
			result = as.numeric(str_remove_all(conc1_mean, pattern = "\\*|\\+")),
			effect = case_when(
				str_detect(effect, 'MOR') ~ "MOR",
				str_detect(effect, 'DVP|GRO|MPH') ~ "DVP_GRO_MPH",
				# str_detect(effect, 'GRO') ~ "GRO",
				# str_detect(effect, 'MPH') ~ "MPH"
			),
			endpoint = case_when(
				endpoint == 'EC50' ~ 'EC50',
				endpoint == 'EC50*' ~ 'EC50',
				endpoint == 'EC50/' ~ 'EC50',
				endpoint == 'LC50' ~ 'LC50',
				endpoint == 'LC50*' ~ 'LC50',
				endpoint == 'LC50*/' ~ 'LC50',
				endpoint == 'LC50/' ~ 'LC50',
				endpoint == 'LD50' ~ 'LD50',
				endpoint == 'LD50/' ~ 'LD50',
				endpoint == 'LOEC' ~ 'LOEC',
				endpoint == 'LOEC/' ~ 'LOEC',
				endpoint == 'LOEL' ~ 'LOEL',
				endpoint == 'LOEL/' ~ 'LOEL',
				endpoint == 'LOELR' ~ 'LOEL',
				endpoint == 'NOEC' ~ 'NOEC',
				endpoint == 'NOEC/' ~ 'NOEC',
				endpoint == 'NOEL' ~ 'NOEL',
				endpoint == 'NOEL/' ~ 'NOEL',
				endpoint == 'NOELR' ~ 'NOEL',
				endpoint == 'NR-ZERO' ~ 'NR-ZERO',
				endpoint == 'NR-ZERO/' ~ 'NR-ZERO',
			),

			# Eco grouping ------------------------------------------------------------

			eco_group = case_when(
				str_detect(family, 'Megachilidae|Apidae') ~ 'Bees',
				str_detect(ecotox_group, 'Insects/Spiders') ~ 'Insects/Spiders',
				str_detect(ecotox_group, 'Flowers, Trees, Shrubs, Ferns') ~
					'Flowers, Trees, Shrubs, Ferns',
				str_detect(ecotox_group, 'Fungi') ~ 'Fungi',
				str_detect(ecotox_group, 'Algae') ~ 'Algae',
				str_detect(ecotox_group, 'Fish') ~ 'Fish',
				str_detect(ecotox_group, 'Crustaceans') ~ 'Crustaceans',
				str_detect(ecotox_group, 'Invertebrates') ~ 'Invertebrates',
				str_detect(ecotox_group, 'Worms') ~ 'Worms',
				str_detect(ecotox_group, 'Molluscs') ~ 'Molluscs',
				str_detect(ecotox_group, 'Birds') ~ 'Birds',
				str_detect(ecotox_group, 'Mammals') ~ 'Mammals',
				str_detect(ecotox_group, 'Amphibians') ~ 'Amphibians',
				str_detect(ecotox_group, 'Reptiles') ~ 'Reptiles',
				str_detect(ecotox_group, 'Moss, Hornworts') ~ 'Moss, Hornworts',
				.default = ecotox_group
			),
			duration_value = as.numeric(obs_duration_mean),
			duration_unit = case_when(
				obs_duration_unit == 'h' ~ 'hours',
				obs_duration_unit == 'd' ~ 'days',
				obs_duration_unit == 'wk' ~ 'weeks'
			),
			harmonized_life_stage = case_when(
				is.na(org_lifestage) ~ 'Other/Unknown',
				.default = harmonized_life_stage
			),
			harmonized_life_stage = factor(
				harmonized_life_stage,
				levels = c(
					'Egg/Embryo',
					'Larva/Juvenile',
					'Subadult/Immature',
					'Adult',
					'Reproductive',
					'Dormant/Senescent',
					'Other/Unknown'
				)
			)
		) %>%
		convert_duration(
			.,
			value_column = 'duration_value',
			unit_column = 'duration_unit'
		) %>%
		convert_units(., value_column = 'result', unit_column = 'conc1_unit') %>%

		# duration ----------------------------------------------------------------

		#filter(eco_group == 'Mammals' | eco_group == 'Birds' | eco_group == 'Fish') %>%
		mutate(
			test_type = case_when(
				# Mammals -----------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Mammals') &
					(effect == 'MOR') &
					(exposure_group == 'ORAL' | is.na(exposure_group)) &
					#(life_stage == 'Adult') &
					(new_unit == 'mg/kg') &
					(endpoint == 'LD50') ~
					'acute',

				(eco_group == 'Mammals') &
					(effect == 'MOR') &
					(exposure_group == 'ORAL' | is.na(exposure_group)) &
					#(life_stage == 'Adult') &
					(new_unit == 'mg/kg bdwt') &
					(endpoint == 'LD50') ~
					'acute',
				## Chronic -----------------------------------------------------------------
				(eco_group == 'Mammals') &
					(effect == 'MOR') &
					(exposure_group == 'ORAL' | is.na(exposure_group)) &
					#(life_stage == 'Adult') &
					(endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
					(new_unit == 'mg/kg/d') ~
					'chronic',

				# Birds -------------------------------------------------------------------
				#NOTE Needs better breakdown for reptiles and amphibians...
				## Acute -------------------------------------------------------------------
				(eco_group == 'Birds' |
					eco_group == 'Amphibians' |
					eco_group == 'Reptiles') &
					(effect == 'MOR') &
					(exposure_group == 'ORAL' | is.na(exposure_group)) &
					#(life_stage == 'Adult') &
					(new_unit == 'mg/kg') &
					(endpoint == 'LD50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Birds' |
					eco_group == 'Amphibians' |
					eco_group == 'Reptiles') &
					(effect == 'MOR') &
					(exposure_group == 'ORAL' | is.na(exposure_group)) &
					#(life_stage == 'Adult') &
					(endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
					(new_unit == 'mg/kg/d' | new_unit == 'mg/kg bdwt/d') ~
					'chronic',

				# Fish --------------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Fish') &
					(effect == 'MOR') &
					#(life_stage == 'Adult') &
					(new_dur == 96) &
					(new_unit == 'mg/L') &
					(endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Fish') &
					(effect == 'MOR') &
					#(life_stage == 'Adult') &
					(new_dur >= 144) &
					(new_unit == 'mg/L') &
					(endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
					'chronic',

				(eco_group == 'Fish') &
					(effect == 'MOR') &
					#(life_stage == 'Adult') &
					(new_dur == 504) &
					(new_unit == 'mg/L') &
					(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
					'chronic',

				# Bees --------------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				# Could be refined later, but OPP doesn't isn't entirely clear
				(eco_group == 'Bees') &
					(effect == 'MOR') &
					(new_dur == 24 | new_dur == 28 | new_dur == 72) &
					(new_unit == 'ug/bee') &
					(endpoint == 'LD50' | endpoint == 'LC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Bees') &
					(effect == 'MOR') &
					(new_dur == 240) &
					(new_unit == 'ug/bee') &
					(endpoint == 'LD50' | endpoint == 'LC50') ~
					'chronic',

				# Insects -----------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Insects/Spiders') &
					(effect == 'MOR') &
					(new_dur == 24 | new_dur == 48 | new_dur == 72) &
					(new_unit == 'mg/L' | new_unit == 'mg/kg') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------

				(eco_group == 'Insects/Spiders') &
					(effect == 'MOR') &
					(new_dur == 504 | new_dur == 672) &
					(new_unit == 'mg/L' | new_unit == 'mg/kg') &
					(endpoint == 'NOEL' | endpoint == 'NOEC' | endpoint == 'NR-ZERO') ~
					'chronic',

				# Invertebrates -----------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
					(effect == 'MOR') &
					(new_dur == 24 | new_dur == 48 | new_dur == 72 | new_dur == 96) &
					(new_unit == 'mg/L' | new_unit == 'mg/kg') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
					(effect == 'MOR') &
					(new_dur == 504 | new_dur == 672) &
					(new_unit == 'mg/L' | new_unit == 'mg/kg') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'chronic',

				# Worms -------------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Worms') &
					(effect == 'MOR') &
					(new_dur == 336) &
					(new_unit == 'mg/kg') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Worms') &
					(effect == 'MOR') &
					(new_dur <= 336) &
					(new_unit == 'mg/kg') &
					(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
					'chronic',

				# Crustaceans -----------------------------------------------------------------------
				## Acute -------------------------------------------------------------------
				(eco_group == 'Crustaceans') &
					(effect == 'MOR') &
					(new_dur <= 96) &
					(new_unit == 'mg/L') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Crustaceans') &
					(effect == 'MOR') &
					(new_dur >= 672) &
					(new_unit == 'mg/L') &
					(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
					'chronic',

				# Algae -----------------------------------------------------------------------
				#NOTE Needs better, hard to find data for fungi and mosses etc
				## Acute -------------------------------------------------------------------
				(eco_group == 'Algae' |
					eco_group == 'Fungi' |
					eco_group == 'Moss, Hornworts') &
					(new_dur <= 24 * 7) &
					(new_unit == 'mg/L') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Algae' |
					eco_group == 'Fungi' |
					eco_group == 'Moss, Hornworts') &
					(new_dur == 96) &
					(new_unit == 'mg/L') &
					(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
					'chronic',

				# Flowers, Trees, Shrubs, Ferns-----------------------------------------------------------------------
				#Note probably could be something like developemental etc...
				## Acute -------------------------------------------------------------------
				(eco_group == 'Flowers, Trees, Shrubs, Ferns') &
					(new_dur <= 7 * 24) &
					(new_unit == 'mg/L') &
					(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
					'acute',

				## Chronic -----------------------------------------------------------------
				(eco_group == 'Flowers, Trees, Shrubs, Ferns') &
					(effect != 'MOR') &
					(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
					'chronic',
			)
		) %>%
		filter(!is.na(test_type))
	return(result)
}

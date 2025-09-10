library(ComptoxR)

all_lists <- ct_lists_all()

query_chems <- ct_list(
	list_name = c(
		'EPAHFR',
		'EPAHFRTABLE2',
		'FRACFOCUS',
		'CALWATERBDS',
		'PRODWATER'
	)
) %>%
	ct_details(query = .) %>%
	mutate(casrn = str_remove_all(casrn, "-"))

q1 <- post_results(casrn = query_chems$casrn)

q2 <- q1 %>%
	left_join(query_chems, ., join_by(casrn == test_cas)) %>%
	group_by(
		preferredName,
		casrn,
		endpoint,
		#test_type,
		eco_group,
		#harmonized_life_stage
	) %>%
	reframe(
		result = min(new_value),
		unit = new_unit
	) %>%
	distinct() %>%
	filter(!is.na(result) & endpoint == 'EC50')


df <- tibble::tribble(
	~time_val,
	~time_unit,
	10,
	"days",
	2,
	"weeks",
	48,
	"hours",
	3600,
	"seconds"
)


# Dictionaries -----------------------------------------------------------

unit_dict <- rio::import(here('ecotox', 'MeasureUnit.csv')) %>%
	clean_names() %>%
	select(
		code,
		target_unit,
		# conversion_factor,
		# conversion_coefficient,
		description
	) %>%
	mutate(
		domain = str_extract(description, "^[^,]+")
	) %>%
	select(-description)

unit_result <- rio::import(here('ecotox', 'lookup_unit_result.csv')) %>%
	clean_names()

unit_symbols <- rio::import(here(
	'ecotox',
	'lookup_unit_result_symbols.csv'
)) %>%
	clean_names() %>%
	bind_rows(mutate(., symbol = toupper(symbol)))


# Diagnostics ------------------------------------------------------------

tbl(eco_con, 'results') %>%
	#filter(is.na(conc1_unit)) %>%
	filter(conc1_unit == 'AI lb/100 gal/acre') %>%
	inner_join(., tbl(eco_con, 'tests'), join_by('test_id')) %>%
	inner_join(
		.,
		tbl(eco_con, 'references') %>%
			select(reference_number, publication_year),
		join_by('reference_number')
	) %>%
	add_count(test_cas, species_number) %>%
	inner_join(., tbl(eco_con, 'species')) %>%
	#select(test_cas, species_number, common_name, latin_name, n) %>%
	arrange(desc(n)) %>%
	#head(., 5) %>%
	collect() %>%
	glimpse() %>%
	View()


# Testing ----------------------------------------------------------------

# library(units)

# valid_units <- units::valid_udunits()

# install_unit("gal100", def = "100 gal")

# a <- set_units(5 / 100, 'lb/ gal')
# units(a) <- units::make_units(g / L)
# a <- set_units(a, 'g/L')

# Parsing ----------------------------------------------------------------

units <- tbl(eco_con, 'results') %>%
	select(orig = conc1_unit, test_id) %>%
	inner_join(
		.,
		tbl(eco_con, 'tests') %>%
			select(test_id, test_cas, species_number, reference_number),
		join_by('test_id')
	) %>%
	inner_join(
		.,
		tbl(eco_con, 'references') %>%
			select(reference_number, publication_year),
		join_by('reference_number')
	) %>%
	group_by(orig) %>%
	summarize(
		n = n(),
		cas_n = n_distinct(test_cas),
		species_n = n_distinct(species_number),
		ref_n = n_distinct(reference_number),
		date_n = n_distinct(publication_year),
		ref_date = max(
			sql("TRY_CAST(REPLACE(publication_year, 'xx', '15') AS NUMERIC)"),
			na.rm = TRUE
		)
	) %>%
	ungroup() %>%
	arrange(
		desc(n),
		desc(cas_n),
		desc(species_n),
		desc(ref_n),
		#	desc(ref_date)
	) %>%
	filter(!is.na(orig) & n > 2 & ref_n > 2) %>%
	collect() %>%
	select(
		-n,
		-cas_n,
		-species_n,
		-ref_n,
		-date_n,
		-ref_date,
	) %>%
	mutate(
		raw = str_replace_all(
			orig,
			{
				# Create a regex pattern for whole-word matching of symbols.
				# Symbols are sorted by length (desc) to prioritize longer matches 
				# (e.g., 'mg/L' over 'g').
				# Lookarounds '(?<!\w)' and '(?!\w)' ensure that symbols are not 
				# part of other words.
				# 'str_escape' is used to handle special characters in symbols.
				unit_symbols %>%
					arrange(-stringr::str_length(symbol)) %>%
					pull(symbol) %>%
					stringr::str_escape() %>%
					paste0("(?<!\\w)", ., "(?!\\w)") %>%
					stringr::str_flatten(., "|")
			},
			replacement = ""
		)	%>% 
			str_squish() %>% 
			str_replace_all(
				.,
				c(
					"/ |-" = "/",
					"0/00" = "ppt",
					'ppmw' = 'ppm',
					'ppmv' = 'ppm',
					'ml' = 'mL',
					'ul' = 'uL',
					#	'acres|acre' = 'ac',
					'dpm' = 'counts/min',
					'% ' = '%_',
					'fl oz' = 'fl_oz'
				)) %>% 
			# The regex replaces a space following a number in the denominator
			# of a fraction with an underscore.
			# e.g. "ml/100 g" -> "ml/100_g"
			str_replace_all(
				.,
				pattern = "/(\\d*\\.?\\d+) ",
				replacement = "/\\1_"
			) %>%
			str_replace_all(., c(' ' = "/", "//" = "/")) %>%
			str_squish(),
		# suffix = str_extract_all(orig, str_flatten(unit_symbols$symbol, "|")),
		# suffix = map_chr(suffix, ~ paste(.x, collapse = " "))
		u = raw
	) %>% 
	separate_wider_delim(
	u,
	delim = "/",
	names_sep = "_",
	too_few = 'align_start'
) %>% 
	mutate(across(dplyr::starts_with('u'), ~na_if(.x, "")))

# ! HERE

left_join(
	.,
	standardtox_dict,
	join_by(num == unit)
) %>%
	rename(num_type = type) %>%
	left_join(
		.,
		standardtox_dict,
		join_by(denom == unit)
	) %>%
	rename(denom_type = type) %>%
	mutate(
		derived_unit = case_when(
			# -- Dosage per unit/organism (highest priority) --
			!is.na(cur_units) &
				str_detect(cur_units, "/org|/fish|/egg|/bee|/cell|/disk|/cntr") ~
				"dosage (amount/unit)",

			# -- Specific Formulations / Non-science --
			!is.na(cur_units) & str_to_lower(cur_units) == "granules" ~ "noscience",

			# -- Application Rates (amount per area) --
			num_type == "mass" & denom_type == "area" ~
				"application rate (mass/area)",
			num_type == "volume" & denom_type == "area" ~
				"application rate (volume/area)",
			num_type == "mol" & denom_type == "area" ~ "molar application rate",

			# -- Molar Concentration (moles per volume) --
			num_type %in% c("mol", "mol/volume") & denom_type == "volume" ~
				"molar concentration",
			!is.na(cur_units) & cur_units %in% c("mM", "M", "nM", "pM", "uM", "N") ~
				"molar concentration",
			!is.na(cur_units) & str_detect(cur_units, fixed("mol/")) ~
				"molar concentration",

			# -- Concentration (mass/volume) --
			is.na(cur_units) & str_detect(raw, "% w/v") ~
				"concentration (mass/volume)",
			num_type == "mass" & denom_type == "volume" ~
				"concentration (mass/volume)",

			# -- Concentration (volume/mass) -- NEW CATEGORY
			num_type == "volume" & denom_type == "mass" ~
				"concentration (volume/mass)",

			# -- Fractions (like-units divided by like-units) --
			is.na(cur_units) & str_detect(raw, "% v/v") ~ "volume fraction",
			num_type == "mass" & denom_type == "mass" ~ "mass fraction",
			num_type == "volume" & denom_type == "volume" ~ "volume fraction",
			num_type == "fraction" & (is.na(denom_type) | denom_type == "noscience") ~
				"fraction (dimensionless)",

			# -- Molality (moles per mass) --
			num_type %in% c("mol", "mol/volume") & denom_type == "mass" ~ "molality",

			# -- Linear Density (mass per length) --
			num_type == "mass" & denom_type == "length" ~ "linear density",

			# -- Radioactivity Units --
			num_type == "radioactivity" & denom_type == "volume" ~
				"radioactivity concentration",
			num_type == "radioactivity" & denom_type == "mass" ~ "specific activity",
			num_type == "radioactivity" & denom_type == "mol" ~ "molar activity",

			# -- Flow/Count Rates (amount per time) --
			(num_type == "mass" | num_type == "volume") & denom_type == "time" ~
				"flow rate",
			!is.na(cur_units) & str_detect(cur_units, "counts/min") ~ "count rate",

			# -- Base Units (not derived from a ratio) --
			is.na(denom_type) &
				num_type %in% c("mass", "volume", "length", "mol", "radioactivity") ~
				num_type,

			# -- Final catch-all for anything else --
			TRUE ~ NA_character_
		)
	)

mutate(
	# The regex extracts characters from the start of the string
	# until a forward slash or space is encountered (i.e., the numerator).
	num = str_extract(raw, "^[^/\\s]+"),
	# The regex uses a positive lookbehind to extract characters
	# that follow a forward slash (i.e., the denominator).
	denom = str_extract(raw, "(?<=/)[^/\\s]+"),

	# The regex matches the first word and captures everything that
	# follows it.
	suffix = stringr::str_match(raw, "^\\S+\\s+(.*)")[, 2],
	cur_units = case_when(
		# The regex checks for the presence of a percent sign.
		str_detect(raw, pattern = '%') ~ NA,
		part_counts >= 1 ~ paste0(num, "/", denom),
		.default = num
	)
)

units %>%
	filter(str_detect(orig, 'mg/L 10 mi') | str_detect(raw, 'mg/L 10 mi'))

units %>%
	distinct(num) %>%
	print(n = Inf)

units %>%
	filter(part_counts == 1) %>%
	count(num) %>%
	arrange(desc(n))


# This script is for testing the updated `post_results` function.
# It assumes that 'plumber.R' has been sourced, providing the
# `post_results` function and its dependencies. If not, you may need to run:
# source("ecotox/plumber.R")

# Example 1: Query by a single CASRN
# This tests the `casrn` parameter with a single value.
test_casrn_single <- post_results(casrn = "50-29-3")
print("--- Test Query 1: Single CASRN ---")
head(test_casrn_single)

# Example 2: Query by multiple CASRNs
# This tests the `casrn` parameter with a vector of values.
test_casrn_multiple <- post_results(casrn = c("50-29-3", "72-20-8"))
print("--- Test Query 2: Multiple CASRNs ---")
head(test_casrn_multiple)

# Example 3: Query by common species name
# This tests the `common_name` parameter using a common name.
test_species_common <- post_results(common_name = "Rainbow trout")
print("--- Test Query 3: Common Species Name ---")
head(test_species_common)

# Example 4: Query by Latin species name
# This tests the `latin_name` parameter using a Latin name.
test_species_latin <- post_results(latin_name = "Oncorhynchus mykiss")
print("--- Test Query 4: Latin Species Name ---")
head(test_species_latin)

# Example 5: Query by a single endpoint
# This tests the `endpoint` parameter with a single value.
test_endpoint_single <- post_results(endpoint = "LC50")
print("--- Test Query 5: Single Endpoint ---")
head(test_endpoint_single)

# Example 6: Query by multiple endpoints
# This tests the `endpoint` parameter with a vector of values.
test_endpoint_multiple <- post_results(endpoint = c("LC50", "EC50"))
print("--- Test Query 6: Multiple Endpoints ---")
head(test_endpoint_multiple)

# Example 7: Query by ecotox group
# This tests the `eco_group` parameter.
test_eco_group <- post_results(eco_group = "Fish")
print("--- Test Query 7: Ecotox Group ---")
head(test_eco_group)

# Example 8: Query combining CASRN and ecotox group
# This tests the function with multiple parameters specified.
test_combination_casrn_group <- post_results(
	casrn = "50-29-3",
	eco_group = "Fish"
)
print("--- Test Query 8: Combination of CASRN and Ecotox Group ---")
head(test_combination_casrn_group)

# Example 9: Query combining common name and endpoint
# This tests another combination of parameters.
test_combination_species_endpoint <- post_results(
	common_name = "Rainbow trout",
	endpoint = "LC50"
)
print("--- Test Query 9: Combination of Common Name and Endpoint ---")
head(test_combination_species_endpoint)

# Example 10: Query combining common and latin names
# This tests using both common_name and latin_name to query species.
test_combination_species_names <- post_results(
	common_name = "Rainbow trout",
	latin_name = "Daphnia magna" # A different species
)
print("--- Test Query 10: Combination of Common and Latin Names ---")
# Check if both species are in the result
print(unique(test_combination_species_names$common_name))
head(test_combination_species_names)

# Example 11: Query with no parameters (should produce an error)
# This tests the error handling when no query parameters are provided.
print("--- Test Query 11: No Parameters (should error) ---")
tryCatch(
	{
		test_no_params <- post_results()
	},
	error = function(e) {
		message("Caught expected error: ", e$message)
	}
)

# Example 12: Query combining CASRN and ecotox group
# This tests the function with multiple parameters specified.
test_combination_casrn_group_standard <- post_results(
	casrn = "50-29-3",
	eco_group = "Fish",
	standard = TRUE
)

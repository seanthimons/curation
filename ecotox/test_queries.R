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
	clean_names() %>%
	select(
		-source,
		-unit_si,
		-unit_conv_si,
		-conv,
		-prefix,
		-word,
		-remove
	) %>%
	mutate(
		unit = case_when(
			unit == 'fl oz' ~ 'fl_oz',
			.default = unit
		)
	)

# fmt: skip
unit_symbols <- 
tibble::tribble(
~symbol,	~name,
"CEC","soil.cation.exchange",
"DT","digestivetract",
"H2O","water",
"TI","tissue",
"ae","acidequivalents",
"agar","agar",
"ai","activeingredient",
"bdwt","bodyweight",
"blood","blood",
"bt","bait",
"bw","bodyweight",
"caliper","caliper",
"circ","circular",
"dbh","diameterbreastheight",
"dia","diameter",
"diet","diet",
"disk","disk",
"dry wght", "dry weight",
"dw","dry weight",
"dry","dry",
"egg","egg",
"eu","experimentalunit",
"fd","food",
"fish","fish",
"food","food",
"humus","humus",
"ld","lipid",
"lipid","lipid",
"litter","litter",
"mat","material",
"media","media",
"om","organicmatter",
"org","organism",
"pellet","pellet",
"plt","pellet",
"sd","seed",
"seed","seed",
"soil","soil",
"solvent","solvent",
"tubers","tubers",
"wet wght", "wet weight",
"wet","wet",
"wt","wet",
'wght', 'weight'
) %>% 
bind_rows(mutate(., symbol = toupper(symbol)))


# Diagnostics ------------------------------------------------------------

tbl(eco_con, 'results') %>%
	#filter(is.na(conc1_unit)) %>%
	filter(conc1_unit == 'ppm w/w') %>%
	inner_join(., tbl(eco_con, 'tests'), join_by('test_id')) %>%
	add_count(test_cas, species_number) %>%
	inner_join(., tbl(eco_con, 'species')) %>%
	inner_join(
		.,
		tbl(eco_con, 'references') %>%
			select(reference_number, publication_year),
		join_by('reference_number')
	) %>%
	select(
		test_cas,
		species_number,
		common_name,
		latin_name,
		organism_habitat,
		n,
		publication_year
	) %>%
	arrange(desc(n)) %>%
	distinct() %>%
	#head(., 5) %>%
	collect() %>%
	glimpse() #%>%	View()


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
	# ! NOTE: Removes infrequent units
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
		idx = 1:n(),
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
		) %>%
			str_squish() %>%
			str_replace_all(
				.,
				c(
					"/ |-" = "/",
					"0/00" = "ppt",
					'\\bppmw\\b' = 'ppm',
					'\\bppmv\\b' = 'ppm',
					'\\bppm w/w\\b' = 'ppm',
					'\\bml\\b' = 'mL',
					'\\bul\\b' = 'uL',
					'\\bof\\b' = "",
					'\\bmi\\b' = 'min',
					# '\\bh\\b' = 'hour',
					# '\\bwk\\b' = 'week',
					# '\\byr\\b' = 'year',
					# '\\bd\\b' = 'day',
					#	'acres|acre' = 'ac',
					#'dpm' = 'counts/min',
					'% ' = '%_',
					'fl oz' = 'fl_oz'
				)
			) %>%
			# The regex replaces a space following a number in the denominator
			# of a fraction with an underscore.
			# e.g. "ml/100 g" -> "ml/100_g"
			str_replace_all(
				.,
				pattern = "/(\\d*\\.?\\d+) ",
				replacement = "/\\1_"
			) %>%
			str_replace_all(
				.,
				c(
					' ' = "/",
					"//" = "/",
					"%_v/v" = "%_v_v",
					"%_w/v" = "%_w_v",
					"%_w/w" = "%_w_w",
					"%_g/g" = "%_w_w"
				)
			) %>%
			str_squish() %>%
			str_remove(., "/$"),

		has_number = str_detect(raw, pattern = '/\\d+'),
		suffix = str_extract_all(orig, str_flatten(unit_symbols$symbol, "|")),
		suffix = map_chr(suffix, ~ paste(.x, collapse = " ")),
		u = raw
	) %>%
	separate_wider_delim(
		u,
		delim = "/",
		names_sep = "_",
		too_few = 'align_start'
	) %>%
	mutate(
		across(dplyr::starts_with('u'), ~ na_if(.x, "")),
		part_counts = rowSums(!is.na(select(., dplyr::starts_with('u'))))
	) %>%
	relocate(part_counts, .after = has_number) %>%
	pivot_longer(
		.,
		cols = dplyr::starts_with('u'),
		names_to = 'name'
	) %>%
	mutate(
		value = case_when(
			value == "%_" ~ "%",
			value == "%_v_v" ~ "% v/v",
			value == "%_w_v" ~ "% w/v",
			value == "%_w_w" ~ "% w/v",
			.default = value
		),
		num_mod = str_extract(value, pattern = "\\b\\d*\\.?\\d+_") %>%
			str_remove_all(., pattern = "_") %>%
			as.numeric(),
		value = str_remove_all(value, pattern = "\\b\\d*\\.?\\d+_")
	) %>%
	left_join(
		.,
		unit_result,
		join_by(value == unit)
	) %>%
	pivot_wider(
		.,
		names_from = name,
		values_from = value:type
	) %>%
	mutate(
		across(matches("^(num_mod|mult)"), ~ if_else(is.na(.x), 1, .x)),
		conversion = (multiplier_u_1 * num_mod_u_1) /
			(multiplier_u_2 * num_mod_u_2) /
			(multiplier_u_3 * num_mod_u_3),
		# The `pmap_chr` function is used to apply a function row-wise.
		# Here, it combines unit and type columns, omitting any NA values,
		# and collapses them into a single string separated by "/".
		cur_unit = purrr::pmap_chr(
			list(unit_conv_u_1, unit_conv_u_2, unit_conv_u_3),
			~ paste(na.omit(c(...)), collapse = "/")
		),
		cur_unit_type = purrr::pmap_chr(
			list(type_u_1, type_u_2, type_u_3),
			~ paste(na.omit(c(...)), collapse = "/")
		),
		unit_domain = case_when(
			# --- Rule 1: Invalid or Uncategorized Units (Highest Priority) ---
			# Catch anything with "noscience" or empty strings first.
			str_detect(cur_unit_type, "noscience") | cur_unit_type == "" ~
				"Invalid / Uncategorized",

			# --- Rule 2: Dosing Rates (Amount / Normalization / Time) ---
			# These are the most specific, so they must come before simpler rates or concentrations.
			str_ends(cur_unit_type, "/time") & str_count(cur_unit_type, "/") == 2 ~
				"Dosing Rate",

			# --- Rule 3: Application Rates (Amount / Area) ---
			cur_unit_type %in% c("mass/area", "volume/area", "mol/area") ~
				"Application Rate",

			# --- Rule 4: Concentrations (Amount / Volume or Amount / Mass) ---
			# Liquid-based concentrations
			cur_unit_type %in% c("mass/volume", "mol/volume", "fraction/volume") ~
				"Concentration (Liquid)",
			# Matrix-based concentrations (e.g., in soil, tissue)
			cur_unit_type %in% c("mass/mass", "mol/mass", "volume/mass") ~
				"Concentration (Matrix)",

			# --- Rule 5: Simple Rates (Amount / Time) ---
			cur_unit_type %in% c("mass/time", "volume/time", "fraction/time") ~
				"Rate",

			# --- Rule 6: Dimensionless Ratios ---
			cur_unit_type %in% c("fraction", "volume/volume") ~ "Ratio / Fraction",

			# --- Rule 7: Radioactivity ---
			# Catches all variations like "radioactivity/volume", "radioactivity/mass", etc.
			str_starts(cur_unit_type, "radioactivity") ~ "Radioactivity",

			# --- Rule 8: Linear Density (Amount / Length) ---
			cur_unit_type %in% c("mass/length", "volume/length") ~ "Linear Density",

			# --- Rule 9: Simple Fundamental Quantities ---
			cur_unit_type == "mass" ~ "Mass",
			cur_unit_type == "volume" ~ "Volume",
			cur_unit_type == "mol" ~ "Amount (molar)",
			cur_unit_type == "length" ~ "Length",
			cur_unit_type == "time" ~ "Time",

			# --- Rule 10: Catch-all for Other Valid but Complex Types ---
			# This will group any remaining complex but valid units.
			TRUE ~ "Other Complex Unit"
		)
	)
unit_conversion_tbl <- units %>%
	select()

# ! HERE

# testing + validation ---------------------------------------------------

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

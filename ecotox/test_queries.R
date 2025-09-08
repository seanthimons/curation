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

standardtox_dict <- rio::import(here('ecotox', 'lookup_unit_result.csv')) %>%
	clean_names() %>%
	select(
		unit,
		type
	)


# Diagnostics ------------------------------------------------------------

tbl(eco_con, 'results') %>%
	filter(conc1_unit == 'ppmw') %>%
	inner_join(tbl(eco_con, 'tests'), join_by('test_id')) %>%
	count(test_cas, species_number) %>%
	arrange(desc(n)) %>%
	head(., 5) %>%
	inner_join(., tbl(eco_con, 'species')) %>%
	select(test_cas, species_number, common_name, latin_name) %>%
	collect() %>%
	glimpse() %>%
	View()


# Testing ----------------------------------------------------------------

library(units)

valid_units <- units::valid_udunits()

a <- set_units(1, 'mg/l')
units(a) <- units::make_units(ppm)
a


# Parsing ----------------------------------------------------------------

units <- tbl(eco_con, 'results') %>%
	select(orig = conc1_unit, test_id) %>%
	inner_join(
		.,
		tbl(eco_con, 'tests') %>%
			select(test_id, test_cas, species_number, reference_number),
		join_by('test_id')
	) %>%
	group_by(orig) %>%
	summarize(
		n = n(),
		cas_n = n_distinct(test_cas),
		species_n = n_distinct(species_number),
		ref_n = n_distinct(reference_number)
	) %>%
	ungroup() %>%
	arrange(desc(n), desc(cas_n), desc(species_n), desc(ref_n)) %>%
	filter(cas_n >= 10 & ref_n > 1) %>%
	collect() %>%
	# select(
	# 	-cas_n,
	# 	-species_n,
	# 	-ref_n
	# ) %>%
	mutate(
		orig_part_counts = str_count(orig, pattern = "/"),
		raw = str_remove_all(orig, 'ae|AE|ai|AI|fl|litter|of |eu|/eu|-atoms') %>%
			str_replace_all(
				.,
				c(
					"/ |-" = "/",
					"sd" = "seed",
					"bt" = "bait",
					"0/00" = "ppt",
					'ppmw' = 'ppm',
					'ppmv' = 'ppm',
					'ml' = 'mL',
					'ul' = 'uL',
					#	'acres|acre' = 'ac',
					'dpm' = 'counts/min'
				)
			) %>%
			str_replace_all(
				.,
				pattern = "/(\\d*\\.?\\d+) ",
				replacement = "/\\1_"
			) %>%
			str_squish(),
		part_counts = str_count(raw, pattern = "/"),
		has_numbers = str_detect(raw, pattern = '/\\d+'),
		num = str_extract(raw, "^[^/\\s]+"),
		denom = str_extract(raw, "(?<=/)[^/\\s]+"),
		suffix = stringr::str_match(raw, "^\\S+\\s+(.*)")[, 2],
		cur_units = case_when(
			str_detect(raw, pattern = '%') ~ NA,
			part_counts >= 1 ~ paste0(num, "/", denom),
			.default = num
		)
	)


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
	rename(denom_type = type)

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

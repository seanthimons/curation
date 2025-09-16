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

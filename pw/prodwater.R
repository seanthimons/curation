# This script processes produced water quality data from the Pennsylvania
# Department of Environmental Protection (PA DEP) and the National Water
# Information System (NEWTS).
#
# The script performs the following actions:
#   1. Installs and loads necessary R packages.
#   2. Establishes a connection to a DuckDB database.
#   3. Reads, cleans, and transforms PA DEP data into a tidy format.
#   4. Writes the processed PA DEP data to a Parquet file and DuckDB table.
#   5. Processes the NEWTS field dictionary to isolate metadata and parameters.
#   6. Writes the NEWTS metadata and parameters to separate DuckDB tables.
#   7. Reads the NEWTS integrated dataset and writes it to a DuckDB table.
#   8. Pivots the NEWTS data into a long format for analysis.
#   9. Disconnects from the DuckDB database.
#
{
	#' Installs and loads a list of R packages.
	#'
	#' This function checks for the installation of specified packages,
	#' installs any that are missing, and then loads them into the R session.
	#'
	#' @param package A character vector of package names.
	#' @param load A logical indicating whether to load the packages.
	install_booster_pack <- function(package, load = TRUE) {
		for (pkg in package) {
			if (!requireNamespace(pkg, quietly = TRUE)) {
				install.packages(pkg)
			}
			if (load) {
				library(pkg, character.only = TRUE)
			}
		}
	}

	# Define and install the required packages for the script.
	booster_pack <- c(
		## IO ----
		'fs',
		'here',
		'janitor',
		'rio',
		'tidyverse',
		## DB ----
		'arrow',
		'nanoparquet',
		'duckdb',
		'duckplyr',
		## EDA ----
		'skimr'
	)

	install_booster_pack(package = booster_pack, load = TRUE)
	rm(install_booster_pack, booster_pack)

	# Set the working directory to the 'pw' subdirectory.
	setwd(here('pw'))
}

# Initialize a connection to the DuckDB database.
con <- dbConnect(duckdb(), dbdir = 'prod_water.duckdb')

# Read and process the PA DEP data.
data.table::fread(file = here('pw', 'PA_DEP_26r_processed.csv')) %>%
	# Remove the existing row index column.
	select(
		-c(
			V1
		)
	) %>%
	# Replace empty strings with NA values.
	mutate(
		across(
			where(is.character),
			~ na_if(.x, "")
		)
	) %>%
	# Convert the date column to a date object.
	mutate(
		date = lubridate::mdy_hm(Date_Collected) %>% lubridate::as_date(),
		.before = 1
	) %>%
	# Pivot the data from wide to long format.
	pivot_longer(
		.,
		cols = !c(
			'date',
			'Date_Collected',
			'Original_Latitude',
			'Original_Longitude',
			'NEWTS_Water_Type',
			'Original_Dataset_ID',
			'Sample_Description',
			'Sample_Matrix',
		),
		names_to = 'raw_analyte',
		values_to = 'raw_val',
		values_drop_na = TRUE
	) %>%
	# Write the processed data to a Parquet file.
	write_parquet(., file = 'pa_prod_water')

# Write the processed PA DEP data to the DuckDB database.
duckdb::dbWriteTable(
	con,
	"pa_prod_water",
	read_parquet(here('pw', 'pa_prod_water')),
	overwrite = TRUE
)

# Read and process the NEWTS field dictionary.
rio::import("NEWTS_FieldDictionary_052024.xlsx") %>%
	.[, 2:5] %>%
	row_to_names(., 1) %>%
	clean_names() %>%
	dbWriteTable(con, 'newts_headers', ., overwrite = TRUE)

# Extract and store metadata from the NEWTS headers.
tbl(con, 'newts_headers') %>%
	filter(str_detect(
		feature_class_field_alias,
		pattern = 'ppm|\\/L|ppq|per mil|\\/|ft|TUa|NTU|FTU|%|mV|Celsius|TU|psi|pH|gravity|ohm',
		negate = T
	)) %>%
	collect() %>%
	dbWriteTable(con, 'newts_meta', value = .)

newts_meta <- tbl(con, 'newts_meta')


# Read and store the full NEWTS dataset.
data.table::fread(file = "NEWTS_Integrated_Full_052024.csv") %>%
	select(!c(1:3)) %>%
	dbWriteTable(con, 'newts', value = .)

# Extract the list of parameter names for pivoting.
np <- tbl(con, 'newts_params') %>%
	select('csv_field_name') %>%
	collect() %>%
	unlist() %>%
	unname()

# Pivot the NEWTS data from wide to long format.
tbl(con, 'newts') %>%
	pivot_longer(
		.,
		cols = any_of(np),
		names_to = 'analyte',
		values_to = 'value',
		values_drop_na = TRUE
	)

# Disconnect from the DuckDB database.
dbDisconnect(con)
rm(con)


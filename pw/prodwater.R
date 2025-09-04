#
{
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

	install_booster_pack(package = booster_pack, load = FALSE)
	rm(install_booster_pack, booster_pack)

	setwd(here('pw'))
}




con <- dbConnect(duckdb(), dbdir = 'prod_water.duckdb')





data.table::fread(file = here('pw', 'PA_DEP_26r_processed.csv')) %>%

	select(
		-c(
			V1

		)
	) %>%
	mutate(
		across(
			where(is.character),
			~ na_if(.x, "")
		)
	) %>%
	mutate(
		date = lubridate::mdy_hm(Date_Collected) %>% lubridate::as_date(),

		.before = 1
	) %>%
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
	write_parquet(., file = 'pa_prod_water')


duckdb::dbWriteTable(
	con,
	"pa_prod_water",
	read_parquet(here('pw', 'pa_prod_water')),
	overwrite = TRUE
)













rio::import("NEWTS_FieldDictionary_052024.xlsx") %>%
	.[, 2:5] %>%
	row_to_names(., 1) %>%
	clean_names() %>%
	dbWriteTable(con, 'newts_headers', ., overwrite = TRUE)

tbl(con, 'newts_headers') %>%
	filter(str_detect(
		feature_class_field_alias,
		pattern = 'ppm|\\/L|ppq|per mil|\\/|ft|TUa|NTU|FTU|%|mV|Celsius|TU|psi|pH|gravity|ohm',
		negate = T
	)) %>%
	collect() %>%
	dbWriteTable(con, 'newts_meta', value = .)

tbl(con, 'newts_headers') %>%
	filter(str_detect(
		feature_class_field_alias,
		pattern = 'ppm|\\/L|ppq|per mil|\\/|ft|TUa|NTU|FTU|%|mV|Celsius|TU|psi|pH|gravity|ohm',
		negate = F
	)) %>%
	collect() %>%
	dbWriteTable(con, 'newts_params', value = .)

data.table::fread(file = "NEWTS_Integrated_Full_052024.csv") %>%
	select(!c(1:3)) %>%
	dbWriteTable(con, 'newts', value = .)

np <- tbl(con, 'newts_params') %>%
	select('csv_field_name') %>%
	collect() %>%
	unlist() %>%
	unname()

tbl(con, 'newts') %>%



	pivot_longer(
		.,
		cols = any_of(np),
		names_to = 'analyte',
		values_to = 'value',
		values_drop_na = TRUE
	)








dbDisconnect(con)
rm(con)


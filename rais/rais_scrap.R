# Combining data -----------------------------------------------------

{
	walk(
		list.files(path = here("rais"), pattern = ".RDS"),
		~ assign(
			str_remove_all(.x, pattern = ".RDS"),
			read_rds(.x),
			envir = .GlobalEnv
		)
	)

	# ! TEMP
	sswqs <- rio::import(here('final', 'sswqs.parquet'))
	glimpse(sswqs)
}

# local = where to apply or application
# endpoint = larger coverage
# general usage = harmonized of local

#Harmonizing to SSWQS schema

rais_list <- list()

#rais_list$arars <-
rais_eco_arars %>%
	rename(
		'name' = 'state',
		'analyte' = 'preferredName',
		'cas' = 'cas_number',
		'local' = 'cit',
		'result' = 'value',
		'unit' = 'unit_name'
	) %>% 
	glimpse()

#rais_list$eco_bench <-
rais_benchmarks_chemicals %>% #glimpse()
	rename(
		'analyte' = 'preferredName',
		'name' = 'source',
		'' = 'cit',
		'' = 'local',
		'result' = 'criterion_value',
		'unit' = 'unit_name'
	)

rais <- rais %>%
	list_rbind()
bind_rows(rais, arars) %>%
	rename(
		preferredName = analyte,
		cit = benchmark,
		local = organism,
		unit_name = units,
		criterion_value = result,
		meta = ft
	) %>%
	mutate(
		local = str_replace_all(
			local,
			'Not specified|Not Specified',
			NA_character_
		),
		protection = 'Ecological',
		origin_category = case_when(
			str_detect(source, 'NOAA|OPP') ~ 'Federal',
			str_detect(source, 'EPA') ~ 'Federal',
			str_detect(
				source,
				'California|Florida|Illinois|New Jersey|New York DEC|Ohio|Washington'
			) ~
				'State',
			str_detect(
				source,
				'Australia and New Zealand|British Columbia|Canada|ECC Canada|Dutch Soil Protection Act|Ontario|UK'
			) ~
				'International',
			str_detect(source, 'LANL|ORNL') ~ 'Academic',
			str_detect(source, 'SETAC ECW') ~ 'Academic',
			#source == '' ~ 'Industry',
			str_detect(source, 'Consensus-Based') ~ 'Other',
			.default = 'Missing'
		),
		priority_id = 1,
		data_category = 'primary',
		criterion_id = paste0('rais_', 1:n())
	)

# Total compounds -----------------------------------------------------------

# rais_compounds <- distinct(rais, preferredName, cas, .keep_all = F) %>%
#   rename(orig_name = preferredName,
#          orig_cas = cas)

# # CAS ---------------------------------------------------------------------

# #TODO check sequence on this
# rais_cas <- ComptoxR::ct_search(type = 'string', search_param = 'equal', query = rais_compounds$orig_cas) %>%
#   filter(!is.na(searchName))

# rais_cas_final <- rais_cas %>%
#   arrange(rank) %>%
#   select(dtxsid,casrn, preferredName, searchValue) %>%
#   distinct(searchValue, .keep_all = T)

# rais_compounds <- left_join(rais_compounds, rais_cas_final, by = c('orig_cas' = 'searchValue'))

# rais_compounds <- rais_compounds %>%
#   split(is.na(.$preferredName))

# rais_compounds$`TRUE` <- rais_compounds$`TRUE` %>%
#   select(orig_name:orig_cas)

# # Name --------------------------------------------------------------------

# rais_name <- ComptoxR::ct_search(type = 'string', search_param = 'equal', query = rais_compounds$`TRUE`$orig_name) %>%
#   filter(!is.na(searchName))

# rais_name_final <- rais_name %>%
#   arrange(rank) %>%
#   select(dtxsid,casrn, preferredName, searchValue) %>%
#   distinct(searchValue, .keep_all = T)

# rais_compounds$`TRUE` <- left_join(rais_compounds$`TRUE`, rais_name_final, by = c('orig_name' = 'searchValue'))

# rais_compounds <- list_rbind(rais_compounds)

# rais_compounds <- rais_compounds %>%
#   split(is.na(.$preferredName))

# rio::export(rais_compounds$`TRUE`, file = paste0('rais_bad_',Sys.Date(),'.csv'))

# rais_compounds <- list_rbind(rais_compounds)

# rais_compounds <- rais_compounds %>%
#   filter(!is.na(preferredName))

# rais <- rais %>%
#   rename(orig_name = preferredName,
#          orig_cas = cas) %>%
#   left_join(., rais_compounds, join_by(orig_name, orig_cas))

# # Final export ------------------------------------------------------------

# rio::export(rais, file = paste0('rais_curated_',Sys.Date(), '.RDS'))

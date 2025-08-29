# Combining data -----------------------------------------------------

{
	walk(
		list.files(path = here("rais"), pattern = ".RDS"),
		~ assign(
			str_remove_all(.x, pattern = ".RDS"),
			read_rds(here('rais', .x)),
			envir = .GlobalEnv
		)
	)

	# ! TEMP
	sswqs <- rio::import(here('final', 'sswqs.parquet'))
	glimpse(sswqs)

	#pt <- ComptoxR::pt$isotopes
	# local = where to apply or application
	# endpoint = larger coverage
	# general usage = harmonized of local

	#Harmonizing to SSWQS schema

	rais_list <- list()

	rais_list$arars <-
		rais_eco_arars %>%
		rename(
			'name' = 'state',
			'analyte' = 'preferredName',
			'cas' = 'cas_number',
			'local' = 'cit',
			'endpoint' = 'group',
			'result' = 'value',
			'unit' = 'unit_name'
		)

	rais_list$eco_bench <-
		rais_benchmarks_chemicals %>%
		rename(
			'analyte' = 'preferredName',
			'name' = 'source',
			'endpoint' = 'cit',
			'result' = 'criterion_value',
			'unit' = 'unit_name'
		)

	rais_list$chem_tox <-
		rais_chemtox %>%
		rename(
			'analyte' = 'compound',
			'cas' = 'casrn',
			'local' = 'name',
			'result' = 'value',
			'name' = 'ref',
			'unit' = 'units',
		) %>%
		mutate(across(everything(), as.character))

	rais_list$radstox <-
		rais_radstox %>%
		rename(
			'analyte' = 'compound',
			'local' = 'name',
			'result' = 'value',
			'unit' = 'units'
		)
	rais_list$rad_bench <-
		rais_benchmarks_rads %>%
		rename(
			'analyte' = 'preferredName',
			'name' = 'source',
			'endpoint' = 'cit',
			'result' = 'criterion_value',
			'unit' = 'unit_name'
		)

	#rais_list$bg_vals <-
	gen_bgval %>%
		rename(
			'analyte' = 'compound',
			'link' = 'cit',
			'result' = 'value',
			'result_bin' = 'n_r',
		)

	rais_data <- rais_list %>%
		list_rbind(names_to = 'd_idx') %>%
		mutate(
			local = str_replace_all(
				local,
				'Not specified|Not Specified',
				NA_character_
			)
		)
	rm(list = ls()[str_detect(ls(), 'sswqs|rais_data', negate = TRUE)])
}

# Total compounds -----------------------------------------------------------

isotopes <- readRDS(here('final', 'pt.RDS')) %>%
	.$isotopes %>%
	filter(!is.na(DTXSID)) %>%
	mutate(search = paste0(element, "-", Z))

source(here('epa', 'dsstox.R'))

# Load into duckdb instance
rais_data %>%
	distinct(analyte, cas, .keep_all = F) %>%
	mutate(
		idx = 1:n(),
		cas_chk = ComptoxR::is_cas(cas),
		search = str_remove_all(analyte, pattern = "\\+D|\\+E") %>%
			str_to_upper(.)
	) %>%
	pivot_longer(., cols = c(search, cas, analyte)) %>%
	distinct(value, .keep_all = TRUE) %>%
	# TEMP
	# filter(idx == '43')
	copy_to(
		dsstox_db,
		.,
		"rais",
		temporary = TRUE,
		overwrite = TRUE
	)

# Checkpoint
tbl(dsstox_db, 'rais') %>% 
	filter()

# Then, use a semi_join to filter the 'dsstox' table
{
	t1 <- Sys.time()
	rais_search <- tbl(dsstox_db, "dsstox") %>%
		semi_join(
			.,
			tbl(
				dsstox_db,
				"rais"
			),
			by = c("values" = "value")
		)

	rais_results <- rais_search %>%
		left_join(
			tbl(dsstox_db, 'rais'),
			.,
			join_by(value == values)
		) %>%
		collect() %>%
		arrange(
			idx,
			factor(
				name,
				levels = c(
					'cas',
					'analyte',
					'search'
				)
			),
			factor(
				parent_col,
				levels = c(
					"PREFERRED_NAME",
					"CASRN",
					"MOLECULAR_FORMULA",
					"ident_upper",
					"IDENTIFIER"
				)
			)
		) %>%
		distinct(idx, name, DTXSID, .keep_all = TRUE) %>%
		#select(-value, -cas_chk, -parent_col) %>%
		# group_by(idx, name) %>%
		# mutate(dup_idx = row_number()) %>%
		# ungroup() %>%
		filter(!is.na(DTXSID)) %>%
		distinct(idx, DTXSID, .keep_all = TRUE) %>%
		group_by(idx) %>%
		mutate(
			dtx_count = n_distinct(DTXSID)
		) %>%
		ungroup()
	# mutate(new_cols = paste(name, dup_idx, sep = "_")) %>%
	# pivot_wider(
	# 	id_cols = idx,
	# 	names_from = 'new_cols',
	# 	names_sort = FALSE,
	# 	values_from = 'DTXSID',
	# 	values_fill = NA
	# ) %>%
	# rowwise() %>%
	# mutate(
	# 	dtx_count = sum(
	# 		!is.na(
	# 			c_across(
	# 				starts_with(c('analyte', 'cas', 'search'))))
	# 	),
	# 	dtx_distinct = n_distinct(
	# 		na.omit(
	# 			c_across(-idx & -dtx_count)))
	# ) %>%
	# ungroup()
	#collect()
	t2 <- Sys.time()
	print(t2 - t1)
	rm(t1, t2)
}

rais_relationships <- rais_results %>%
	distinct(DTXSID) %>%
	filter(!is.na(DTXSID)) %>%
	pull(DTXSID)

rais_relationships_result <- rais_relationships %>%
	ct_related(query = .) %>%
	filter(query %in% rais_relationships & dtxsid %in% rais_relationships)

rio::export(
	rais_relationships_result,
	file = here('rais', 'rais_relationships.RDS')
)

rais_details <- 


rais_relt <- rais_results %>%

	left_join(
		.,
		rais_relationships_result,
		join_by(DTXSID == query)
	) %>%
	# ? TODO Does it makes sense to cut down the relationships?
	filter(str_detect(relationship, 'Component'))


# SHUTDOWN ---------------------------------------------------------------

{
	dbDisconnect(dsstox_db)
	rm(dsstox_db)
}

# rais_compounds <- rais_compounds %>%
# 	filter(cas %ni% rais_cas$CASRN)

# rais_iso <- rais_compounds %>%
# 	filter(analyte %in% isotopes$search)

# # # CAS ---------------------------------------------------------------------

# # TODO
# rais_cas <- ComptoxR::ct_search(query = rais_compounds$cas) %>%
# 	arrange(rank) %>%
# 	select(dtxsid, casrn, preferredName, searchValue) %>%
# 	distinct(searchValue, .keep_all = T)

# rais_analytes <- ComptoxR::ct_search(query = rais_compounds$analyte)

# arrange(rank) %>%
# 	select(dtxsid, casrn, preferredName, searchValue) %>%
# 	distinct(searchValue, .keep_all = T)

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

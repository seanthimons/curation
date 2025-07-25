raw <- list.files(
	here('epa', 'dsstox_raw', 'Feb_2024'),
	pattern = "\\.xlsx?$",
	full.names = TRUE
) %>%
	map(
		.,
		~ {
			readxl::read_excel(
				.x,
				col_types = c(
					"text", #dtxsid
					"text", #pref
					"text", #casrn
					"skip", #inchi
					"skip", #iupac
					"skip", #smiles
					'text', #mol formula
					'skip', #avg mass
					'skip', #mono mass
					"skip", #QSAR
					"skip", #MS
					"text" #identifier
				),
				na = c("-", "")
			)
		},
		.progress = TRUE
	) %>%
	list_rbind() %>%
	mutate(
		ident_upper = str_to_upper(IDENTIFIER)
	) %>%
	tidyr::separate_longer_delim(
		.,
		cols = c(IDENTIFIER, ident_upper),
		delim = "|"
	) %>%
	# NOTE: Removes rows where the CASRN is in the ID columns (double entry), but keeps bad CASRNs.
	filter(
		CASRN != IDENTIFIER &
			CASRN != ident_upper
	) %>%
	pivot_longer(
		.,
		cols = c(PREFERRED_NAME, CASRN, MOLECULAR_FORMULA, IDENTIFIER, ident_upper),
		names_to = 'parent_col',
		values_to = "values"
	) %>%
	distinct() %>%
	arrange(factor(
		parent_col,
		levels = c("PREFERRED_NAME", "CASRN","MOLECULAR_FORMULA", "ident_upper", "IDENTIFIER")
	))

nanoparquet::write_parquet(
					x = raw,
					file = here("final", "dsstox_Feb_2024.parquet")
				)

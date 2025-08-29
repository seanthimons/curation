# Load necessary libraries

library(mirai)
library(parallel)

daemons(n = detectCores() - 2)

# Main processing pipeline
front_page <- fact_sheets %>% 
	compact() %>% 
	map(
		in_parallel(
			~ {
				library(magrittr)
				# Notice the change from %>% to |> inside the parallel function
				purrr::pluck(., 'fs') %>% 
					dplyr::mutate(idx = 1) %>% 
					tidyr::pivot_wider(
						id_cols = idx,
						names_from = name,
						values_from = value
					) %>% 
					janitor::clean_names()
			}
		),
		.progress = TRUE
	) %>% 
	list_rbind(names_to = 'method_id') %>%
	select(-idx)


analyte_list <- fact_sheets %>% 
	compact() %>% 
	map(
		in_parallel(
		~ {
			library(magrittr)
			purrr::pluck(., 'analytes') %>%
				dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
		}),
		.progress = TRUE
	) %>%
	list_rbind(names_to = 'method_id') %>%
	clean_names()

raw_analyte <- analyte_list %>%
	select(analyte) %>%
	distinct()

cur_analyte <- raw_analyte %>%
	rename(orig_name = analyte) %>%
	mutate(
		idx = 1:n(),
		n = str_count(orig_name, pattern = "\\("),
		raw_cas = str_extract(orig_name, "\\([^()]*\\)$") %>%
			str_remove_all("^\\(|\\)$"),
		is_cas = ComptoxR::is_cas(raw_cas),
		cas_chk = ComptoxR::as_cas(raw_cas),
		raw_name = str_remove(orig_name, "\\([^()]*\\)$") %>%
			str_to_title()
	)

cur_cas <- 
	cur_analyte %>% 
	filter(!is.na(cas_chk)) %>% 
	pull(cas_chk) %>% 
	ct_search(query = ., 'POST')

cur_analyte %>% 
	filter(!is.na(cas_chk)) %>%
	filter(cas_chk %ni% cur_cas$searchValue)

chems <- cur_analyte$chems %>%
	left_join(., chems_search, join_by('is_cas' == 'raw_search')) %>%
	select(
		'orig_name',
		'idx',
		'n',
		'dtxsid',
		'casrn',
		'preferredName'
	)

biol <- cur_analyte$biol %>%
	separate_wider_delim(
		.,
		cols = raw_cas,
		delim = '-',
		names = c('service', 'ident')
	)

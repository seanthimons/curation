# Diagnostics ------------------------------------------------------------

tbl(eco_con, 'results') %>%
  #filter(is.na(conc1_unit)) %>%
  filter(conc1_unit == 'ml/305 m') %>%
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
  glimpse() %>%
  View()


dbListTables(eco_con) %>%
  .[str_detect(., 'endpoint')] %>%
  print(.) %>%
  map(., ~ tbl_glimpse(.x))


tbl(eco_con, 'app_effect_groups_and_measurements') %>%
  collect() %>%
  View()

tbl(eco_con, 'app_effect_groups') %>%
  mutate(
    effect_group = str_sub(group_effect_term_s, 1, 3)
  ) %>% 
	rename(term = group_effect_term_s) %>% 
	collect() %>% 
	separate_longer_delim(cols = c(term, description), delim = "/") %>% 
	separate_longer_delim(cols = c(term, description), delim = ",") %>% 
	mutate(
		across(
			c(description, term), ~str_squish(.x))
		) %>% 
	distinct() %>% 
	arrange(effect_group) %>% 
	print(n = Inf)


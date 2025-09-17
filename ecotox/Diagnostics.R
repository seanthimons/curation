# Diagnostics ------------------------------------------------------------

tbl(eco_con, 'results') %>%
  #filter(is.na(conc1_unit)) %>%
  filter(conc1_unit == 'no/g soil') %>%
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
  glimpse() %>%	View()


dbListTables(eco_con) %>%
  .[str_detect(., 'endpoint')]

media <- tbl(eco_con, 'tests') %>% 
	select(organism_habitat) %>% 
	distinct() %>% 
	collect()

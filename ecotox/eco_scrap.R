dbListTables(eco_con) %>% sort()

tbl(eco_con, 'exposure_type_codes')
tbl(eco_con, 'app_exposure_types')

tbl(eco_con, 'species')
tbl(eco_con, 'app_lifestages') %>% print(n = Inf)
tbl(eco_con, 'lifestage_codes')


dbListTables(eco_con) %>%
  sort() %>%
  .[str_detect(., pattern = 'app_')]

tbl(eco_con, 'tests') %>%
  glimpse()
pull('test_type') %>%
  unique()

tbl(eco_con, 'results') %>%
  pull(endpoint) %>%
  unique() %>%
  .[str_detect(., pattern = 'NO')]
glimpse()

tbl(eco_con, 'endpoint_codes') %>%
  collect() %>%
  print(n = Inf)

eco_risk_tbl %>%
  filter(
    eco_group == 'Mammals',
    life_stage == 'Adult' | life_stage == 'Other/Unknown',
    endpoint_group == 'NOEL | NOEC'
  ) %>%

  group_by(
    eco_group,
    life_stage,
    #org_lifestage,
    endpoint_group
  ) %>%
  reframe(
    n = n(),
    dur_mode = Mode(new_dur),
    dur_min = min(new_dur),
    dur_mean = mean(new_dur),
    dur_max = max(new_dur)
  ) %>%
  #arrange(eco_group, life_stage) %>%
  #knitr::kable(.) %>%
  print(n = Inf)

eco_risk_tbl %>%
  filter(
    # eco_group == 'Fish',
    # common_name == 'Zebra Danio',
    # new_unit == 'mg/L'
  ) %>%
  group_by(
    eco_group,
    common_name,
    #endpoint_group,
    #new_unit
  )
reframe(
  #n = n(),
  conc = mean(as.numeric(new_value))
) %>%
  pivot_wider(names_from = endpoint_group, values_from = conc) %>%
  print(n = Inf)

#multigeneration----

tbl(eco_con, 'tests') %>%
  select(
    'test_id',
    'test_cas',
    'species_number',
    'exposure_type',
    'test_type',
    'organism_lifestage'
  ) %>%
  filter(
    organism_lifestage != 'F11',
    str_detect(organism_lifestage, 'F0|F1')
  ) %>%
  inner_join(
    tbl(eco_con, 'chemicals'),
    join_by(test_cas == cas_number)
  ) %>%
  inner_join(
    tbl(eco_con, "species") %>%
      select(
        'species_number',
        'common_name',
        'latin_name',
        'ecotox_group'
      ),
    join_by('species_number')
  ) %>%
  inner_join(
    tbl(eco_con, 'results') %>%
      select(
        'result_id',
        'test_id',
        'obs_duration_mean',
        'obs_duration_unit',
        'endpoint',
        'effect',
        'conc1_mean',
        'conc1_unit'
      ),
    join_by('test_id')
  ) %>%
  collect() %>%
  group_by(
    chemical_name,
    #common_name,
    organism_lifestage
  ) %>%
  summarize(
    n = n()
  ) %>%
  arrange(organism_lifestage) %>%
  pivot_wider(names_from = organism_lifestage, values_from = n)



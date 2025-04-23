dbListTables(eco_con) %>% sort()

tbl(eco_con, 'exposure_type_codes')
tbl(eco_con, 'app_exposure_types')

tbl(eco_con, 'species')
tbl(eco_con, 'app_lifestages')
tbl(eco_con, 'lifestage_codes')


dbListTables(eco_con) %>%
  sort() %>%
  .[str_detect(., pattern = 'app_')]

tbl(eco_con, 'tests') %>% 
  glimpse()
pull('test_type') %>% 
  unique()

tbl(eco_con, 'results') %>%
  pull(endpoint) %>% unique() %>% 
  .[str_detect(., pattern = 'NO')]
glimpse()

tbl(eco_con, 'endpoint_codes') %>% 
  collect() %>% 
  print(n = Inf)

eco_risk_tbl %>% 
  group_by(eco_group, life_stage) %>% 
  reframe(
    n = n(),
    dur_mode = Mode(new_dur),
    dur_min = min(new_dur),
    dur_mean = mean(new_dur),
    dur_max = max(new_dur)
  ) %>% 
  arrange(eco_group, life_stage) %>% 
  knitr::kable(.) %>% 
  print(n = Inf)

eco_risk_tbl %>% 
  filter(eco_group == 'Mammals') %>% 
  group_by(
    common_name,
    endpoint_group,
    new_unit) %>% 
  reframe(
    n = n(),
    conc = mean(as.numeric(new_value))
  )


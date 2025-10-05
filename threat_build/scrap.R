dbListTables(threat_db)

tbl(threat_db, "toxval_v97_0") %>%
  glimpse()

cwa <- ct_list('PRODWATER') %>%
  as_tibble() %>%
  select(dtxsids = value) %>%
  duckdb::dbWriteTable(
    threat_db,
    name = 'cwa',
    value = .,
    overwrite = TRUE,
    temporary = TRUE
  )

cwa <- tbl(threat_db, "cwa")

elements <- ct_list('ELEMENTS') %>% 
	append(., c('DTXSID5024217', 'DTXSID5024219')) %>% 
  as_tibble() %>% 
  select(dtxsids = value) %>%
  duckdb::dbWriteTable(
    threat_db,
    name = 'elements',
    value = .,
    overwrite = TRUE,
    temporary = TRUE
  )

elements <- tbl(threat_db, "elements")

best <- tbl(threat_db, "toxval_v97_0") %>%
  semi_join(elements, join_by('DTXSID' == 'dtxsids')) %>%
  filter(
    str_detect(SOURCE, "ECOTOX|RSL|DOD|NJ DEP", negate = TRUE),
    TOXVAL_TYPE != 'MCL',
    #SPECIES_COMMON == 'Human',
    #EXPOSURE_ROUTE == "oral",
    RISK_ASSESSMENT_CLASS == 'Water',
    TOXVAL_UNITS == "mg/L",
  ) %>%
	mutate(TOXVAL_NUMERIC = as.numeric(TOXVAL_NUMERIC)) %>%
  group_by(
    DTXSID,
    NAME
  ) %>%
  slice_min(order_by = TOXVAL_NUMERIC, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    DTXSID,
    NAME,
    TOXVAL_TYPE,
    SOURCE,
    SUB_SOURCE,
    TOXVAL_NUMERIC,
    UNITS = TOXVAL_UNITS
  ) %>%
  #head(n = 20) %>%
  collect() %>%
  print(n = Inf)

mcl <- tbl(threat_db, "toxval_v97_0") %>%
  semi_join(elements, join_by('DTXSID' == 'dtxsids')) %>%
  filter(
    (str_detect(SOURCE, 'EPA') || str_detect(SUB_SOURCE, 'EPA')),
    TOXVAL_TYPE == 'MCL',
    #SPECIES_COMMON == 'Human',
    #EXPOSURE_ROUTE == "oral",
    RISK_ASSESSMENT_CLASS == 'Water',
    TOXVAL_UNITS == "mg/L",
  ) %>%
	mutate(TOXVAL_NUMERIC = as.numeric(TOXVAL_NUMERIC)) %>%
  group_by(
    DTXSID,
    NAME
  ) %>%
  slice_min(order_by = TOXVAL_NUMERIC, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    DTXSID,
    NAME,
    TOXVAL_TYPE,
    SOURCE,
    SUB_SOURCE,
    TOXVAL_NUMERIC,
    UNITS = TOXVAL_UNITS
  ) %>%
  #head(n = 20) %>%
  collect() %>%
  print(n = Inf)

sources <- tbl(threat_db, "toxval_v97_0") %>%
  semi_join(elements, join_by('DTXSID' == 'dtxsids')) %>%
	select(SOURCE, SUB_SOURCE, TOXVAL_TYPE, RISK_ASSESSMENT_CLASS) %>%
	distinct() %>% 
	collect() #%>% View()

cols <- tbl(threat_db, "toxval_v97_0") %>%
  semi_join(elements, join_by('DTXSID' == 'dtxsids')) %>%
  filter(
    (str_detect(SOURCE, 'EPA') || str_detect(SUB_SOURCE, 'EPA')),
    TOXVAL_TYPE == 'MCL',
    #SPECIES_COMMON == 'Human',
    #EXPOSURE_ROUTE == "oral",
    RISK_ASSESSMENT_CLASS == 'Water',
    TOXVAL_UNITS == "mg/L",
  ) %>%
	collect() %>% 
	janitor::remove_empty(., which = c("rows", "cols"))

dbListTables(threat_db)

tbl(threat_db, "toxval_v96_1") %>%
  #filter(DTXSID == 'DTXSID2021238') %>%
  #filter()
  glimpse()

cwa <- ct_list('PRODWATER') %>% 
	as_tibble() %>% 
	select(dtxsids = value) %>% 
duckdb::dbWriteTable(
	threat_db, name = 'cwa', value = ., overwrite = TRUE, temporary = TRUE
)

cwa <- tbl(threat_db, "cwa")


{
  tbl(threat_db, "toxval_v96_1") %>%
		semi_join(cwa, join_by('DTXSID' =='dtxsids')) %>% 
    filter(
      SOURCE != "ECOTOX",
      SPECIES_COMMON == 'Human',
      EXPOSURE_ROUTE == "oral",
      RISK_ASSESSMENT_CLASS == 'Water',
      TOXVAL_UNITS == "mg/L",
    ) %>%
    group_by(
      DTXSID,
      TOXVAL_TYPE,
      SOURCE,
    ) %>%
    summarize(
      TOXVAL_NUMERIC = min(as.numeric(TOXVAL_NUMERIC), na.rm = TRUE),
      UNITS = first(TOXVAL_UNITS)
    ) %>%
    arrange(desc(TOXVAL_NUMERIC)) %>%
    head(n = 20) %>%
    collect() %>%
    print(n = Inf)
}

{
  tbl(threat_db, "toxval_v96_1") %>%
    select(
      SOURCE
      #,SUB_SOURCE,
      #TOXVAL_TYPE,
      #TOXVAL_SUBTYPE
    ) %>%
    distinct() %>%
    arrange(SOURCE) %>%
    print(n = Inf)
  # filter(
  #  SOURCE %in% c('ECHA IUCLID', 'ChemIDplus')) %>%
  #   select(!contains('ORIGINAL')) %>%
  #   distinct(SUB_SOURCE)
}

{
  tbl(threat_db, "toxval_v96_1") %>%
    filter(
      SOURCE %in% c('ToxRefDB')
    ) %>%
    select(!contains('ORIGINAL')) %>%
    glimpse()
  #distinct(SUB_SOURCE)
}

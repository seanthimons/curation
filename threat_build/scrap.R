dbListTables(threat_db)

tbl(threat_db, "toxval_9_6") %>%
  #filter(DTXSID == 'DTXSID2021238') %>% 
  filter()
  glimpse()

cwa <- ct_list('CWA311HS') %>% pluck(.,1)

{
tbl(threat_db, "toxval_9_6") %>%
  filter(
    SOURCE != "ECOTOX",
    DTXSID %in% cwa$dtxsids,
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
    TOXVAL_NUMERIC = min(as.numeric(TOXVAL_NUMERIC), na.rm = TRUE)
    ,UNITS = first(TOXVAL_UNITS)
    ) %>%
  arrange(desc(TOXVAL_NUMERIC)) %>%
  head(n = 20) %>% 
  collect() %>% 
  print(n = Inf)
}

{
  
tbl(threat_db, "toxval_9_6") %>%
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
  
  tbl(threat_db, "toxval_9_6") %>%
  filter(
   SOURCE %in% c('ToxRefDB')) %>%
    select(!contains('ORIGINAL')) %>% 
    glimpse()
    #distinct(SUB_SOURCE)
  
}

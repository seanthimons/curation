lot <- dbListTables(threat_db)

loc <- lot %>%
  map(
    .,
    ~ {
      tbl(threat_db, .x) %>%
        colnames()
    }
  )

tbl(threat_db, lot[2]) %>%
  #filter(DTXSID == 'DTXSID8020913') %>%
  distinct(QC_STATUS) %>%
  collect() %>%
  print(n = Inf)

symdiff(loc[[1]], loc[[2]])

setdiff(loc[[1]], loc[[2]])

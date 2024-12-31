map2(lof, f_names, function(x,y){
  tp <- read_delim_arrow(file = here('ecotox', x), delim = '|', read_options = list(encoding = "latin1"))
  write_parquet(tp, paste0(here('ecotox', 'pq', y), '.parquet'))
  gc()
} ,.progress = T)

map(f_names, function(x){
  cat(x, '\n')
  
  dbWriteTable(con, x, read_parquet(paste0(here('ecotox', 'pq', x), '.parquet')), overwrite = TRUE)
  
  # tbl(con, sql(sprintf("SELECT * FROM read_parquet('%s')", paste0(here('ecotox', 'pq', x), '.parquet')))) %>%
  #   compute(name = x, temporary = FALSE)
}, .progress = T)

dbListTables(con)

# validation --------------------------------------------------------------

setwd(here('ecotox', 'validation'))

lof <- list.files(pattern = '.txt') %>% .[str_detect(., pattern = 'release', negate = T)]

f_names <- str_remove_all(lof, pattern = '.txt')

dir.create(here('ecotox', 'pq_val'))

map2(lof, f_names, function(x,y){
  tp <- read_delim_arrow(file = here('ecotox', 'validation', x), delim = '|', read_options = list(encoding = "latin1"))
  
  dbWriteTable(con, x, read_parquet(paste0(here('ecotox', 'pq_val', x), '.parquet')), overwrite = TRUE)

} ,.progress = T)


map(f_names, function(x){
  cat(x, '\n')
  
  dbWriteTable(con, x, read_parquet(paste0(here('ecotox', 'pq_val', x), '.parquet')), overwrite = TRUE)
  
  # tbl(con, sql(sprintf("SELECT * FROM read_parquet('%s')", paste0(here('ecotox', 'pq', x), '.parquet')))) %>%
  #   compute(name = x, temporary = FALSE)
}, .progress = T)

chemicals <- read_parquet(here('ecotox', 'pq_val', 'chemicals.parquet')) %>% 
  mutate(cas_number = as.character(cas_number))

dbWriteTable(con, 'chemicals', chemicals, overwrite = T)

tests <- read_parquet(here('ecotox', 'pq', 'tests.parquet')) %>% 
  mutate(cas_number = as.character(cas_number))

dbWriteTable(con, 'tests', tests, overwrite = T)

DBI::dbDisconnect(con, shutdown = TRUE)


{
  "id": 0,
  "source": "string",
  "year": "string",
  "studyDurationValue": 0,
  "studyDurationClass": "string",
  "toxvalNumericQualifier": "string",
  "studyDurationUnits": "string",
  "riskAssessmentClass": "string",
  "dtxsid": "string",
  "exposureRoute": "string",
  "toxvalNumeric": 0,
  "subsource": "string",
  "toxvalType": "string",
  "toxvalSubtype": "string",
  "toxvalUnits": "string",
  "studyType": "string",
  "sourceUrl": "string",
  "subsourceUrl": "string",
  "priorityId": 0,
  "criticalEffect": "string",
  "generation": "string",
  "exposureMethod": "string",
  "detailText": "string",
  "population": "string",
  "strain": "string",
  "media": "string",
  "sex": "string",
  "exposureForm": "string",
  "lifestage": "string",
  "supercategory": "string",
  "speciesCommon": "string",
  "humanEcoNt": "string"
}

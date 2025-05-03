{
  library(plumber)
  library(duckdb)
  library(duckplyr)
  library(DBI)
  library(magrittr)
}
options("plumber.port" = 5555)

#* @apiTitle Testing API for EcoTox

#* Health Check
#* @get /health-check
health <- function() {

  db = list.files(pattern = '.duckdb')

  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = FALSE)
  on.exit(dbDisconnect(con))

  list(
    timestamp = Sys.time(),
    db = list.files(pattern = '.duckdb'),
    created = tbl(con, 'versions') %>% filter(latest == TRUE) %>% pull(date),
    db_size_mb = round(file.size(db)/(1024*1024), 2)
  )
}

#* Full list of chemicals currently in EcoTox
#* @get /inventory
inventory <- function() {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- tbl(con, 'chemicals') %>% collect()
  return(result)
}

#* Get a list of available tables within database
#* @get /all_tbls
all_tbls <- function() {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- DBI::dbListObjects(con) %>%
    .$table %>%
    map(
      .,
      ~ pluck(., 'name') %>%
        unname()
    ) %>%
    unlist()

  return(result)
}

#* Retrieve column names for a given table
#* @get /fields/<table_name:str>
#* @param table_name:str
fields <- function(table_name) {
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = "ecotox.duckdb",
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con))

  result <- DBI::dbListFields(con, table_name)

  return(result)
}


#* Retrieve table from database by name
#* @get /tables/<table_name:str>
#* @param table_name:str
get_tbl <- function(table_name) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  result <- tbl(con, table_name) %>% collect()
  return(result)
}


get_results <- function(query, habitat = NA) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "ecotox.duckdb", read_only = TRUE)
  on.exit(dbDisconnect(con))

  if (missing(habitat) | is.na(habitat)) {
    habitat <- c('Non-Soil', 'Soil', 'Water')
  }

  result = tbl()
  return(result)
}

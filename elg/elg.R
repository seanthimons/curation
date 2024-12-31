# packages ----------------------------------------------------------------

{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  #library(ComptoxR)
}

setwd(here('elg'))

# functions ---------------------------------------------------------------

modify_list <- function(x) {
  if (is.null(x)) {
    return(as.logical(NA))
  } else {
    return(x)
  }
}


endpoints <- c(
  'pointSourceCategories',
  'pollutants',
  'pollutantCategories',
  'treatmentTechnologies',
  'treatmentTechnologyCategories'
)

# raw ---------------------------------------------------------------------


dat <- map(endpoints, ~{
  resp <- GET(url = paste0('https://owapps.epa.gov/elg/api/', .x))
  .x <- content(resp) %>% 
    map(., as_tibble) %>% 
    list_rbind()
}, .progress = T) %>% 
  set_names(., snakecase::to_snake_case(endpoints))

rm(endpoints)

# polls <- dat$pollutants$pollutantId[1:10]
# 
# poll_limits <- map(polls, ~{
#   resp <- GET(url = paste0('https://owapps.epa.gov/elg/api/pollutant/?id=', .x))
#   .x <- content(resp)
# }, .progress = T) %>%
#   set_names(., polls)
# 
# poll_ranges <- poll_limits %>%
#   map(., ~pluck(., 'ranges')) %>% 
#   compact()
# 
# poll_limits_temp <- poll_limits %>% 
#   map(., ~pluck(., 'pscs'))
# 
# temp <- poll_limits_temp %>% 
#   map(.,
#       ~map(., 
#         ~{as_tibble(.x)}))


# Pollutant categories ----------------------------------------------------


pollutant_categories <- map(dat$pollutant_categories$id, ~{
  resp <- GET(url = paste0('https://owapps.epa.gov/elg/api/pollutantCategory/?id=', .x))
  .x <- content(resp)
}, .progress = T) %>%
  set_names(., dat$pollutant_categories$description) %>% 
  map(., ~pluck(., 'pscs'))%>% 
  map(., ~map(., as_tibble)) %>% 
  map(., list_rbind) %>% 
  list_rbind(names_to = 'super_category') %>% 
  unnest_wider(., col = rangeOfPollutantLimitations)


## temp --------------------------------------------------------------------


poll_temp <- pollutant_categories %>% 
  distinct(pollutantId, pointSourceCategoryCode) #%>%  slice_head(., n = 30)


## Pollutant limitations ---------------------------------------------------


poll_limits <- map2(poll_temp$pollutantId, poll_temp$pointSourceCategoryCode, function(id, code){
  resp <- GET(url = paste0('https://owapps.epa.gov/elg/api/pollutantLimitations?pollutantId=', id,'&pointSourceCategoryCode=', code))
  df <- content(resp)
  return(df)
}, .progress = T) %>% 
  map_depth(., 3, modify_list) %>% 
  map_depth(., 2, as_tibble) %>% 
  map_depth(., 1, list_rbind) %>% 
  list_rbind(names_to = 'pollutant_frame')
  #mutate(., longTermAverageCount = if_else(longTermAverageCount == 1, TRUE, FALSE)) %>% 
  #split(.$longTermAverageCount)

poll_temp <- poll_limits %>% 
  filter(longTermAverageCount == 1) %>% 
  select(longTermAverageCount, limitationId) %>% 
  distinct(limitationId) %>% 
  slice_head(., n = 30)

poll_lta <- map(poll_temp$limitationId, function(id){
  resp <- GET(url = paste0('https://owapps.epa.gov/elg/api/limitation?id=', id))
  df <- content(resp)
  return(df)
}, .progress = T)


# multisearch -------------------------------------------------------------

cats <- dat$point_source_categories$pointSourceCategoryCode

walk(cats, function(id){
  cat(id, '\n')
  download.file(
    url = paste0('https://owapps.epa.gov/elg/api/multiCriteriaSearch?pointSourceCategoryCode=', id,'&sicCode=&naicsCode=&pollutantId=&pollutantGroupId=&rangeLow=&rangeHigh=&rangeUnitCode=&treatmentTechnologyCode=&treatmentTechnologyGroup=&filterPointSourceCategoryCode=&filterPollutantId=&filterTreatmentId=&download=true'),
    destfile = paste0(here('elg', 'raw'), id, '.xlsx'),
    mode = "wb",
    quiet = T
                )
}, .progress = T)

rm(cats)

lf <- list.files(here('elg'), pattern = '.xlsx')


elg_con <- dbConnect(duckdb(), dbdir = "elg.duckdb", read_only = FALSE)




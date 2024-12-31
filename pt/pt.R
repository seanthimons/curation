# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(rvest)
library(httr)
library(polite)
library(rio)
library(ComptoxR)

`%ni%` <- Negate(`%in%`)

pt <- list()

# oxidation states --------------------------------------------------------

ind_html <-
  polite::bow('https://en.wikipedia.org/wiki/Oxidation_state#List_of_oxidation_states_of_the_elements') %>% 
  polite::scrape(.) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table()

pt$oxidation_state <- ind_html[[3]] %>%
  .[4:121,1:19] %>%
  setNames(., LETTERS[1:19]) %>%
  as_tibble() %>%
  mutate(
    I = as.character(I),
    A = as.numeric(A)) %>%
  mutate(across(B:S, ~na_if(., ""))) %>%
  pivot_longer(., cols = D:R, values_to = 'oxidation_state', values_drop_na = T) %>%
  select(-name) %>%
  setNames(., c('number', 'element', 'symbol', 'group', 'oxs')) %>%
  mutate(
    ox = case_when(
      stringr::str_detect(oxs, "\\+") ~ "+",
      stringr::str_detect(oxs, "\\u2212") ~ "-",
      .default = " "),
    oxs = stringr::str_remove_all(oxs, "[[:symbol:]]"),
    oxidation_state = paste0(oxs,ox) %>% str_trim(),
    search = case_when(
      oxidation_state == '0' ~ symbol,
      oxidation_state == '1-' ~ paste0(symbol,ox),
      oxidation_state == '1+' ~ paste0(symbol,ox),
      .default = paste0(symbol,oxidation_state)
    )
    
  ) %>%
  select(!c(oxs:ox))

rm(ind_html)

# Table -------------------------------------------------------------------

pt$elements <- bow('https://en.wikipedia.org/wiki/List_of_chemical_elements') %>% 
  scrape(content="text/html; charset=UTF-8") %>% 
  html_nodes(".wikitable") %>% html_table() %>% 
  pluck(1) %>% 
  set_names(., c(
    'Number',
    'Symbol',
    'Name',
    'Origin of name',
    'Group',
    'Period',
    'Block',
    'Standard atomic weight',
    'Density',
    'Melting point',
    'Boiling point',
    'Specific heat capacity',
    'Electro negativity',
    'Abundance',
    'Origin',
    'Phase'
  )) %>% 
  slice(., 4:nrow(.)) %>% 
  mutate(Number = as.numeric(Number))

pt_dtxsid <- ComptoxR::ct_search(type = 'string', search_param = 'equal', query = pt$elements$Name)

pt_dtxsid_final <- pt_dtxsid %>%
  arrange(., rank) %>% 
  distinct(searchValue, .keep_all = T) %>% 
  select(dtxsid, casrn, preferredName, searchValue)

pt$elements <- left_join(pt$elements, pt_dtxsid_final, by = c('Name' = 'searchValue')) %>% 
  mutate(Number = as.character(Number))

rm(pt_dtxsid, pt_dtxsid_final)

# Nuclides ----------------------------------------------------------------

# nuclides <- bow('https://en.wikipedia.org/wiki/List_of_radioactive_nuclides_by_half-life') %>% 
#   scrape(content="text/html; charset=UTF-8") %>%
#   html_nodes(".wikitable") %>% html_table() %>% 
#   map(., ~select(., 'isotope')) %>% 
#   list_rbind() %>% 
#   pluck(1) %>% 
#   as_tibble() %>% 
#   #removes citations + comments
#   mutate(., value = str_remove_all(value, pattern  = regex('\\[(.*?)\\]|\\((.*?)\\)|\\{(.*?)\\}'))) %>% 
#   filter(str_detect(value, pattern = '-')) %>% 
#   separate_wider_delim(., cols = 'value', delim = '-', names = c('elements', 'iso_num')) %>% 
#   filter(!str_detect(iso_num, '[[:punct:]]')) %>% 
#   mutate(elements = snakecase::to_sentence_case(elements)) %>% 
#   left_join(., pt$elements %>% select(Symbol:Name), by = join_by(elements == Name)) %>%
#   mutate(., 
#          isotopes = paste0(elements, '-', iso_num),
#          isotope_symbol = paste0(Symbol, '-', iso_num),
#          isotope_mass_symbol = paste0(iso_num, '-', Symbol),
#          .keep = 'none')
# 
# stable <- bow('https://en.wikipedia.org/wiki/List_of_nuclides') %>% 
#   scrape(content="text/html; charset=UTF-8") %>%
#   html_nodes(".wikitable") %>% html_table() %>%
#   discard_at(., 1) %>% 
#   keep_at(., 1:3) %>% 
#   map(., ~select(., c('A', 'Z'))) %>% 
#   list_rbind() %>% 
#   mutate(Z = as.character(Z)) %>% 
#   left_join(., select(pt$elements, c(Number, Symbol, Name)), join_by('Z' == 'Number')) %>% 
#   mutate(isotopes = paste0(Name,'-', A),
#          isotope_symbol = paste0(Symbol,'-', A),
#          isotope_mass_symbol = paste0(A,'-', Symbol), .keep = 'none')
# 
# pt$isotopes <- bind_rows(stable, nuclides) %>%
#   distinct(isotopes, .keep_all = T)

nuclides <- bow('https://en.wikipedia.org/wiki/Table_of_nuclides') %>% 
  scrape(content="text/html; charset=UTF-8") %>%
  html_nodes(".wikitable") %>%
  html_table() %>% 
  pluck(., 3) %>%
  .[-1, -1] %>% 
  `colnames<-`(., NULL) %>% 
  as_tibble(., .name_repair = 'universal') %>%
  mutate_all(~na_if(., "")) %>% 
  pivot_longer(., cols = everything(), names_to = 'col', values_to = 'iso', values_drop_na = T) %>% 
  filter(iso %ni% pt$elements$Number, iso %ni% pt$elements$Symbol) %>% 
  mutate(
    Z = str_extract_all(iso, pattern = "\\d+(?:\\.\\d+)?"), 
    element = str_extract_all(iso, pattern = "[^0-9.]+")) %>% 
  select(-iso, -col) %>% 
  unnest(., cols = c(Z, element))

pt$isotopes <- nuclides

# Export ------------------------------------------------------------------

rio::export(pt, file = 'pt.RDS')

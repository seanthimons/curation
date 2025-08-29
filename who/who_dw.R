{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(jsonlite)
  library(tabulapdf)
  library(pdftools)
  library(docxtractr)

  setwd(here("who"))

  `%ni%` <- Negate(`%in%`)
}

path <- '2022 - A Global Overview of National Regulations and Standards for Drinking-Water Quality.pdf'
# path <- 'World Health Organization - 2022 - Guidelines for drinking-water quality.pdf'

raw <- pdftools::pdf_toc(path) %>%
  pluck(., 'children') %>%
  .[c(3:8)]

super <- raw %>%
  map(., ~ pluck(., 'title'))

toc <- raw %>%
  set_names(., super) %>%
  map(., ~ pluck(., 'children')) %>%
  map(., ~ map(., ~ pluck(., 'title'))) %>%
  list_c()

super_org <- raw %>%
  set_names(., super) %>%
  map(., ~ pluck(., 'children')) %>%
  map(., ~ map(., ~ pluck(., 'title')))

# raw <-
# tabulapdf::extract_tables(
#   file = here('who', path),
#   col_names = FALSE,
#   method = 'lattice',
#   #output = 'csv',
#   pages = c(13,13)
#   )

path_word <- '2022 - A Global Overview of National Regulations and Standards for Drinking-Water Quality.docx'

raw_word <- read_docx(here('who', path_word))

tbls_raw <- docx_extract_all_tbls(raw_word, guess_header = F)

# Extract tables from the Word document
tbls_raw <- docx_extract_all_tbls(raw_word, guess_header = F)

# Rename the list of tables with the table of contents names
tbls <- tbls_raw %>%
  set_names(., toc) %>% # Set the names of the list elements
  map(., as_tibble) %>% # Convert each list element to a tibble
  list_rbind(., names_to = 'compound') %>% # Row bind the tibbles together, using 'compound' as the name for the list
  rename(name = V1, value = V2) %>% # Rename the columns of the tibble
  filter(
    !str_detect(name, pattern = 'Number of countries'),
    !str_detect(value, pattern = 'None|No value')
  ) %>% # Filter out rows where the name contains 'Number of countries' or the value contains 'None' or 'No value'
  mutate(orig_value = value) %>% # Create a new column called 'orig_value' and assign it the value of the 'value' column
  separate(
    value,
    into = c("value1", "value2", "value3"),
    sep = "\\s{1,2}",
    extra = "merge"
  ) %>% # Split the 'value' column into three columns called 'value1', 'value2', and 'value3' using one or two whitespace characters as the separator
  rename(value = value1, unit = value2, comment = value3) %>% # Rename the columns
  mutate(
    # Convert the 'value' column to a numeric type
    num_value = readr::parse_number(value),
    # Replace missing 'num_value' with appropriate values based on the 'compound' column
    num_value = case_when(
      str_detect(compound, pattern = 'pH') & is.na(num_value) ~
        as.numeric(unit),
      str_detect(compound, pattern = 'Escherichia coli') & is.na(num_value) ~ 0,
      str_detect(compound, pattern = 'Aluminium') & is.na(num_value) ~ NA,
      .default = num_value,
    ),
    # Replace missing 'unit' with appropriate values based on the 'compound' column
    unit = case_when(
      is.na(unit) &
        compound %ni%
          c(
            'Trihalomethanes (Total)',
            'Temperature',
            'Taste',
            'Escherichia coli (Faecal coliforms, Thermotolerant coliforms)'
          ) ~
        'mg/l',
      is.na(unit) & compound == 'Trihalomethanes (Total)' ~ 'TTHM',
      is.na(unit) & compound == 'Temperature' ~ 'C',
      is.na(unit) & compound == 'Taste' ~ 'DN',
      is.na(unit) &
        compound ==
          'Escherichia coli (Faecal coliforms, Thermotolerant coliforms)' ~
        'CFU/100 ml',
      compound ==
        'Escherichia coli (Faecal coliforms, Thermotolerant coliforms)' ~
        'CFU/100 ml',
      str_detect(compound, pattern = 'pH') ~ 'standard unit',
      unit == 'cfu' ~ 'CFU/ ml',
      unit == 'per' ~ 'CFU/100 ml',
      compound == 'Clostridium perfringens (Sulphite-reducing anaerobes)' &
        name == 'Maximum value set' &
        unit == 'per' ~
        'CFU/20 ml',
      compound == 'Aluminium' & unit == 'GV' ~ 'mg/l',
      unit == 'mg' ~ paste0('mg ', comment),
      unit == "\uf06dS/cm" ~ 'uS/cm',
      unit %in% c('mg/l4', 'mg/l5', 'mg/l6', 'mg/l7') ~ 'mg/l',
      .default = unit
    ),

    # TODO Fix Naming here
    # Create a new column 'comment\_2' based on the 'orig\_value' column
    comment_2 = case_when(
      str_detect(orig_value, pattern = '[*]') ~ 'ASTER',
      .default = NA
    ),
    # Remove any asterisks from the 'unit' column
    unit = str_remove_all(unit, pattern = '[*]')
  )

# filter(is.na(unit)) %>% print(n = Inf)
#
# tbls %>% count(unit) %>% arrange(desc(n)) %>% print(n = Inf)

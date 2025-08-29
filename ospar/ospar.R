{
  library(rvest)
  library(httr)
  library(tidyverse)
  library(rio)
  library(ComptoxR)
  library(here)
}
# OSPAR List of Substances of Possible Concern----
webpage <- read_html(
  "https://www.ospar.org/work-areas/hasec/hazardous-substances/possible-concern/list"
)

t_list <- c(1:4) %>%
  map(
    .,
    ~ {
      .x <- html_nodes(
        webpage,
        xpath = paste0('//*[@id="sections"]/section[', .x, ']/div/table')
      ) %>%
        html_table() %>%
        pluck(., 1)
    }
  ) %>%
  set_names(., LETTERS[1:4]) %>%
  list_rbind(names_to = 'list') %>%
  distinct(., .keep_all = T)

rm(webpage)

# PLONOR----

#Had to work out how to handle the split tables

library(tabulapdf)

pdf_file <- here('ospar', '13-06e_agreement_plonor_update_2024.pdf')

tables <- extract_tables(pdf_file, pages = 4:7, output = "tibble")

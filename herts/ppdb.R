library(rvest)
library(purrr)
library(dplyr)

page <- read_html("https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/4.htm")

# Extract structural components with class detection
components <- page %>%
  html_elements(
  "table[class^='indexpage'], 
  table[class^='report_sub_section'],
  table.report_data"
  ) %>%
  map_dfr(~ tibble(
    type = case_when(
      html_attr(.x, "class") %>% str_detect("indexpage") ~ "section",
      html_attr(.x, "class") %>% str_detect("report_sub_section") ~ "subsection",
      TRUE ~ "data"
    ),
    content = list(.x)
  ))

# Title parser using proper class selectors
title_parser <- function(node, type) {
  selector <- switch(type,
                     "section" = "td.indexpage",
                     "subsection" = "td.report_sub_section",
                     "data" = NA_character_
  )
  if (!is.na(selector)) html_text2(html_element(node, selector)) else NA_character_
}


# Build hierarchical structure
hierarchy <- components %>%
  mutate(
    title = map2_chr(content, type, title_parser),
    parent_section = cumsum(type == "section") %>% 
      ifelse(type == "section", ., NA_integer_),
    current_subsection = cumsum(type == "subsection") %>% 
      ifelse(type == "subsection", ., NA_integer_)
  ) %>%
  fill(parent_section, current_subsection, .direction = "down")

# Data table parser with header detection
parse_data_table <- function(table_node) {
  rows <- html_elements(table_node, "tr")
  
  # Find header row (first row with th elements)
  header_row <- detect_index(rows, ~ length(html_elements(.x, "th")) > 0)
  
  headers <- rows[[header_row]] %>%
    html_elements("th") %>%
    html_text2() %>%
    trimws()
  
  data_rows <- rows[-seq_len(header_row)] %>%
    map(~ html_elements(.x, "td") %>%
          html_text2() %>%
          trimws() %>%
          set_names(headers[1:length(.)]) %>%
          as.list()
    ) %>%
    bind_rows()
  
  return(data_rows)
}

# Combine data with hierarchy
final_data <- hierarchy %>%
  filter(type == "data") %>%
  mutate(
    section_title = hierarchy$title[parent_section],
    subsection_title = hierarchy$title[current_subsection],
    table_data = map(content, parse_data_table)
  ) %>%
  select(section_title, subsection_title, table_data)

# Access results
final_data %>%
  filter(!map_lgl(table_data, is.null)) %>%
  tidyr::unnest(table_data)


# attempt -----------------------------------------------------------------

library(rvest)
library(purrr)
library(dplyr)
library(stringr)

page <- read_html("https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/4.htm")

# Extract all relevant tables and their hierarchy
tables <- page %>% 
  html_elements("table.report_section, table.report_data") %>% 
  map_dfr(~ {
    parent <- html_element(.x, xpath = "./ancestor::table[contains(@class, 'indexpage')]")
    tibble(
      type = if_else(html_attr(.x, "class") == "report_data", "data", "section"),
      parent_class = html_attr(parent, "class") %||% NA_character_,
      content = list(.x)
    )
  })

# Enhanced title parser for nested structure
parse_titles <- function(node) {
  # Check for section titles in multiple possible locations
  title <- node %>% 
    html_element(xpath = ".//td[contains(@class, 'report_section')] | 
                           .//td[contains(@class, 'indexpage')] | 
                           .//td[contains(@class, 'report_sub_section')]") %>% 
    html_text2() %>% 
    str_remove_all("[\r\n\t]") %>% 
    str_trim()
  
  if (is.na(title)) {
    # Fallback for deeply nested titles
    title <- node %>% 
      html_element(xpath = ".//td[contains(text(), 'General Information') or 
                                 contains(text(), 'Properties')]") %>% 
      html_text2() %>% 
      str_trim()
  }
  
  return(title %||% NA_character_)
}

# Parse data tables with header validation
parse_data_table <- function(table_node) {
  # Get all rows including potential header
  rows <- html_elements(table_node, "tr")
  
  # Find header row (first row with th elements)
  header_row <- detect_index(rows, ~ length(html_elements(.x, "th")) > 0)
  
  # Extract headers safely
  headers <- rows[[header_row]] %>%
    html_elements("th") %>%
    html_text2() %>%
    trimws() %>%
    discard(~ . == "")
  
  # Process data rows with column count validation
  data_rows <- rows[-seq_len(header_row)] %>%
    map(~ html_elements(.x, "td") %>%
          html_text2() %>%
          trimws() %>%
          `length<-`(max(length(.), length(headers))) %>%  # Pad columns
          set_names(headers[seq_along(.)]) %>%             # Safe naming
          as.list()
    ) %>%
    bind_rows()
  
  return(data_rows)
}


# Build hierarchical data structure
results <- tables %>% 
  mutate(
    section_title = case_when(
      type == "section" ~ map_chr(content, parse_titles),
      TRUE ~ NA_character_
    ),
    data_table = if_else(
      type == "data", 
      map(content, possibly(parse_data_table, NULL)),  # Error handling
      list(NULL)
    )
  ) %>% 
  fill(section_title, .direction = "down") %>% 
  filter(!map_lgl(data_table, is.null))

# Access results
final_data <- results %>% 
  tidyr::unnest(data_table)

# View first section's data
final_data %>% 
  filter(section_title == first(na.omit(section_title))) %>% 
  head()





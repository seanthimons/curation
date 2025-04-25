library(rvest)
library(tidyverse)
library(xml2)

# URL of the webpage
url <- 'https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/4.htm'

# Function to extract table titles
extract_table_title <- function(table_node, title_selector) {
  title_node <- html_node(table_node, title_selector)
  if (!is.na(title_node)) {
    return(html_text(title_node, trim = TRUE))
  } else {
    return(NA_character_)
  }
}

# Function to extract data from report tables
extract_report_data <- function(table_node) {
  rows <- html_nodes(table_node, "tr")
  
  # Extract data from each row
  data <- lapply(rows, function(row) {
    cells <- html_nodes(row, "td, th")
    cell_values <- html_text(cells, trim = TRUE)
    return(cell_values)
  })
  
  # Convert to a data frame
  df <- as.data.frame(do.call(rbind, lapply(data, function(x) {
    length(x) <- max(lengths(data))
    return(x)
  })), stringsAsFactors = FALSE)
  
  return(df)
}

# Function to recursively process the webpage
process_page <- function(url) {
  page <- read_html(url)
  
  # Extract main and sub section titles
  main_section_titles <- html_nodes(page, "table.indexpage .report_section_title") %>%
    html_text(trim = TRUE)
  subsection_titles <- html_nodes(page, "table.report_sub_section .report_sub_section_title") %>%
    html_text(trim = TRUE)
  
  # Extract report data tables
  report_data_tables <- html_nodes(page, "table.report_data")
  
  # Initialize lists
  section_titles <- list()
  main_index <- 1
  sub_index <- 1
  
  # Iterate through report data tables and assign titles
  for (i in seq_along(report_data_tables)) {
    main_title <- ifelse(main_index <= length(main_section_titles), main_section_titles[main_index], NA_character_)
    sub_title <- ifelse(sub_index <= length(subsection_titles), subsection_titles[sub_index], NA_character_)
    
    section_titles[[i]] <- c(main_title, sub_title)
    
    # Increment the indexes only when the title is available
    if (main_index <= length(main_section_titles)) {
      main_index <- main_index + 1
    }
    if (sub_index <= length(subsection_titles)) {
      sub_index <- sub_index + 1
    }
  }
  
  report_data <- lapply(report_data_tables, function(table) {
    data <- extract_report_data(table)
    return(list(type = "report_data", content = data))
  })
  
  return(list(
    section_titles = section_titles,
    report_data = report_data
  ))
}

# Process the page
extracted_data <- process_page(url)

# Print the extracted data
print(extracted_data$section_titles)
print(extracted_data$report_data)

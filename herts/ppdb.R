library(rvest)
library(tidyverse)
library(xml2)

# URL of the webpage.  Switch to local file for testing.
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
  df <- as.data.frame(
    do.call(
      rbind,
      lapply(data, function(x) {
        length(x) <- max(lengths(data))
        return(x)
      })
    ),
    stringsAsFactors = FALSE
  )

  return(df)
}

# Recursive function to process the webpage
process_section <- function(node) {
  section_data <- list()

  # Check if the node is a main section
  if (inherits(node, "xml_node")) {
    class_attr <- html_attr(node, "class")

    if (!is.na(class_attr) && "indexpage" %in% strsplit(class_attr, " ")[[1]]) {
      title <- extract_table_title(node, ".report_section_title")

      # Find subsections within this main section
      subsection_nodes <- html_nodes(node, "table.report_sub_section")
      subsections <- lapply(subsection_nodes, process_section)

      # Find report data tables within this main section
      report_data_tables <- html_nodes(node, "table.report_data")
      report_data <- lapply(report_data_tables, function(table) {
        data <- extract_report_data(table)
        return(data)
      })

      # Add subsections and report data to the section data
      if (length(subsections) > 0 || length(report_data) > 0) {
        section_data[[title]] <- list()
        if (length(subsections) > 0) {
          section_data[[title]]$subsections <- subsections
        }
        if (length(report_data) > 0) {
          section_data[[title]]$report_data <- report_data
        }
      }
    } else if (
      !is.na(class_attr) &&
        "report_sub_section" %in% strsplit(class_attr, " ")[[1]]
    ) {
      # Check if the node is a subsection
      title <- extract_table_title(node, ".report_sub_section_title")

      # Find report data tables within this subsection
      report_data_tables <- html_nodes(node, "table.report_data")
      report_data <- lapply(report_data_tables, function(table) {
        data <- extract_report_data(table)
        return(data)
      })

      # Add report data to the section data
      if (length(report_data) > 0) {
        section_data[[title]] <- list(report_data = report_data)
      }
    }
  }

  return(section_data)
}

# Read the webpage
page <- read_html(url)

# Find main sections
index_tables <- html_nodes(page, "table.indexpage")

# Process each main section
nested_data <- lapply(index_tables, process_section)

# Print the nested data
print(nested_data)

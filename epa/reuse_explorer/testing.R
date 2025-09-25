q1 <- lf[9]

testing <- map(q1, ~{

	.x$head_url <-
  httr2::request(.x$Url) %>%
  httr2::req_method('HEAD') %>%
  httr2::req_perform() %>%
  pluck(., 'url')

	raw_tbl <- .x$head_url %>%
  bow(.) %>%
  scrape(content = "text/html; charset=UTF-8") %>%
  html_element(., xpath = '//*[@id="table1"]')






# 1. Extract citation text from the page (usually in a list after the table).
citation_p_nodes <- raw_tbl %>%
  xml2::xml_parent() %>%
  rvest::html_elements(xpath = "following-sibling::p[sup]")

citations_df <- NULL
# Process each footnote paragraph found.
citations_df <- citation_p_nodes %>%
  map(
    ~ {
      full_text <- rvest::html_text(.x, trim = TRUE)

      marker <- .x %>%
        rvest::html_element("sup") %>%
        rvest::html_text()

      citation_text <- full_text %>%
        stringr::str_remove(marker) %>%
        stringr::str_trim()

      tibble(citation_marker = marker, citation_text = citation_text)
    }
  )
})

# 2. Process the main table row-by-row, handling rowspans correctly.
table_rows <- rvest::html_elements(raw_tbl, "tbody tr")

# -- FIX APPLIED HERE --
# We need to store the values from the spanned cells as we iterate.
# Initialize a list to hold the text of the cells that span multiple rows.
spanned_values <- list(class = NA_character_, source = NA_character_)

table_data <- table_rows %>%
  map_dfr(
    # Using map_dfr to combine the results into a single tibble
    ~ {
      # In this map, `.x` represents a single row node (<tr>).
      cells <- rvest::html_elements(.x, "td")

      if (length(cells) == 0) {
        return(NULL)
      }

      # First, handle all the superscript logic for the current row's cells.
      # This modifies the XML nodes in memory before we extract any text.
      sup_node <- rvest::html_element(.x, "sup")
      citation_marker <- if (!is.na(sup_node)) {
        rvest::html_text(sup_node)
      } else {
        NA_character_
      }

      sup_nodes_in_cells <- rvest::html_elements(cells, "sup")
      if (length(sup_nodes_in_cells) > 0) {
        for (node in sup_nodes_in_cells) {
          current_text <- rvest::html_text(node)
          if (!stringr::str_starts(current_text, "\\^")) {
            xml2::xml_set_text(node, paste0("^", current_text, "^"))
          }
        }
      }

      # Now, extract the text, accounting for the table structure.
      # A "full" row in this table has 5 cells. A "partial" row has 3.
      if (length(cells) == 5) {
        # This is a full row, so it contains the spanned cells.
        # Update our stored values and extract text from all 5 cells.
        spanned_values$class <<- rvest::html_text(cells[1], trim = TRUE)
        spanned_values$source <<- rvest::html_text(cells[2], trim = TRUE)

        tibble(
          class = spanned_values$class,
          source = spanned_values$source,
          param = rvest::html_text(cells[3], trim = TRUE),
          spec = rvest::html_text(cells[4], trim = TRUE),
          req = rvest::html_text(cells[5], trim = TRUE),
          citation_marker = citation_marker
        )
      } else {
        # This is a partial row. Use the stored values for the first two columns.
        tibble(
          class = spanned_values$class,
          source = spanned_values$source,
          param = rvest::html_text(cells[1], trim = TRUE),
          spec = rvest::html_text(cells[2], trim = TRUE),
          req = rvest::html_text(cells[3], trim = TRUE),
          citation_marker = citation_marker
        ) %>%
          mutate(citation_marker = str_remove_all(citation_marker, "\\^"))
      }
    }
  )

# 3. Join the extracted table data with the citations.
if (!is.null(citations_df) && "citation_marker" %in% names(table_data)) {
  .x$dat <- left_join(table_data, citations_df, by = "citation_marker")
} else {
  .x$dat <- table_data
}

.x$head_url <- NULL
.x$Url <- NULL

.x <- compact(.x)

.x <- as_tibble(.x) %>%
  unnest(., cols = dat)
# } else {
#   .x <- NULL
# }


library(rvest)

# The URL from lf[9]
url <- "https://www.epa.gov/node/288422"

# Scrape the page and get just the table node
raw_tbl <- read_html(url) %>%
  html_element(xpath = '//*[@id="table1"]')

header_cells <- html_elements(raw_tbl, "thead th")

# This tells you the number of columns
length(header_cells)
#> [1] 4

# This shows you their names
html_text(header_cells, trim = TRUE)
#> [1] "Recycled Water Class/Category (Approved Uses)"
#> [2] "Source Water Type"                            
#> [3] "Water Quality Parameter"                      
#> [4] "Specification"

        .x$dat <- html_table(.x$head_url) %>%
          select(1:5) %>%
          set_names()

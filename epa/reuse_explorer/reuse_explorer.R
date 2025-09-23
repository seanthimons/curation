{
  library(rio)
  library(janitor)
  library(tidyverse)
  library(here)
  library(httr)
  library(rvest)
  library(polite)
  library(jsonlite)
  library(fuzzyjoin)
  library(stringdist)

  setwd(here("epa", "reuse_explorer"))
}


# download ----------------------------------------------------------------

lf <- jsonlite::read_json(
  "https://www.epa.gov/system/files/other-files/2021-10/documents.json"
) %>%
  map(
    .,
    ~ {
      .x <- c(.x, dat = NA, head_url = NA)
      .x$State <- str_squish(.x$State)
      return(.x)
    },
    .progress = TRUE
  )


# raw ---------------------------------------------------------------------

raw <- lf %>%
  imap(
    .,
    ~ {
      total <- length(lf)
      cli::cli_text(
        "[{.y}/{total}] ",
        .x$State,
        ": ",
        .x$`Source of Water`,
        "; ",
        .x$`Reuse Application`,
        "\n"
      )

      .x$head_url <-
        httr2::request(.x$Url) %>%
        httr2::req_method('HEAD') %>%
        httr2::req_perform() %>%
        pluck(., 'url')

      raw_tbl <- .x$head_url %>%
        bow(.) %>%
        scrape(content = "text/html; charset=UTF-8") %>%
        html_element(., xpath = '//*[@id="table1"]')

      if (length(.x$head_url) != 0L) {
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
                stringr::str_remove(fixed(marker)) %>%
                stringr::str_trim()

              tibble(citation_marker = marker, citation_text = citation_text)
            }
          ) %>%
          list_rbind()

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
                spanned_values$source <<- rvest::html_text(
                  cells[2],
                  trim = TRUE
                )

                tibble(
                  class = spanned_values$class,
                  source = spanned_values$source,
                  param = rvest::html_text(cells[3], trim = TRUE),
                  spec = rvest::html_text(cells[4], trim = TRUE),
                  req = rvest::html_text(cells[5], trim = TRUE),
                  citation_marker = citation_marker
                ) %>%
                  mutate(
                    citation_marker = str_remove_all(citation_marker, "\\^")
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
                  mutate(
                    citation_marker = str_remove_all(citation_marker, "\\^")
                  )
              }
            }
          )

        # 3. Join the extracted table data with the citations.
        if (
          !is.null(citations_df) &&
            nrow(citations_df) > 0 &&
            "citation_marker" %in% names(table_data)
        ) {
          .x$dat <- left_join(
            table_data,
            citations_df,
            by = "citation_marker"
          )
        } else {
          .x$dat <- table_data
        }

        .x$head_url <- NULL
        .x$Url <- NULL

        .x <- compact(.x)

        .x <- as_tibble(.x) %>%
          unnest(., cols = dat)
      } else {
        .x <- NULL
      }

      return(.x)
    },
    .progress = TRUE
  ) %>%
compact() %>%
list_rbind()

# TODO Explore fuzzy matches for params from TADA and SSWQS

compounds <- raw %>%
  count(param) %>%
  arrange(desc(n)) %>%
  filter(!is.na(param)) %>%
  mutate(
    idx_n = 1:n(),
    #orig_param = param,
    param = str_replace_all(param, pattern = '\\u00a0', replacement = " ") %>%
      str_squish(),
    #uni = stringi::stri_escape_unicode(param),
    #len = str_length(uni)
  ) %>%
  select(-n) %>%
  #arrange(param) %>%
  #mutate(idx_alp = 1:n()) %>% #%>% mutate(across(where(is.numeric), as.character))
  distinct(param, .keep_all = TRUE)

# hashing -----------------------------------------------------------------

nwqs <- rio::import(here("final", "nwqs.RDS")) %>%
  distinct(preferredName, dtxsid)

#TEMP Need to import hashing functions here

save.image('reuse.RData')

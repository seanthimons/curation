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

# The script is hardcoded to pull the first entry to shorten validation time.
raw <- lf[1] %>%
  map(
    .,
    ~ {
      cli::cli_text(
        .x$State,
        ": ",
        .x$`Source of Water`,
        "; ",
        .x$`Reuse Application`,
        "\n"
      )
      .x$head_url <-
        HEAD(.x$Url) %>%
        .$url %>%
        bow(.) %>%
        scrape(content = "text/html; charset=UTF-8") %>%
        html_element(., xpath = '//*[@id="table1"]')

      if (length(.x$head_url) != 0L) {
        # 1. Extract citation text from the page (usually in a list after the table).
        citation_p_nodes <- .x$head_url %>%
          xml2::xml_parent() %>%
          rvest::html_elements(xpath = "following-sibling::p[sup]")

        citations_df <- NULL
        if (length(citation_p_nodes) > 0) {
          # Process each footnote paragraph found.
          citations_df <- citation_p_nodes %>%
            map_dfr(~{
              children <- rvest::html_children(.x)
              markers <- rvest::html_text(children[rvest::html_name(children) == "sup"])
              texts <- rvest::html_text(
                children[rvest::html_name(children) == "text"],
                trim = TRUE
              )
              texts <- texts[texts != ""]
              
              tibble(
                citation_marker = markers[1:length(texts)],
                citation_text = texts
              )
            })
        }

        # 2. Process the main table row-by-row to preserve superscripts and extract citation markers.
        table_rows <- rvest::html_elements(.x$head_url, "tbody tr")

        table_data <- table_rows %>%
          map_dfr(
            ~ {
              # In this map, `.x` represents a single row node (<tr>).
              cells <- rvest::html_elements(.x, "td")

              if (length(cells) == 0) {
                return(NULL)
              }
              
              # --- FIX APPLIED HERE ---
              # To get a single citation marker for the row, search within the row node (`.x`) itself,
              # not within the list of cells. This ensures `sup_node` is a single object.
              sup_node <- rvest::html_element(.x, "sup")
              citation_marker <- if (!is.na(sup_node)) {
                rvest::html_text(sup_node)
              } else {
                NA_character_
              }

              # Now, proceed with modifying all <sup> tags within the cells for caret wrapping.
              # This logic is correct as it needs to act on all cells.
              sup_nodes_in_cells <- rvest::html_elements(cells, "sup")
              if (length(sup_nodes_in_cells) > 0) {
                xml2::xml_set_text(
                  sup_nodes_in_cells,
                  paste0("^", rvest::html_text(sup_nodes_in_cells), "^")
                )
              }

              # Create a tibble for the current row with the modified text.
              tibble(
                class = rvest::html_text(cells[1], trim = TRUE),
                source = rvest::html_text(cells[2], trim = TRUE),
                param = rvest::html_text(cells[3], trim = TRUE),
                spec = rvest::html_text(cells[4], trim = TRUE),
                req = rvest::html_text(cells[5], trim = TRUE),
                # Use the single citation_marker value determined for the whole row.
                citation_marker = citation_marker
              )
            }
          )

        # 3. Join the extracted table data with the citations.
        if (
          !is.null(citations_df) && "citation_marker" %in% names(table_data)
        ) {
          .x$dat <- left_join(table_data, citations_df, by = "citation_marker")
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
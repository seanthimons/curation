# packages ----------------------------------------------------------------

library(mirai)
library(rio)
library(janitor)
library(tidyverse)
library(here)
library(httr2)
library(rvest)
library(xml2)
library(jsonlite)
library(stringdist)
library(fuzzyjoin)

# NOTE: The working directory is set based on the original script's structure.
# Ensure your project has the 'epa/reuse_explorer' subdirectories.
setwd(here("epa", "reuse_explorer"))


# functions for parallel processing ---------------------------------------

#' A safe function to download a single file.
#' It takes one element of the lf list, performs the download, and returns
#' the updated list element. Includes robust error handling.
download_file_safely <- function(doc_meta) {
  tryCatch(
    {
      # --- Determine final URL by following redirects ---
      head_response <- httr2::request(doc_meta$Url) %>%
        httr2::req_method('HEAD') %>%
        httr2::req_perform()

      doc_meta$head_url <- head_response$url

      safe_filename_base <- paste(
        doc_meta$State,
        doc_meta$`Source of Water`,
        doc_meta$`Reuse Application`,
        sep = "_"
      ) %>%
        janitor::make_clean_names(case = "snake")

      doc_meta$filepath <- file.path(
        here("epa", "reuse_explorer", "raw"),
        paste0(safe_filename_base, ".html")
      )

      # --- Download and write file to disk ---
      page_response <- httr2::request(doc_meta$head_url) %>%
        httr2::req_perform()

      writeBin(httr2::resp_body_raw(page_response), doc_meta$filepath)

      return(doc_meta)
    },
    error = function(e) {
      # If any error occurs (network, file write, etc.), log it and return NULL
      message(
        "Failed to download or write file for: ",
        doc_meta$State,
        ". Error: ",
        e$message
      )
      return(NULL)
    }
  )
}


#' A safe function to parse a single HTML file.
#' It takes one downloaded document metadata object, parses the HTML,
#' and returns a cleaned data frame. Includes robust error handling.
parse_html_file <- function(doc_meta) {
  tryCatch(
    {
      # Pre-check: Ensure the file was actually downloaded before proceeding
      if (is.na(doc_meta$filepath) || !file.exists(doc_meta$filepath)) {
        message("File not found for ", doc_meta$State, ", skipping.")
        return(NULL)
      }

      # --- Read the static HTML file from disk ---
      local_html_doc <- read_html(doc_meta$filepath)
      raw_tbl_node <- local_html_doc %>%
        html_element(xpath = '//*[@id="table1"]')

      # If the main table is missing, there's nothing to parse.
      if (is.na(raw_tbl_node)) {
        message(
          "Table with id 'table1' not found in file for ",
          doc_meta$State,
          ", skipping."
        )
        return(NULL)
      }

      # --- Start of original parsing logic ---
      citation_p_nodes <- raw_tbl_node %>%
        xml2::xml_parent() %>%
        rvest::html_elements(xpath = "following-sibling::p[sup]")

      citations_df <- NULL
      if (length(citation_p_nodes) > 0) {
        citations_df <- citation_p_nodes %>%
          map_dfr(
            ~ {
              full_text <- rvest::html_text(.x, trim = TRUE)
              marker <- .x %>%
                rvest::html_element("sup") %>%
                rvest::html_text() %>%
                stringr::str_squish()
              citation_text <- full_text %>%
                stringr::str_remove(fixed(marker)) %>%
                stringr::str_trim()
              tibble(citation_marker = marker, citation_text = citation_text)
            }
          )
      }

      true_citation_markers <- if (!is.null(citations_df)) {
        unique(citations_df$citation_marker)
      } else {
        character(0)
      }
      sup_nodes <- xml2::xml_find_all(raw_tbl_node, ".//sup")

      walk(
        sup_nodes,
        ~ {
          node_text <- xml2::xml_text(.x)
          if (node_text %in% true_citation_markers) {
            xml2::xml_replace(
              .x,
              xml2::xml_cdata(paste0("__", node_text, "__"))
            )
          }
        }
      )

      dirty_df <- raw_tbl_node %>%
        as.character() %>%
        read_html() %>%
        rvest::html_table(header = TRUE, fill = TRUE) %>%
        pluck(1) %>%
        janitor::clean_names()

      dirty_df <- dirty_df %>%
        fill(everything(), .direction = "down") %>%
        mutate(row_id = row_number())

      citation_map <- dirty_df %>%
        pivot_longer(
          -row_id,
          names_to = "column",
          values_to = "value",
          values_transform = as.character
        ) %>%
        filter(str_detect(value, "\\__")) %>%
        mutate(
          citation_marker = str_extract_all(value, "(?<=\\__)(.*?)(?=\\__)")
        ) %>%
        unnest(citation_marker) %>%
        select(row_id, citation_marker) %>%
        distinct()

      row_citations <- NULL
      if (nrow(citation_map) > 0 && !is.null(citations_df)) {
        row_citations <- citation_map %>%
          left_join(citations_df, by = "citation_marker") %>%
          group_by(row_id) %>%
          summarise(
            citation_text = paste(unique(citation_text), collapse = "; ")
          ) %>%
          ungroup() %>%
          mutate(row_id = as.character(row_id))
      }

      clean_df <- dirty_df %>%
        mutate(across(
          everything(),
          ~ str_replace_all(., "\\__.*?\\__", "")
        )) %>%
        mutate(across(everything(), str_squish))

      final_df <- clean_df
      if (!is.null(row_citations)) {
        final_df <- left_join(final_df, row_citations, by = "row_id")
      } else {
        final_df$citation_text <- NA_character_
      }

      final_df <- final_df %>%
        mutate(
          State = doc_meta$State,
          `Source of Water` = doc_meta$`Source of Water`,
          `Reuse Application` = doc_meta$`Reuse Application`,
          .before = 1
        ) %>%
        select(-row_id)
      # --- End of original parsing logic ---

      return(final_df)
    },
    error = function(e) {
      # If any error occurs during parsing, log it and return NULL
      message(
        "Failed to parse file for: ",
        doc_meta$State,
        ". Error: ",
        e$message
      )
      return(NULL)
    }
  )
}


# setup mirai daemons -----------------------------------------------------

# Use n-1 cores to leave one free for system processes
n_workers <- parallel::detectCores() - 1
message("Starting ", n_workers, " mirai daemons...")
mirai::daemons(n = n_workers)


# download ----------------------------------------------------------------

dir.create(here("epa", "reuse_explorer", "raw"), showWarnings = FALSE)

# Read the initial JSON metadata
lf <- jsonlite::read_json(
  "https://www.epa.gov/system/files/other-files/2021-10/documents.json"
) %>%
  map(
    ~ {
      .x <- c(.x, dat = NA, head_url = NA)
      .x$State <- str_squish(.x$State)
      return(.x)
    }
  )

# --- Parallel Download ---
message("Dispatching ", length(lf), " download tasks to workers...")
download_tasks <- lf %>%
  map(
    ~ in_parallel(
      {
        download_file_safely(doc)
      },
      doc = .x
    )
  )

# CORRECTED: Collect results using purrr::map with the identity function ~.x
lf_downloaded <- purrr::map(download_tasks, ~.x)
message("All download tasks complete.")


# raw (Parse Files) -------------------------------------------------------

# --- Parallel Parsing ---
message("Dispatching parsing tasks to workers...")

# First, remove any NULLs from failed downloads, then dispatch parsing tasks.
parse_tasks <- lf_downloaded %>%
  purrr::compact() %>%
  map(
    in_parallel(
      ~ {
        library(rio)
        library(janitor)
        library(tidyverse)
        library(here)
        library(httr2)
        library(rvest)
        library(xml2)
        library(jsonlite)

        parse_html_file(doc)
      },
      doc = .x
    )
  )

raw <- purrr::map(parse_tasks, unname) %>%
  purrr::list_rbind()

# Stop daemons when finished to clean up background processes
mirai::daemons(n = 0)

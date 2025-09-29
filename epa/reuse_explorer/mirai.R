# packages ----------------------------------------------------------------
{
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
}

# functions for parallel processing ---------------------------------------

#' A safe function to download a single file.
#' It takes one element of the lf list, performs the download, and returns
#' the updated list element. Includes robust error handling.
download_file_safely <- function(doc_meta) {
  tryCatch(
    {
      # Builds a safe filename based on metadata
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

      # If the file already exists, skip downloading it again
      if (!file.exists(doc_meta$filepath)) {
        #  Determine final URL by following redirects
        head_response <- httr2::request(doc_meta$Url) %>%
          httr2::req_method('HEAD') %>%
          httr2::req_perform()

        doc_meta$head_url <- head_response$url

        # Download and write file to disk
        page_response <- httr2::request(doc_meta$head_url) %>%
          httr2::req_perform()

        writeBin(httr2::resp_body_raw(page_response), doc_meta$filepath)
      } # else {cli::cli_alert_info("File already exists for {doc_meta$filepath}, skipping download.")}
      # Return the updated metadata object
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
        message("File not found for ", doc_meta$filepath, ", skipping.")
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
          # Get the text from the <sup> node and clean it
          node_text <- xml2::xml_text(.x) %>% stringr::str_squish()

          # Split the text by commas to handle cases like "a,c"
          markers <- stringr::str_split(node_text, pattern = ",")[[1]] %>%
            stringr::str_trim()

          # Check if ALL of the identified markers are valid
          if (all(markers %in% true_citation_markers)) {
            # Construct a replacement string where each marker is individually delimited
            # e.g., c("a", "c") becomes "__a____c__"
            replacement_text <- paste0("__", markers, "__", collapse = "")

            # Replace the original <sup> node with the new text
            xml2::xml_replace(.x, xml2::xml_cdata(replacement_text))
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
        # ! Does this remove the internal line breaks?
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
mirai::daemons(n = parallel::detectCores() - 1)


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

# Parallel Download -----

lf_downloaded <- lf %>%
  map(
    in_parallel(
      \(doc_meta) {
        library(rio)
        library(janitor)
        library(tidyverse)
        library(here)
        library(httr2)
        library(rvest)
        library(xml2)
        library(jsonlite)
        download_file_safely(doc_meta)
      },
      download_file_safely = download_file_safely
    ),
    .progress = TRUE
  )


# Parallel Parse ------------------------------------------------------------
lf_names <- lf_downloaded %>%
  map(., ~ pluck(.x, "filepath") %>% basename() %>% tools::file_path_sans_ext())

raw <- lf_downloaded %>%
  purrr::compact() %>%
  map(
    in_parallel(
      \(doc_meta) {
        library(rio)
        library(janitor)
        library(tidyverse)
        library(here)
        library(httr2)
        library(rvest)
        library(xml2)
        library(jsonlite)

        parse_html_file(doc_meta)
      },
      parse_html_file = parse_html_file
    ),
    .progress = TRUE
  ) %>%
  purrr::set_names(lf_names) %>%
  compact() %>%
  list_rbind()

rm(lf_names, lf_downloaded, lf)

# Stop daemons when finished to clean up background processes
mirai::daemons(n = 0)

#Cleaning ------------------------------------------------------

cur <- raw %>%
  mutate(
    idx = 1:n(),
    area = State,
    source_water_super = `Source of Water`,
    source_water_type = purrr::reduce(
      pick(starts_with("source_water_type")),
      coalesce
    ),
    reuse_application = `Reuse Application`,
    application_class = purrr::reduce(
      pick(starts_with("recycled_water_")),
      coalesce
    ),
    specification = purrr::reduce(
      pick(starts_with("specification")),
      coalesce
    ), #%>% str_replace_all("\\R+", "\\__") %>% str_remove_all("\\t+"),
    sampling_requirements = purrr::reduce(
      pick(starts_with("sampling_")),
      coalesce
    ), #%>% str_replace_all("\\R+", "\\__") %>% str_remove_all("\\t+"),
    water_quality_parameter = purrr::reduce(
      pick(starts_with("water_quality_parameter")),
      coalesce
    ),
    citation_text = na_if(citation_text, ""),
    .keep = 'unused'
  ) %>%
  select(
    idx,
    area,
    source_water_super,
    source_water_type,
    reuse_application,
    application_class,
    specification,
    water_quality_parameter,
    sampling_requirements,
    citation_text
  ) %>%
  mutate(
    raw_specification = specification,
    specification = str_replace_all(
      specification,
      pattern = c(
        "\\u00a0" = " ", # non-breaking space
        "\\u2013" = "-", # en dash
        "\\u2014" = "-", # em dash
        "\\u2015" = "-", # horizontal bar
        "\\u00ad" = "-", # soft hyphen
        "\\u2018" = "'", # left single quotation mark
        "\\u2019" = "'", # right single quotation mark
        "\\u201c" = '"', # left double quotation mark
        "\\u201d" = '"', # right double quotation mark
        "\\u2026" = "...", # ellipsis
        "\\u00b0" = " degrees ", # degree symbol
        "\\u03bc" = "u", # Greek letter mu
        "\\u00b5" = "u", # micro sign
        "\\u00b3" = "", #Removes superscripted numbers; not sure why this wasn't caught earlier
        "\\u2264" = "<=", # less than or equal to
        "\\u2265" = ">=", # greater than or equal to
        "\\u00a7" = "section" # section sign (looks like a double S
      )
    ),
    unicode_count = stringi::stri_count(
      specification,
      regex = "[^\\x00-\\x7F]|\\>|\\<"
    )
  ) %>%
  rowwise() %>%
  mutate(
    specification = case_when(
      unicode_count >= 2 ~
        gsub(
          pattern = "(^(?:[^<>\\(\\)]+|\\([^)]*\\))*(?:<=|>=|<|>)(?:[^<>\\(\\)]+|\\([^)]*\\))*?)((?:<=|>=|<|>))",
          replacement = "\\1__\\2",
          specification,
          perl = TRUE
        ),
      .default = specification
    ),
    #specification_annotatation = str_extract(specification, "\\(.*\\)"),
  ) %>%
  separate_longer_delim(cols = specification, delim = "__") %>%
  #separate_longer_delim(cols = sampling_requirements, delim = "__") %>%
  filter(specification != "")


# NOTE: To inspect specifications with multiple entries, uncomment below:
cur %>%
  slice_sample(n = 20) %>%
  filter(str_detect(specification, "\\|")) %>%
  select(specification) %>%
  mutate(unicode = stringi::stri_escape_unicode(specification)) %>%
  print(n = Inf, width = Inf)

compounds <- cur %>%
  count(water_quality_parameter) %>%
  arrange(desc(n))

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

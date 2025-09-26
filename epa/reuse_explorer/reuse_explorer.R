# packages ----------------------------------------------------------------

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

dir.create(
  here("epa", "reuse_explorer", "raw"),
  showWarnings = FALSE
)


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

lf <- lf[65:66] %>%
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

      # --- Determine final URL ---
      .x$head_url <-
        httr2::request(.x$Url) %>%
        httr2::req_method('HEAD') %>%
        httr2::req_perform() %>%
        pluck(., 'url')

      safe_filename_base <- paste(
        .x$State,
        .x$`Source of Water`,
        .x$`Reuse Application`,
        sep = "_"
      ) %>%
        janitor::make_clean_names(case = "snake")

      .x$filepath <- file.path(
        here("epa", "reuse_explorer", "raw"),
        paste0(safe_filename_base, ".html")
      )

      # --- Download and write file ---
      page_response <- httr2::request(.x$head_url) %>%
        httr2::req_perform()

      writeBin(httr2::resp_body_raw(page_response), .x$filepath)

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

      # Check if the downloaded file exists before trying to read
      if (is.na(.x$filepath) || !file.exists(.x$filepath)) {
        cli::cli_alert_warning("File not found for {.x$State}, skipping.")
        return(NULL)
      }

      # --- Read the static HTML file from disk ---
      local_html_doc <- read_html(.x$filepath)

      # Find the main table node
      raw_tbl_node <- local_html_doc %>%
        html_element(xpath = '//*[@id="table1"]')

      # Return NULL if the table doesn't exist
      if (is.na(raw_tbl_node)) {
        cli::cli_alert_warning("Table with id 'table1' not found, skipping.")
        return(NULL)
      }

      # 1. READ CITATIONS: Extract definitions from <p> tags after the table.
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

      # 2. MODIFY HTML: Wrap true citation markers (<sup>) with delimiters.
      sup_nodes <- xml2::xml_find_all(raw_tbl_node, ".//sup")

      walk(
        sup_nodes,
        ~ {
          node_text <- xml2::xml_text(.x)
          #cli::cli_inform("Inspecting node: {.code {as.character(.x)}}")
          if (node_text %in% true_citation_markers) {
            #cli::cli_alert_info("Found valid citation marker: '{node_text}'. Replacing node in memory.")
            # Replace the <sup> node with a text node like "^a^"
            xml2::xml_replace(
              .x,
              xml2::xml_cdata(paste0("__", node_text, "__"))
            )
          } # else{cli::cli_alert_warning("Node text '{node_text}' is not a true citation. Skipping modification.")}
        }
      )

      # # 3. READ DATA: Parse the modified HTML table into a "dirty" data frame.
      # # html_table with fill = TRUE helps handle cells with rowspan.
      dirty_df <- raw_tbl_node %>%
        as.character() %>% # Convert the modified node to a string
        read_html() %>% # Re-parse the string into a new, clean HTML object
        rvest::html_table(header = TRUE, fill = TRUE) %>%
        pluck(1) %>%
        janitor::clean_names()

      # # Handle rowspans by filling NA values downwards
      dirty_df <- dirty_df %>%
        fill(everything(), .direction = "down") %>%
        mutate(row_id = row_number())

      # # --- Process and join citations ---

      # # Pivot longer to find and extract all citation markers from all cells.
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
        # Join the map with citation definitions and aggregate by row.
        row_citations <- citation_map %>%
          left_join(citations_df, by = "citation_marker") %>%
          group_by(row_id) %>%
          summarise(
            citation_text = paste(unique(citation_text), collapse = "; ")
          ) %>%
          ungroup() %>%
          mutate(row_id = as.character(row_id))
      }

      # # --- Clean the main data table ---

      # # Remove the citation markers (e.g., "^a^") from all cells
      clean_df <- dirty_df %>%
        mutate(across(
          everything(),
          ~ str_replace_all(., "\\__.*?\\__", "")
        )) %>%
        mutate(across(everything(), str_squish))

      # # --- Combine and Finalize ---

      final_df <- clean_df
      if (!is.null(row_citations)) {
        final_df <- left_join(final_df, row_citations, by = "row_id")
      } else {
        final_df$citation_text <- NA_character_
      }

      # # Add metadata and remove temporary row id
      final_df <- final_df %>%
        mutate(
          State = .x$State,
          `Source of Water` = .x$`Source of Water`,
          `Reuse Application` = .x$`Reuse Application`,
          .before = 1
        ) %>%
        select(-row_id)

      return(final_df)
    },
    .progress = TRUE
  ) %>%
  compact() %>%
  list_rbind()

#saveRDS(raw, here("epa", "reuse_explorer","reuse_raw.RDS"))

# clean -------------------------------------------------------------------

cur <- raw %>%
  mutate(
    area = State,
    source_water_super = `Source of Water`,
    source_water_type = source_water_type,
    reuse_application = `Reuse Application`,
    application_class = coalesce(
      recycled_water_class_category_approved_uses,
      recycled_water_class_category
    ),
    specification = coalesce(specification, specifications) %>%
      str_replace_all("\\R+", "\\|") %>%
      str_remove_all("\\t+"),
    sampling_requirements = sampling_monitoring_requirements_frequency_of_monitoring_site_location_of_sample_quantification_methods,
    water_quality_parameter = str_remove_all(
      water_quality_parameter,
      "\\^.*?\\^"
    ),
    citation_text = na_if(citation_text, ""),
    .keep = 'unused'
  ) %>%
  select(
    area,
    source_water_super,
    source_water_type,
    reuse_application,
    application_class,
    water_quality_parameter,
    specification,
    sampling_requirements,
    citation_text
  ) %>%
  separate_longer_delim(cols = specification, delim = "\\|")

cur %>%
  filter(str_detect(specification, "\\|")) %>%
  pull(specification) %>%
  stringi::stri_escape_unicode()

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

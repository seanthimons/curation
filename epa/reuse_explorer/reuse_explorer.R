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

# Hardcoded value for validation

# lf[10] %>% dput()

# list(list(
#   State = "Australia",
#   `Source of Water` = "Rainwater Collected Onsite",
#   `Reuse Application` = "Onsite Non-Potable Water Reuse",
#   Url = "https://www.epa.gov/node/288423",
#   dat = NA,
#   head_url = NA
# ))

raw <- lf[61:72] %>%
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

      if (!is.na(raw_tbl)) {
        # 1. Extract citation text definitions (This logic remains correct and robust)
        citation_p_nodes <- raw_tbl %>%
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
                  rvest::html_text()
                citation_text <- full_text %>%
                  stringr::str_remove(fixed(marker)) %>%
                  stringr::str_trim()
                tibble(citation_marker = marker, citation_text = citation_text)
              }
            )
        }

        # 2. Get header info and create clean column names
        header_cells <- rvest::html_elements(raw_tbl, "thead th")
        n_cols <- length(header_cells)
        
        # To create clean names, we create a temporary version of the header nodes
        # and remove the superscripts from that temporary version.
        temp_header_for_names <- rvest::html_elements(raw_tbl, "thead th")
        xml2::xml_remove(rvest::html_elements(temp_header_for_names, "sup"))
        col_names <- rvest::html_text(temp_header_for_names, trim = TRUE) %>%
          janitor::make_clean_names()

        # 3. Process table rows using a 'for' loop for rowspan safety
        table_rows <- rvest::html_elements(raw_tbl, "tbody tr")
        spanned_cell_memory <- list()
        all_rows_list <- list()

        for (current_row_node in table_rows) {
          current_cells <- rvest::html_elements(current_row_node, "td")
          if (length(current_cells) == 0) next

          # --- FIX APPLIED HERE ---
          # This is a safe, in-place modification within the loop.
          # It finds all 'sup' tags in the current row's cells and wraps their content.
          sup_nodes_in_cells <- rvest::html_elements(current_cells, "sup")
          if (length(sup_nodes_in_cells) > 0) {
            for (node in sup_nodes_in_cells) {
              current_text <- rvest::html_text(node)
              if (!stringr::str_starts(current_text, "\\^")) {
                xml2::xml_set_text(node, paste0("^", current_text, "^"))
              }
            }
          }
          # --- END OF FIX ---

          full_row_values <- vector("character", n_cols)
          cell_idx <- 1

          for (col_idx in 1:n_cols) {
            if (!is.null(spanned_cell_memory[[as.character(col_idx)]])) {
              full_row_values[col_idx] <- spanned_cell_memory[[as.character(col_idx)]]$value
              spanned_cell_memory[[as.character(col_idx)]]$rows_left <- spanned_cell_memory[[as.character(col_idx)]]$rows_left - 1
              if (spanned_cell_memory[[as.character(col_idx)]]$rows_left == 0) {
                spanned_cell_memory[[as.character(col_idx)]] <- NULL
              }
            } else {
              if (cell_idx <= length(current_cells)) {
                target_cell <- current_cells[[cell_idx]]
                # Extract the text, which now includes the '^' characters
                cell_text <- rvest::html_text(target_cell, trim = TRUE)
                
                full_row_values[col_idx] <- cell_text
                rowspan <- rvest::html_attr(target_cell, "rowspan")
                if (!is.na(rowspan) && as.numeric(rowspan) > 1) {
                  spanned_cell_memory[[as.character(col_idx)]] <- list(value = cell_text, rows_left = as.numeric(rowspan) - 1)
                }
                cell_idx <- cell_idx + 1
              } else {
                full_row_values[col_idx] <- NA_character_
              }
            }
          }
          row_as_tibble <- full_row_values %>% set_names(col_names) %>% as_tibble_row()
          all_rows_list <- c(all_rows_list, list(row_as_tibble))
        }
        table_data <- dplyr::bind_rows(all_rows_list)

        # 4. Find all markers for each row and join the citation text
        if (!is.null(citations_df) && nrow(citations_df) > 0) {
            header_sup_nodes <- rvest::html_elements(header_cells, "sup")
            header_markers_map <- map(header_sup_nodes, ~{
                marker_text <- rvest::html_text(.x)
                col_index <- which(map_lgl(header_cells, ~!is.na(rvest::html_element(.x, "sup")) && rvest::html_text(rvest::html_element(.x, "sup")) == marker_text))
                list(marker = marker_text, col = col_index)
            })

            citation_texts <- map_chr(1:nrow(table_data), ~{
                row_index <- .x
                current_html_row <- table_rows[[row_index]]
                
                # Get markers from cells; remove the '^' wrappers before lookup
                cell_markers <- rvest::html_elements(current_html_row, "td sup") %>% rvest::html_text(trim = TRUE) %>% str_remove_all(., "\\^")
                
                active_cols <- which(!is.na(table_data[row_index,]))
                header_markers_for_row <- keep(header_markers_map, ~ .x$col %in% active_cols) %>% map_chr("marker")
                all_markers <- unique(c(cell_markers, header_markers_for_row))
                
                if (length(all_markers) == 0) return(NA_character_)
                
                marker_tbl <- tibble(citation_marker = all_markers)
                joined <- left_join(marker_tbl, citations_df, by = "citation_marker")
                
                valid_texts <- na.omit(joined$citation_text)
                if (length(valid_texts) == 0) return("")
                
                paste(valid_texts, collapse = "; ")
            })
            if(length(citation_texts) == nrow(table_data)){
              table_data$citation_text <- citation_texts
            }
        }

        # 5. Final cleanup and assignment
        .x$dat <- table_data
        .x$head_url <- NULL
        .x$Url <- NULL
        .x <- compact(.x)
        .x <- as_tibble(.x) %>% unnest(., cols = dat)
      } else {
        .x <- NULL
      }
      return(.x)
    },
    .progress = TRUE
  )  %>% compact() %>%
list_rbind()

saveRDS(raw, here("epa", "reuse_explorer","reuse_raw.RDS"))

# clean -------------------------------------------------------------------
cur <- raw %>% 
	mutate(
		area = State, 
		source_water_super = `Source of Water`,
		source_water_type = source_water_type, 
		reuse_application = `Reuse Application`,
		application_class = coalesce(recycled_water_class_category_approved_uses, recycled_water_class_category),
		specification = coalesce(specification, specifications) %>% str_replace_all("\\R+", "\\|") %>% str_remove_all("\\t+"),
		sampling_requirements = sampling_monitoring_requirements_frequency_of_monitoring_site_location_of_sample_quantification_methods,
		water_quality_parameter = str_remove_all(water_quality_parameter, "\\^.*?\\^"),
		citation_text = na_if(citation_text, ""),
		.keep = 'unused'
	) %>% 
	select(area, source_water_super, source_water_type, reuse_application, application_class,water_quality_parameter, specification, sampling_requirements, citation_text) %>% 
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

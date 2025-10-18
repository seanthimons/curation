clowder_list <- request(
  'https://clowder.edap-cluster.com/api/datasets/61147fefe4b0856fdc65639b/listAllFiles'
) %>%
  req_perform() %>%
  resp_body_json()

folder_list <- clowder_list %>%
  map(~ .x[c("id", "filename", "folders")]) %>%
  keep(~ !is.null(.$folders) && length(.$folders) > 0)

tv_list <- clowder_list %>%
  map(~ .x[c("id", "filename")]) %>%
  keep(~ str_detect(.x$filename, "toxval_v9")) %>%
  discard(~ str_detect(.x$filename, '.sql|README|gz|with'))
#discard(~ str_detect(.x$filename, '.sql|README|qc_status|gz|with')) #%>%
# map(., ~pluck(., 'filename')) %>%
# unlist()

tv_ver <- tv_list %>%
  map(., ~ pluck(., 'filename')) %>%
  unlist() %>%
  str_replace_all(., 'toxval_all_res_', "") %>%
  str_subset(., pattern = 'toxval_v\\d{2}_\\d?') %>%
  str_extract(., pattern = 'v\\d{2}_\\d?') %>%
  unique() %>%
  rev()

tv_source_ver <- tv_ver %>%
  str_replace(., "^(v)(\\d)(\\d)", "\\1\\2_\\3") %>%
  set_names(tv_ver)

{
  # The base URL for the request
  base_url <- "https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b/updatedFilesAndFolders"

  # The data to be sent in the request body
  body_data <- list(
    currentSpace = "6112f2bee4b01a90a3fa7689", #COMPTOX space
    folderId = "62e184ebe4b055edffbfc22b" # ToxValDB folder
  )

  # Construct and perform the request
  page <- request(base_url) %>%
    req_method("POST") %>%
    req_url_query(
      limit = 20,
      pageIndex = 0,
      space = "6112f2bee4b01a90a3fa7689",
      filter = ""
    ) %>%
    req_headers(
      `accept` = "*/*",
      # `accept-language` = "en-US,en;q=0.9",
      # `origin` = "https://clowder.edap-cluster.com",
      # `priority` = "u=1, i",
      `referer` = "https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689",
      # `sec-ch-ua-mobile` = "?0",
      # `sec-ch-ua-platform` = "Windows",
      # `sec-fetch-dest` = "empty",
      # `sec-fetch-mode` = "cors",
      # `sec-fetch-site` = "same-origin",
      # `x-requested-with` = "XMLHttpRequest"
    ) %>%
    req_user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"
    ) %>%
    req_body_json(data = body_data) %>%
    req_perform() %>%
    resp_body_html()

  h3_nodes <- page %>%
    html_nodes("#folderListDiv h3")

  # Extract the 'id' attribute from each h3 node
  ids <- h3_nodes %>%
    html_attr("id") %>%
    str_remove_all(., '-name')

  # From each h3 node, find the 'a' tag and extract its text
  # The trim=TRUE argument removes leading/trailing whitespace and newlines
  names <- h3_nodes %>%
    html_node("a") %>%
    html_text(trim = TRUE)

  # Combine the extracted IDs and names into a data frame for a clean output
  tv_folders <- list(
    ids,
    names,
    tv_ver
  )

  tv_folders
}

# The `imap` function passes the list value as the first argument (`item`, the ID)
# and the list name as the second argument (`idx`, the folder name).
# Using `cli_text` provides a clean, single-line output for each item.
# pwalk(tv_folders, function(item, name, ver) {
#   print(here::here('epa', 'toxval', 'toxval_raw', ver, paste0(ver, ".zip")))
#   #cli::cli_text("{.strong {name}}: {item}")
# })

tv_folders %>%
  pmap(
    .,
    function(item, name, ver) {
      cli::cli_alert('Downloading: {name}')

      # The base URL for the request
      base_url <- "https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b/updatedFilesAndFolders"

      # The data to be sent in the request body
      body_data <- list(
        currentSpace = "6112f2bee4b01a90a3fa7689",
        folderId = item
      )

      # Construct and perform the request
      page <- request(base_url) %>%
        req_method("POST") %>%
        req_url_query(
          limit = 20,
          pageIndex = 0,
          space = "6112f2bee4b01a90a3fa7689",
          filter = ""
        ) %>%
        req_headers(
          `accept` = "*/*",
          # `accept-language` = "en-US,en;q=0.9",
          # `origin` = "https://clowder.edap-cluster.com",
          # `priority` = "u=1, i",
          `referer` = "https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689",
          # `sec-ch-ua-mobile` = "?0",
          # `sec-ch-ua-platform` = "Windows",
          # `sec-fetch-dest` = "empty",
          # `sec-fetch-mode` = "cors",
          # `sec-fetch-site` = "same-origin",
          # `x-requested-with` = "XMLHttpRequest"
        ) %>%
        req_user_agent(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"
        ) %>%
        req_body_json(data = body_data) %>%
        req_perform() %>%
        resp_body_html()

      h3_nodes <- page %>%
        html_nodes("#folderListDiv h3")

      # Extract the 'id' attribute from each h3 node
      ids <- h3_nodes %>%
        html_attr("id") %>%
        str_remove_all(., '-name')

      # From each h3 node, find the 'a' tag and extract its text
      # The trim=TRUE argument removes leading/trailing whitespace and newlines
      names <- h3_nodes %>%
        html_node("a") %>%
        html_text(trim = TRUE)

      # Combine the extracted IDs and names into a data frame for a clean output
      results <- tibble(
        id = ids,
        name = names
      ) %>%
        filter(name == 'Data Excel Files')

      fs::dir_create(here::here('epa', 'toxval', 'toxval_raw', ver))

      dest_path <- here::here(
        'epa',
        'toxval',
        'toxval_raw',
        ver,
        paste0(ver, ".zip")
      )

      if (fs::file_exists(dest_path)) {
        cli::cli_alert_info(
          "File for {name} already exists, skipping download."
        )
      } else {
        # Retry logic for download
        max_retries <- 3
        download_success <- FALSE
        for (attempt in 1:max_retries) {
          tryCatch(
            {
              download.file(
                paste0(
                  'https://clowder.edap-cluster.com/api/datasets/61147fefe4b0856fdc65639b/downloadFolder?folderId=',
                  results$id
                ),
                destfile = dest_path,
                mode = 'wb'
              )
              download_success <- TRUE
              cli::cli_alert_success("Successfully downloaded {name}")
              break # Exit loop on success
            },
            error = function(e) {
              if (grepl("timeout", e$message, ignore.case = TRUE)) {
                cli::cli_warn(
                  "Timeout on attempt {attempt} for {name}. Retrying in 5 seconds..."
                )
                Sys.sleep(5) # Wait before retrying
              } else {
                cli::cli_abort(
                  "Download failed for {name} with a non-timeout error: {e$message}"
                )
              }
            }
          )
        }
        if (!download_success) {
          cli::cli_abort(
            "Failed to download {name} after {max_retries} attempts."
          )
        }
      }
    },
    .progress = T
  )


# Unzipping --------------------------------------------------------------

walk(
  tv_ver,
  ~ {
    ver <- .x

    zip_path <- here::here(
      'epa',
      'toxval',
      'toxval_raw',
      ver,
      paste0(ver, ".zip")
    )

    unzip_dir <- here::here('epa', 'toxval', 'toxval_raw', ver)

    if (fs::file_exists(zip_path)) {
      # Check if the zip file is valid by trying to list its contents
      zip_contents <- try(unzip(zip_path, list = TRUE), silent = TRUE)

      if (inherits(zip_contents, "try-error") || nrow(zip_contents) == 0) {
        cli::cli_alert_danger(
          "Zip file for version {ver} appears to be corrupt or empty. Deleting and skipping."
        )
        fs::file_delete(zip_path)
      } else {
        cli::cli_alert_info("Unzipping: {ver}.zip")
        unzip(zip_path, exdir = unzip_dir, overwrite = TRUE)
        cli::cli_alert_success("Unzipped: {ver}.zip")
      }
    } else {
      cli::cli_alert_warning("Zip file for version {ver} not found, skipping.")
    }
  },
  .progress = "Unzipping archives"
)


# Building ---------------------------------------------------------------

tv_ver %>%
  iwalk(
    .,
    ~ {
      ver <- .x
      cli::cli_alert("Building: {ver}")

      version_path <- here('epa', 'toxval', 'toxval_raw', ver)

      excel_files <- fs::dir_ls(
        version_path,
        recurse = TRUE,
        regexp = "\\.xlsx?$"
      )

      if (length(excel_files) == 0) {
        cli::cli_alert_warning("No Excel files found for version {ver}, skipping.")
        return() # Skip to the next iteration
      }

      raw <- excel_files %>%
        map(
          ~ {
            readxl::read_excel(.x, col_types = "text", na = c("-", "")) %>%
              janitor::clean_names()
          },
          .progress = 'Reading in files'
        ) %>%
        list_rbind()

      output_file <- here('final', paste0('toxval_', ver, '.parquet'))
      fs::dir_create(here('final'))

      cli::cli_alert_success('Dumping: {basename(output_file)}')
      nanoparquet::write_parquet(
        raw,
        file = output_file
      )
    },
    .progress = TRUE
  )

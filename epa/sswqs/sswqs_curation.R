# packages ---------------------------------------------------------------

source(here("epa", "sswqs", "load_packages.R"), echo = FALSE)

# !!! Deploy flag ---------------------------------------------------------------------------

deploy <- TRUE

# Checkpoint -------------------------------------------------------------

# Determine if a rebuild is necessary.
# A rebuild is needed if any file is missing, or if any existing file is older than 90 days.
{
  files_to_check <- c(
    "sswqs.RDS"
  )

  files_exist_check <- file.exists(files_to_check)

  rebuild_is_needed <- if (!all(files_exist_check)) {
    cli::cli_alert_info(
      "One or more data files are missing. Rebuilding dataset."
    )
    tibble(files_to_check, files_exist_check) %>% print()
    TRUE
  } else {
    # Using mtime (modification time) is more robust for checking data freshness.
    file_ages_days <- difftime(
      Sys.time(),
      file.info(files_to_check)$mtime,
      units = "days"
    )
    if (any(file_ages_days > 180)) {
      cli::cli_alert_warning(
        "One or more data files are older than 90 days. Rebuilding dataset."
      )
      tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
      TRUE
    } else {
      cli::cli_alert_success(
        "All data files are present and up-to-date. Skipping rebuild."
      )
      tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
      FALSE
    }
  }
	rm(files_to_check, files_exist_check, file_ages_days)
}

# Rebuild ----------------------------------------------------------------

# If a rebuild is needed, run the data scraping and processing sections.
if (rebuild_is_needed) {
  cli::cli_alert_info('Rebuilding dataset...')

  library(V8)

  #Download JS----
  {
    cli::cli_alert_info('Downloading spec...')

    cx <- v8()

    # TODO Find a way to download this without hard linking it; this is a temporary solution

    # ! NOTE: Hard linked file
    cx$source("https://cfpub.epa.gov/wqsits/wqcsearch/data/criteria_json_5a.js")

    # ! NOTE: JS parsing here, do not modify
    vars <- cx$eval(
      "
	Object.keys(this).filter(function(key) {
		return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
	})
"
    ) %>%
      str_split(., ",") %>%
      pluck(., 1)

    parent_dat <- vars %>%
      map(
        .,
        ~ {
          cx$get(.x)
        }
      ) %>%
      set_names(., vars)

    state_vars <- parent_dat$entities %>%
      map(
        .,
        ~ as_tibble(.) %>%
          t(.) %>%
          as_tibble()
      ) %>%
      list_rbind() %>%
      set_names(., c("area", "region", "abv", "cat", "file", "coverage")) %>%
      mutate(
        across(everything(), ~ na_if(., "")),
        json = paste0(
          "https://cfpub.epa.gov/wqsits/wqcsearch/data/stateJson_",
          abv,
          ".js"
        ),
        idx = 1:n()
      )

    state_extra <- state_vars %>%
      select(area, region, cat, file, coverage, idx, abv)

    state_vars <- state_vars %>%
      select(abv, json)

    rm(vars, state_extra, cx)
  }

  # State data JS download -------------------------------------------------------------

  {
    cli::cli_alert_info('Requesting state data...')

    # TODO: Need to wrap in error handling for dropped connections...
    state_dat <- state_vars %>%
      pmap(
        .,
        function(abv, json) {
          # ! NOTE: Can use this as a progress meter
          #cli::cli_inform(abv, "\n")
          ctx <- v8()
          ctx$source(json)

          # ! NOTE: JS parsing here
          st_vars <- ctx$eval(
            "
	Object.keys(this).filter(function(key) {
		return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
	})
"
          ) %>%
            str_split(., ",") %>%
            pluck(., 1)

          dat <- st_vars %>%
            map(
              .,
              ~ {
                ctx$get(.x)
              }
            ) %>%
            set_names(., st_vars) %>%
            modify_at(., "criteriaData_sub", ~ pluck(., 1)) %>%
            compact(.)

          if (length(dat) == 0) {
            dat <- NULL
          } else {
            dat$desc_use_class_sub %<>%
              flatten(.) %>%
              enframe(., name = "key", value = "local") %>%
              unnest(., "local")

            new_names <- c(
              "analyte" = "analyte",
              "result" = "V1",
              "unit_code" = "V2",
              "protection" = "V3",
              "use_class" = "V4",
              "source" = "V5",
              "page_source" = "V6"
            )

            dat$criteriaData_sub %<>%
              map_if(
                .,
                is.list,
                ~ {
                  map(
                    .,
                    ~ {
                      t(.x) %>%
                        as_tibble(.)
                    }
                  ) %>%
                    list_rbind(.)
                }
              ) %>%
              map_if(
                .,
                is.character,
                ~ {
                  as_tibble(.x) %>%
                    mutate(across(everything(), as.character))
                }
              ) %>%
              list_rbind(., names_to = "analyte") %>%
              rename(!!!new_names)
          }
          rm(ctx)
          return(dat)
        },
        .progress = TRUE
      ) %>%
      set_names(., state_vars$abv) %>%
      # ! NOTE: Removes some empty records that don't have full status yet; some tribes not yet authorized
      compact(.)
  }

  # Export intermediate files ----------------------------------------------
  {
    saveRDS(state_dat, file = "sswqs_state_dat.RDS")
    saveRDS(parent_dat, file = "sswqs_parent_dat.RDS")

    # ! TEMP Checkpoint: read in if something is goofed.
    #state_dat <- readRDS(file = "sswqs_state_dat.RDS")
    #parent_dat <- readRDS(file = "sswqs_parent_dat.RDS")
  }

  # Extracting -------------------------------------------------------------

  {
    # sources ----------------------------------------------------------------

    sources <- state_dat %>%
      map(
        .,
        ~ {
          pluck(.x, "sourcedoc_sub") %>%
            flatten(.) %>%
            enframe(., name = "key", value = "link")
        }
      ) %>%
      list_rbind(names_to = "area") %>%
      unnest("link")

    # use_class --------------------------------------------------------------

    use_class <- state_dat %>%
      map(., ~ pluck(., "desc_use_class_sub")) %>%
      list_rbind(names_to = "area")

    # numerical data ---------------------------------------------------------

    # ! NOTE: Schema is : area, analyte, result, unit, protection code (not totally needed?), protection use case, source, page-source
    crit_dat <- state_dat %>%
      map(., ~ pluck(., "criteriaData_sub")) %>%
      list_rbind(names_to = "area")

    # ! NOTE Manual extraction of relevant data, since it is not easy to parse
    protection_dict <- crit_dat %>%
      select(protection) %>%
      distinct(protection) %>%
      mutate(
        # ! NOTE mapped manually here
        endpoint = case_when(
          protection == '' ~ NA,
          protection == 'H' ~ 'Human Health',
          protection == 'Hw' ~ 'Human Health - Water + Organism',
          protection == 'Ho' ~ 'Human Health - Organism Only',
          protection == 'Ac' ~ 'Aquatic Life - Chronic',
          protection == 'Aa' ~ 'Aquatic Life - Acute',
          protection == 'AFc' ~ 'Aquatic Life - Freshwater - Chronic',
          protection == 'AFa' ~ 'Aquatic Life - Freshwater - Acute',
          protection == 'A' ~ 'Aquatic Life',
          protection == 'HFw' ~ 'Human Health - Freshwater - Water + Organism)',
          protection == 'HFo' ~ 'Human Health - Freshwater - Organism Only',
          protection == 'ASa' ~ 'Aquatic Life - Saltwater - Acute',
          protection == 'ASc' ~ 'Aquatic Life - Saltwater - Chronic',
          protection == 'HF' ~ 'Human Health - Freshwater',
          protection == 'c' ~ 'Chronic',
          protection == 'F' ~ 'Freshwater',
          protection == 'Hc' ~ 'Human Health - Chronic',
          protection == 'HSo' ~ 'Human Health - Saltwater - Organism Only',
          protection == 'S' ~ 'Saltwater',
          protection == 'AF' ~ 'Aquatic Life - Freshwater',
          protection == 'O' ~ 'Organoleptic',
          protection == 'HS' ~ 'Human Health - Saltwate',
          protection == 'a' ~ 'Acute',
          protection == 'AcCCC' ~ 'Aquatic Life - Chronic',
          protection == 'AS' ~ 'Aquatic Life - Saltwater',
          protection == 'AaCMC' ~ 'Aquatic Life - Acute',
          protection == 'AFcCCC' ~ 'Aquatic Life - Freshwater - Chronic',
          protection == 'AScCCC' ~ 'Aquatic Life - Saltwater - Chronic',
          protection == 'AFaCMC' ~ 'Aquatic Life - Freshwater - Acute',
          protection == 'ASaCMC' ~ 'Aquatic Life - Saltwater - Acut',
          protection == 'Fc' ~ 'Freshwater - Chronic',
          protection == 'Fa' ~ 'Freshwater - Acute',
          protection == 'ABa' ~ 'Aquatic Life - Brackish - Acute',
          protection == 'ABc' ~ 'Aquatic Life - Brackish - Chronic',
          protection == 'Sc' ~ 'Aquatic Life - Saltwater - Chronic',
          protection == 'Sa' ~ 'Aquatic Life - Saltwater - Acute',
          protection == 'Ha' ~ 'Human Health - Acute',
          protection == 'Hco' ~ 'Human Health - Chronic - Organism Only',
          protection == 'Hcw' ~ 'Human Health - Chronic - Water + Organism',
          protection == 'Aco' ~ 'Aquatic Life - Chronic - Organism Only',
          protection == 'B' ~ 'Brackish',
          protection == 'Ow' ~ 'Organoleptic - Water + Organism',
          protection == 'AFao' ~
            'Aquatic Life - Freshwater - Acute - Organism Only',
          protection == 'AFo' ~ 'Aquatic Life - Freshwater - Organism Only',
          protection == 'AaCCC' ~ 'Aquatic Life - Acute',
          protection == 'AcCMC' ~ 'Aquatic Life - Chronic',
          protection == 'Haw' ~ 'Human Health - Acute - Water + Organism',
          protection == 'o' ~ 'Organism Only',
          protection == 'ACCC' ~ 'Aquatic Life',
          protection == 'Hao' ~ 'Human Health - Acute - Organism Only',
          protection == 'AB' ~ 'Aquatic Life - Brackish',
          protection == 'AFaCCC' ~ 'Aquatic Life - Freshwater - Acute',
          protection == 'AFaw' ~
            'Aquatic Life - Freshwater - Acute - Water + Organism',
          protection == 'AFco' ~
            'Aquatic Life - Freshwater - Chronic - Organism Only',
          protection == 'AScCMC' ~ 'Aquatic Life - Saltwater - Chronic',
          protection == 'CMC' ~ 'Aquatic Life - Chronic',
          protection == 'HFa' ~ 'Human Health - Freshwater - Acute',
          protection == 'HSa' ~ 'Human Health - Saltwater - Acute',
          protection == 'w' ~ 'Water + Organism',
        ),
        # ! Water
        location = case_when(
          str_detect(endpoint, 'Fresh') ~ 'freshwater',
          str_detect(endpoint, 'Salt') ~ 'saltwater',
          str_detect(endpoint, 'Brackish') ~ 'brackish'
        ),
        # ! Level of protection
        application = case_when(
          str_detect(endpoint, 'Human') ~ 'human health',
          str_detect(endpoint, 'Aquatic') ~ 'aquatic life',
          str_detect(endpoint, 'Organoleptic') ~ 'organoleptic'
        ),
        # ! Exposure group
        exposure = case_when(
          str_detect(endpoint, 'Water \\+ Organism') ~ 'water and organism',
          str_detect(endpoint, 'Organism Only') ~ 'organism only',
        ),
        # ! Exposure duration
        subtype = case_when(
          str_detect(endpoint, 'chronic|Chronic') ~ 'chronic',
          str_detect(endpoint, 'acute|Acute') ~ 'acute',
        )
      )

    rm(state_dat)

    # pollutants -------------------------------------------------------------

    pollutants <-
      parent_dat$pollutants %>%
      enframe(., name = 'idx', value = 'v') %>%
      unnest_wider(., 'v', names_sep = '') %>%
      rename(
        'analyte' = 'v1',
        'cas' = 'v2',
        'remap' = 'v3',
        'dtxsid' = 'v4'
      ) %>%
      mutate(across(everything(), ~ na_if(., "")))

    pollutantRemap <- parent_dat$pollutantRemap %>%
      enframe(., name = "idx", value = "old_idx") %>%
      unnest_longer(., col = "old_idx") %>%
      mutate(old_idx = as.character(old_idx))

    # units ------------------------------------------------------------------

    units <- parent_dat$units %>%
      enframe(., name = 'idx', value = 'v') %>%
      unnest_wider(., 'v', names_sep = '') %>%
      select(-v2) %>%
      rename(
        unit = v1
      )

    # entities ----------------------------------------------------------------

    entities <- parent_dat$entities %>%
      map(., ~ enframe(.x) %>% pivot_wider(., names_from = name)) %>%
      list_rbind() %>%
      rename(
        name = `1`,
        region = `2`,
        short_code = `3`,
        level = `4`,
        cit = `5`,
        coverage = `6`
      ) %>%
      mutate(coverage = na_if(coverage, ""))

    # ! NOTE Manual categories
    use_class_super <- read.table(
      'use_class_super.txt',
      sep = ",",
      header = TRUE,
      colClasses = "character"
    ) %>%
      select(-local)
  }
  # Building ---------------------------------------------------------------
  {
    sswqs <-
      crit_dat %>%
      # Area ----
      left_join(
        entities,
        .,
        join_by(
          short_code == area
        )
      ) %>% #glimpse()
      # Remap ----
      left_join(
        .,
        pollutantRemap,
        join_by(
          analyte == old_idx
        )
      ) %>%
      mutate(
        idx = case_when(
          is.na(idx) ~ analyte,
          .default = idx
        )
      ) %>%
      select(-analyte) %>%
      #glimpse()
      # Pollutants ----
      left_join(
        .,
        pollutants,
        join_by(
          idx == idx
        )
      ) %>%
      select(-idx, -remap) %>%
      #glimpse()
      # Use ----
      left_join(
        .,
        use_class,
        join_by(
          short_code == area,
          use_class == key
        )
      ) %>%
      #Protection ----
      left_join(
        .,
        protection_dict,
        join_by(
          protection == protection
        )
      ) %>%
      select(
        -protection,
      ) %>%
      # Units ----
      left_join(
        .,
        units,
        join_by(
          unit_code == idx
        )
      ) %>%
      select(-unit_code) %>%
      # Source and citation ----
      left_join(
        .,
        sources,
        join_by(
          source == key,
          short_code == area,
        )
      ) %>%
      # General usage ----
      left_join(
        .,
        use_class_super,
        join_by(
          use_class == key
        )
      ) %>%
      mutate(
        general_usage = case_when(
          local == 'limited contact recreation waters' ~ 'Recreation',
          .default = general_usage
        )
      ) #%>% filter(!is.na(result)) #%>% glimpse()

    rm(
      parent_dat,
      units,
      use_class,
      use_class_super,
      sources,
      protection_dict,
      pollutants,
      pollutantRemap,
      entities,
      crit_dat
    )

    saveRDS(sswqs, file = "sswqs.RDS")
    #sswqs <- readRDS(file = "sswqs.RDS")
  }

  # Cleaning and Unit Harmonization ----------------------------------------

  curated_sswqs <- sswqs %>%
    # Filter out rows with narrative criteria or non-parsable text
    filter(
      !str_detect(
        result,
        pattern = regex(
          "\\bsee\\b|\\bwithin\\b|\\busing\\b|\\bmore\\b|\\bincrease\\b|\\bnot\\b|/",
          ignore_case = TRUE
        )
      ),
      !is.na(result),
      result != ""
    ) %>%
    # Initial cleaning of the result string
    mutate(
      result = str_remove_all(string = result, pattern = ","),
      result = str_trim(result),
			# ! One-off adjustments
      result = case_when(
        result == '6.90E+0.1' ~ '6.90E+01',
        .default = result
      )
    ) %>%
    rename(orig_result = result) %>%
    # Main result parsing pipeline
    mutate(
      .id = 1:n(), # Unique identifier for each original row
      result = str_to_lower(orig_result),
      result = str_replace_all(result, " million", "e6"),
      result = str_remove_all(result, "[\\*,]"),
      result = str_remove_all(result, "[[:space:]]"), # "5.0 e-9" -> "5.0e-9",
      result = str_replace_all(result, "x10\\^?", "e"), # "5x10^-9" -> "5e-9"
      result = str_replace(result, "(?<=[0-9])(?<!e)([+-])(?=0\\d(?!\\d))", "e\\1"), # Fix for Fortran-style exponents (e.g., '4.56+02')
      parsed_value = as.numeric(result),
      num_bool = !is.na(parsed_value)
    ) %>%
    # Handle ranges (e.g., "5.6-7.8") by splitting into separate rows
    rowwise() %>%
    mutate(
      result = if (!num_bool) str_split(result, pattern = "-") else list(result)
    ) %>%
    unnest(result) %>%
    ungroup() %>%
    # Re-parse values after splitting
    mutate(
      parsed_value = as.numeric(result),
      num_bool = !is.na(parsed_value)
    ) %>%
    filter(num_bool) %>%

		# Unit harmonization --------------------------------------------------------------------

    mutate(
      # Create a standardized, clean unit name for matching
      cleaned_unit = case_when(
        is.na(unit) ~ "[no units]",
        TRUE ~
          unit %>%
          str_replace_all("[\\u00B5\\u03BC]", "u") %>% # Harmonize micro symbol
          stringi::stri_trans_general("latin-ascii") %>%
          str_to_lower() %>%
          str_replace_all("\\s+(per|/)\\s+", "/") %>%
          str_trim()
      ),

      # Define the target harmonized unit based on the cleaned unit
      harmonized_unit = case_when(
        cleaned_unit %in% c("°c", "°f") ~ "°c",
        cleaned_unit %in%
          c(
            "ug/l",
            "parts/billion (ppb)",
            "mg/l",
            "ppm",
            "ng/l",
            "pg/l",
            "ppq",
            "fg/l"
          ) ~
          "ug/l",
        cleaned_unit %in% c("mg/kg fish tissue", "mg/kg wet weight") ~
          "mg/kg (wet weight)",
        cleaned_unit %in% c("ug/g", "ug/kg") ~ "mg/kg",
        cleaned_unit %in%
          c(
            "count/100ml",
            "cfu/100 ml or mpn/100 ml",
            "mpn/100 ml",
            "organisms/100 ml"
          ) ~
          "count/100ml",
        cleaned_unit %in% c("ph units", "standard units") ~ "ph units",
        cleaned_unit %in% c("us/cm", "umhos/cm", "ds/m") ~ "us/cm",
        cleaned_unit %in% c("pci/l", "picocuries/l") ~ "pci/l",
        cleaned_unit %in% c("meters", "feet") ~ "meters",
        cleaned_unit %in% c("kg/yr", "lbs/year", "pounds/year") ~ "kg/yr",
        cleaned_unit %in%
          c("fibers/l", "million fibers/l", "mf/l", "microfibers/l") ~
          "fibers/l",
        cleaned_unit %in% c("ntu", "jtu") ~ "ntu",
        cleaned_unit %in%
          c("color units", "platinum cobalt units", "change in pcu") ~
          "pcu",
        TRUE ~ cleaned_unit # Default to the cleaned unit if no rule matches
      ),

      # Define conversion factor string (to accommodate formulas and numbers)
      conversion_factor_str = case_when(
        cleaned_unit == "°f" ~ "°C = (°F - 32) * 5/9",
        cleaned_unit %in% c("mg/l", "ppm") ~ "1000",
        cleaned_unit == "ng/l" ~ "0.001",
        cleaned_unit == "pg/l" ~ "1e-6",
        cleaned_unit %in% c("ppq", "fg/l") ~ "1e-9",
        cleaned_unit == "ug/g" ~ "1",
        cleaned_unit == "ug/kg" ~ "0.001",
        cleaned_unit == "ds/m" ~ "1000",
        cleaned_unit == "feet" ~ "0.3048",
        cleaned_unit %in% c("lbs/year", "pounds/year") ~ "0.453592",
        cleaned_unit %in% c("million fibers/l", "mf/l") ~ "1e6",
        harmonized_unit == cleaned_unit ~ "1", # If unit is not changed, factor is 1
        unit == harmonized_unit ~ "1",
        is.na(unit) ~ NA_character_,
        TRUE ~ "1" # Default factor is 1
      )
    ) %>%
    # Apply the conversion to create a harmonized numeric value
    mutate(
      numeric_conversion_factor = as.numeric(conversion_factor_str),
      # Overwrite parsed_value with the NEW, HARMONIZED value
      parsed_value = case_when(
        # Apply Fahrenheit to Celsius formula
        cleaned_unit == "°f" ~ (parsed_value - 32) * 5 / 9,
        # Apply all other numeric conversion factors
        !is.na(numeric_conversion_factor) ~
          parsed_value * numeric_conversion_factor,
        # If no conversion is defined or possible, keep the original value
        TRUE ~ parsed_value
      )
    ) %>%
    # --- Final Grouping and Calculation ---
    # Re-group by the original row ID to process ranges correctly
    group_by(.id) %>%
    mutate(
      # Identify if the value is a single value, or the high/low end of a range
      result_bin = case_when(
        n() == 1 ~ "as_is",
        n() > 1 & parsed_value == max(parsed_value) ~ "high",
        n() > 1 & parsed_value == min(parsed_value) ~ "low",
      ),
      # Calculate the final curated result (mean of the harmonized range values)
      curated_result = mean(parsed_value, na.rm = TRUE)
    ) %>%
    ungroup()

  # Export -----------------------------------------------------------------

  rio::export(curated_sswqs, file = here('final', 'sswqs.parquet'))
  rm(rebuild_is_needed)
}

if (deploy) {
  sswqs <- readRDS(file = "sswqs.RDS")
}

rm(deploy, rebuild_is_needed)

# Curation ------------------------------------------------------------------------------

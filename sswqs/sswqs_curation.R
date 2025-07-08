# packages ---------------------------------------------------------------

{
  library(here)
  library(tidyverse)
  library(janitor)
  library(V8)
  #library(httr2)

  setwd(here('sswqs'))
}

# functions --------------------------------------------------------------

pretty_casewhen <- function(var, x) {
  message(paste0(var, " == '", x, "' ~ '',\n"))
}


# Checkpoint -------------------------------------------------------------

if (!exists('sswqs') & file.exists("sswqs.RDS")) {
  # ! NOTE: If the file exists, read it in
  sswqs <- readRDS(file = "sswqs.RDS")
} else {
  # ! NOTE: If the file does not exist, run the curation script
  cli::cli_alert_info('Running curation script, this may take a while')

  #Download JS----
  {
    cli::cli_alert_info('Downloading spec')

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
    cli::cli_alert_info('Requesting state data, grab some coffee')

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
        .progress = T
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
}

# Cleaning ---------------------------------------------------------------

# Find unique non-numerical strings in the 'result' column ---------------

# The goal is to identify unique values in `sswqs$result` that are
# descriptive text (e.g., "Narrative", "Varies with hardness") rather than
# purely numerical values (e.g., "5.8", ">1000", "5.6 - 7.8").

# The most robust strategy is to define a regex for what constitutes a
# purely numerical string and then select everything that *doesn't* match.

# This regex identifies strings that are essentially numeric. It handles:
# - Optional leading inequalities ('>' or '<')
# - Numbers with decimals
# - Scientific notation (e.g., 1.23E-05)
# The `^` and `$` anchors ensure the pattern matches the *entire* string.
#numeric_only_pattern <- "^[><]?\\s*[0-9.]+(?:[eE][-+]?[0-9]+)?\\s*$"

curated_sswqs <- sswqs %>%
  # ! NOTE: temporarily select only the result column for cleaning
  #select(result) %>%
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
  mutate(
    result = str_remove_all(string = result, pattern = ","),
    result = str_trim(result),
    result = case_when(
      result == '6.90E+0.1' ~ '6.90E+01',
      .default = result
    )
  ) %>%
  rename(orig_result = result) %>%
  mutate(
    # Initialize a unique identifier for each row
    .id = 1:n(),
    # --- Start Cleaning Pipeline ---

    # 1. Create a working copy and convert to lowercase for consistency
    result = str_to_lower(orig_result),

    # 2. Handle special text cases like "million"
    result = str_replace(result, " million", "e6"),

    # 3. Remove any non-essential characters like '*' or commas
    result = str_remove_all(result, "[\\*,]"),

    # 4. Remove all whitespace to simplify subsequent patterns
    # e.g., "5.0 e - 9" becomes "5.0e-9" and "7 x 10-6" becomes "7x10-6"
    result = str_remove_all(result, "[[:space:]]"),

    # 5. Standardize scientific notation separator 'x10^' or 'x10' to 'e'
    # e.g., "5x10^-9" becomes "5e-9" and "7x106" becomes "7e6"
    result = str_replace_all(result, "x10\\^?", "e"),

    # 6. Convert the fully cleaned string to a numeric value
    parsed_value = as.numeric(result),
    #num_bool = !is.na(parsed_value),

    # 7. Fix Fortran-style notation (e.g., '4.56+02') by inserting an 'e'
    # This advanced regex finds a '+' or '-' that is preceded by a digit,
    # but NOT preceded by an 'e', and is followed by exactly two digits.
    # It then inserts an 'e' before the sign.
    # This correctly changes "4.56+02" to "4.56e+02" but leaves "5.1e-09" or "4.56+023" alone.
    # This fix is only applied to values that failed the initial numeric conversion.

    result = case_when(
      is.na(parsed_value) ~
        str_replace(
          result,
          "(?<=[0-9])(?<!e)([+-])(?=0\\d(?!\\d))",
          "e\\1"
        ),
      .default = result
    ),

    # 8. Re-parse after the fix and create final boolean flag
    parsed_value = as.numeric(result),
    num_bool = !is.na(parsed_value)
  ) %>%

  # Process each row independently
  rowwise() %>%
  # Split 'result' column by '-' if it's not a numeric value
  # Numeric values are wrapped in a list to maintain structure for unnest
  mutate(
    result = if (!num_bool) {
      str_split(result, pattern = "-")
    } else {
      list(result)
    }
  ) %>%
  # Unnest the 'result' column, expanding rows where splits occurred
  unnest(result) %>%
  # Remove row-wise grouping
  ungroup() %>%
  # 9. Re-parse the split values
  mutate(
    parsed_value = as.numeric(result),
    num_bool = !is.na(parsed_value),
  ) %>%
  filter(
    num_bool == TRUE
  ) %>%
  group_by(.id) %>%
  mutate(
    # Create `result_bin` to store a single representative value for each original result.
    # If there's only one value after splitting (e.g., "5.8"), use that value.
    # If there are multiple values (e.g., "5.6-7.8" splits into "5.6" and "7.8"),
    # select both the minimum and maximum values from the split.
    result_bin = case_when(
      # If there's only one value in the split, assign it to `result_bin`.
      n() == 1 ~ "as_is",
      # If there are multiple values, and the current row represents the maximum value in the group, assign it.
      n() > 1 & parsed_value == max(parsed_value) ~ "high",
      # If there are multiple values, and the current row represents the minimum value in the group, assign it.
      n() > 1 & parsed_value == min(parsed_value) ~ "low",
    ),
    curated_result = mean(parsed_value, na.rm = TRUE), # Calculate mean of the parsed values
  ) %>%
  ungroup()


# Unit harmonization -----------------------------------------------------

# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)

# Recreate the input data from the text provided
# In your actual script, you would load your 'curated_sswqs' object
file_content <- '
"most_common_analyte" "most_common_analyte_count" "orig_unit" "cleaned_unit"
"total dissolved solids" 29 "%" "%"
"biological integrity" 2 "% below background levels" "% below background levels"
"alkalinity" 1 "% change from natural conditions" "% change from natural conditions"
"filamentous algae" 1 "% cover" "% cover"
"turbidity increase" 42 "% increase above background" "% increase above background"
"dissolved oxygen" 51 "% of saturation value" "% of saturation value"
"sodium" 2 "% of total cations" "% of total cations"
"blue-green algae" 3 "% of total count" "% of total count"
"total coliform" 1 "% positive samples/month" "% positive samples/month"
"e. coli" 145 "CFU/100 mL or MPN/100 mL" "cfu/100 ml or mpn/100 ml"
"total bacteroides" 2 "MBN per 100 mL" "mbn/100 ml"
"fecal coliform" 47 "MPN/100 ml" "mpn/100 ml"
"sodium" 2 "SAR" "sar"
"acute toxicity" 1 "TUa" "tua"
"chronic toxicity" 1 "TUc" "tuc"
"e. coli" 6 "[no units]" "[no units]"
"microcystis aeruginosa cell density" 2 "cells/ml" "cells/ml"
"water flow" 4 "cfs" "cfs"
"color" 2 "change in PCU" "change in pcu"
"S-tube" 2 "cm" "cm"
"color" 27 "color units" "color units"
"e. coli" 326 "count/100ml" "count/100ml"
"blue-green algae" 3 "count/ml" "count/ml"
"conductivity" 6 "dS/m" "ds/m"
"light penetration" 10 "feet" "feet"
"2,3,7,8-tcdd (dioxin)" 2 "fg/l" "fg/l"
"asbestos" 49 "fibers/l" "fibers/l"
"ecosystem respiration" 1 "g O2/m2-day" "g o2/m2-day"
"dimethyl phthalate" 3 "g/l" "g/l"
"turbidity" 1 "jtu" "jtu"
"total phosphorus" 2 "kg/yr" "kg/yr"
"total phosphorus" 3 "lbs/year" "lbs/year"
"clarity" 2 "m^-1" "m^-1"
"carbonate, residual" 1 "meq/l" "meq/l"
"secchi depth" 57 "meters" "meters"
"asbestos" 3 "mf/l" "mf/l"
"ammonia" 2 "mg TAN/L" "mg tan/l"
"alkalinity" 3 "mg/L as CaCO3" "mg/l as caco3"
"methylmercury" 65 "mg/kg" "mg/kg"
"selenium" 62 "mg/kg dry wt" "mg/kg dry wt"
"methylmercury" 11 "mg/kg fish tissue" "mg/kg fish tissue"
"mercury" 1 "mg/kg wet weight" "mg/kg wet weight"
"dissolved oxygen" 819 "mg/l" "mg/l"
"total nitrogen" 6 "mg/l as n" "mg/l as n"
"chlorophyll a" 3 "mg/m2" "mg/m2"
"asbestos" 1 "microfibers/l" "microfibers/l"
"specific conductance" 2 "micromhos" "micromhos"
"asbestos" 34 "million fibers/l" "million fibers/l"
"osmotic pressure" 1 "milliosmoles/kg" "milliosmoles/kg"
"beta particles and photon activity" 3 "millirems" "millirems"
"oxidation-reduction potential" 6 "millivolts" "millivolts"
"settleable materials" 1 "ml/l" "ml/l"
"fine sediment" 4 "mm" "mm"
"e. coli" 4 "mpn" "mpn"
"beta particles and photon activity" 20 "mrem/yr" "mrem/yr"
"p,p\'-dichlorodiphenyltrichloroethane (ddt)" 35 "ng/l" "ng/l"
"turbidity" 273 "ntu" "ntu"
"fecal coliform" 39 "organisms/100 ml" "organisms/100 ml"
"total phosphorus" 5 "parts per billion (ppb)" "parts/billion (ppb)"
"radium 226 and 228 combined" 62 "pci/l" "pci/l"
"2,3,7,8-tcdd (dioxin)" 35 "pg/l" "pg/l"
"pH (acidity/alkalinity)" 279 "ph units" "ph units"
"radium 226 and 228 combined" 41 "picocuries/l" "picocuries/l"
"color" 9 "platinum cobalt units" "platinum cobalt units"
"phosphorous (yellow)" 6 "pounds per acre-foot of lake volume per year" "pounds/acre-foot of lake volume/year"
"total phosphorus" 16 "pounds per year" "pounds/year"
"dissolved oxygen" 9 "ppm" "ppm"
"2,3,7,8-tcdd (dioxin)" 4 "ppq" "ppq"
"pH (acidity/alkalinity)" 725 "standard units" "standard units"
"odor" 12 "threshold odor number" "threshold odor number"
"total dissolved solids" 1 "tons" "tons"
"total nitrogen" 2 "tons/million cubic meters of water" "tons/million cubic meters of water"
"bromoform" 1 "total thms" "total thms"
"temperature" 423 "°C" "°c"
"temperature" 206 "°F" "°f"
"specific conductance" 11 "µS/cm" "µs/cm"
"arsenic" 1 "µg/kg" "µg/kg"
"cyanide" 603 "µg/l" "µg/l"
"conductivity" 26 "µmhos/cm" "µmhos/cm"
"selenium" 6 "μg/g" "μg/g"
"chromium (vi)" 4 NA NA
'
curated_sswqs <- read_delim(file_content, delim = " ", quote = "\"", trim_ws = TRUE) %>%
  rename(unit = orig_unit)

# --- Start of the Script Logic ---

# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)

# Recreate the input data from the text provided
# In your actual script, you would load your 'curated_sswqs' object
file_content <- '
"most_common_analyte" "most_common_analyte_count" "orig_unit" "cleaned_unit"
"total dissolved solids" 29 "%" "%"
"biological integrity" 2 "% below background levels" "% below background levels"
"alkalinity" 1 "% change from natural conditions" "% change from natural conditions"
"filamentous algae" 1 "% cover" "% cover"
"turbidity increase" 42 "% increase above background" "% increase above background"
"dissolved oxygen" 51 "% of saturation value" "% of saturation value"
"sodium" 2 "% of total cations" "% of total cations"
"blue-green algae" 3 "% of total count" "% of total count"
"total coliform" 1 "% positive samples/month" "% positive samples/month"
"e. coli" 145 "CFU/100 mL or MPN/100 mL" "cfu/100 ml or mpn/100 ml"
"total bacteroides" 2 "MBN per 100 mL" "mbn/100 ml"
"fecal coliform" 47 "MPN/100 ml" "mpn/100 ml"
"sodium" 2 "SAR" "sar"
"acute toxicity" 1 "TUa" "tua"
"chronic toxicity" 1 "TUc" "tuc"
"e. coli" 6 "[no units]" "[no units]"
"microcystis aeruginosa cell density" 2 "cells/ml" "cells/ml"
"water flow" 4 "cfs" "cfs"
"color" 2 "change in PCU" "change in pcu"
"S-tube" 2 "cm" "cm"
"color" 27 "color units" "color units"
"e. coli" 326 "count/100ml" "count/100ml"
"blue-green algae" 3 "count/ml" "count/ml"
"conductivity" 6 "dS/m" "ds/m"
"light penetration" 10 "feet" "feet"
"2,3,7,8-tcdd (dioxin)" 2 "fg/l" "fg/l"
"asbestos" 49 "fibers/l" "fibers/l"
"ecosystem respiration" 1 "g O2/m2-day" "g o2/m2-day"
"dimethyl phthalate" 3 "g/l" "g/l"
"turbidity" 1 "jtu" "jtu"
"total phosphorus" 2 "kg/yr" "kg/yr"
"total phosphorus" 3 "lbs/year" "lbs/year"
"clarity" 2 "m^-1" "m^-1"
"carbonate, residual" 1 "meq/l" "meq/l"
"secchi depth" 57 "meters" "meters"
"asbestos" 3 "mf/l" "mf/l"
"ammonia" 2 "mg TAN/L" "mg tan/l"
"alkalinity" 3 "mg/L as CaCO3" "mg/l as caco3"
"methylmercury" 65 "mg/kg" "mg/kg"
"selenium" 62 "mg/kg dry wt" "mg/kg dry wt"
"methylmercury" 11 "mg/kg fish tissue" "mg/kg fish tissue"
"mercury" 1 "mg/kg wet weight" "mg/kg wet weight"
"dissolved oxygen" 819 "mg/l" "mg/l"
"total nitrogen" 6 "mg/l as n" "mg/l as n"
"chlorophyll a" 3 "mg/m2" "mg/m2"
"asbestos" 1 "microfibers/l" "microfibers/l"
"specific conductance" 2 "micromhos" "micromhos"
"asbestos" 34 "million fibers/l" "million fibers/l"
"osmotic pressure" 1 "milliosmoles/kg" "milliosmoles/kg"
"beta particles and photon activity" 3 "millirems" "millirems"
"oxidation-reduction potential" 6 "millivolts" "millivolts"
"settleable materials" 1 "ml/l" "ml/l"
"fine sediment" 4 "mm" "mm"
"e. coli" 4 "mpn" "mpn"
"beta particles and photon activity" 20 "mrem/yr" "mrem/yr"
"p,p\'-dichlorodiphenyltrichloroethane (ddt)" 35 "ng/l" "ng/l"
"turbidity" 273 "ntu" "ntu"
"fecal coliform" 39 "organisms/100 ml" "organisms/100 ml"
"total phosphorus" 5 "parts per billion (ppb)" "parts/billion (ppb)"
"radium 226 and 228 combined" 62 "pci/l" "pci/l"
"2,3,7,8-tcdd (dioxin)" 35 "pg/l" "pg/l"
"pH (acidity/alkalinity)" 279 "ph units" "ph units"
"radium 226 and 228 combined" 41 "picocuries/l" "picocuries/l"
"color" 9 "platinum cobalt units" "platinum cobalt units"
"phosphorous (yellow)" 6 "pounds per acre-foot of lake volume per year" "pounds/acre-foot of lake volume/year"
"total phosphorus" 16 "pounds per year" "pounds/year"
"dissolved oxygen" 9 "ppm" "ppm"
"2,3,7,8-tcdd (dioxin)" 4 "ppq" "ppq"
"pH (acidity/alkalinity)" 725 "standard units" "standard units"
"odor" 12 "threshold odor number" "threshold odor number"
"total dissolved solids" 1 "tons" "tons"
"total nitrogen" 2 "tons/million cubic meters of water" "tons/million cubic meters of water"
"bromoform" 1 "total thms" "total thms"
"temperature" 423 "°C" "°c"
"temperature" 206 "°F" "°f"
"specific conductance" 11 "µS/cm" "µs/cm"
"arsenic" 1 "µg/kg" "µg/kg"
"cyanide" 603 "µg/l" "µg/l"
"conductivity" 26 "µmhos/cm" "µmhos/cm"
"selenium" 6 "μg/g" "μg/g"
"chromium (vi)" 4 NA NA
'
curated_sswqs <- read_delim(file_content, delim = " ", quote = "\"", trim_ws = TRUE) %>%
  rename(unit = orig_unit)

# --- Start of the Script Logic ---

unit_harmonization <- curated_sswqs %>%
  select(analyte, unit) %>%
  filter(!is.na(unit)) %>%
  count(unit, analyte, sort = TRUE) %>%
  group_by(unit) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  rename(
    most_common_analyte = analyte,
    most_common_analyte_count = n
  ) %>%
  mutate(
    orig_unit = unit,
    cleaned_unit = unit %>%
      # Replace unicode mu characters (U+00B5; U+03BC) with "u"
      str_replace_all("[\\u00B5\\u03BC]", "u") %>%
      stringi::stri_trans_general("latin-ascii") %>%
      str_to_lower() %>%
      str_replace_all("\\s+(per|/)\\s+", "/") %>%
      str_trim(),
    
    # --- HARMONIZATION AND CONVERSION LOGIC ---
    harmonized_unit = case_when(
      # Temperature -> °c
      cleaned_unit %in% c("°c", "°f") ~ "°c",
      
      # Concentration (Mass/Volume) -> ug/l
      cleaned_unit %in% c("ug/l", "parts/billion (ppb)") ~ "ug/l",
      cleaned_unit %in% c("mg/l", "ppm") ~ "ug/l",
      cleaned_unit == "ng/l" ~ "ug/l",
      cleaned_unit == "pg/l" ~ "ug/l",
      cleaned_unit %in% c("ppq", "fg/l") ~ "ug/l", # NEW RULE
      
      # Concentration (Mass/Mass) - with careful distinctions
      cleaned_unit %in% c("mg/kg fish tissue", "mg/kg wet weight") ~ "mg/kg (wet weight)",
      cleaned_unit %in% c("ug/g", "ug/kg") ~ "mg/kg", # NEW RULE: ug/kg added
      
      # Biological Counts -> count/100ml
      cleaned_unit %in% c("count/100ml", "cfu/100 ml or mpn/100 ml", "mpn/100 ml", "organisms/100 ml") ~ "count/100ml",
      
      # pH -> ph units
      cleaned_unit %in% c("ph units", "standard units") ~ "ph units",
      
      # Conductivity -> us/cm
      cleaned_unit %in% c("us/cm", "umhos/cm", "ds/m") ~ "us/cm",
      
      # Radioactivity -> pci/l
      cleaned_unit %in% c("pci/l", "picocuries/l") ~ "pci/l",
      
      # Length/Depth -> meters
      cleaned_unit %in% c("meters", "feet") ~ "meters",
      
      # Mass loading -> kg/yr
      cleaned_unit %in% c("kg/yr", "lbs/year", "pounds/year") ~ "kg/yr",

      # Asbestos -> fibers/l
      cleaned_unit %in% c("fibers/l", "million fibers/l", "mf/l", "microfibers/l") ~ "fibers/l",
      
      # Turbidity -> ntu
      cleaned_unit %in% c("ntu", "jtu") ~ "ntu",

      # Color -> pcu
      cleaned_unit %in% c("color units", "platinum cobalt units", "change in pcu") ~ "pcu",
      
      # If no rule matches, keep the cleaned unit
      TRUE ~ cleaned_unit
    ),
    
    conversion_factor = case_when(
      # Temperature (Formula)
      cleaned_unit == "°f" ~ "°C = (°F - 32) * 5/9",
      
      # Concentration (Mass/Volume) -> ug/l
      cleaned_unit %in% c("mg/l", "ppm") ~ "1000",
      cleaned_unit == "ng/l" ~ "0.001",
      cleaned_unit == "pg/l" ~ "1e-6",
      cleaned_unit %in% c("ppq", "fg/l") ~ "1e-9", # NEW FACTOR

      # Mass/Mass -> mg/kg
      cleaned_unit == "ug/g" ~ "1",
      cleaned_unit == "ug/kg" ~ "0.001", # NEW FACTOR
      
      # Conductivity -> us/cm
      cleaned_unit == "ds/m" ~ "1000",
      
      # Length/Depth -> meters
      cleaned_unit == "feet" ~ "0.3048",
      
      # Mass loading -> kg/yr
      cleaned_unit %in% c("lbs/year", "pounds/year") ~ "0.453592",

      # Asbestos -> fibers/l
      cleaned_unit %in% c("million fibers/l", "mf/l") ~ "1e6",

      # Units that are equivalent or the target unit get a factor of 1
      cleaned_unit %in% c("°c", "ug/l", "parts/billion (ppb)", "mg/kg", "mg/kg dry wt", 
                          "mg/kg fish tissue", "mg/kg wet weight", "count/100ml", 
                          "cfu/100 ml or mpn/100 ml", "mpn/100 ml", "organisms/100 ml",
                          "ph units", "standard units", "us/cm", "umhos/cm", "meters", 
                          "kg/yr", "fibers/l", "microfibers/l", "ntu", "jtu", "pcu",
                          "color units", "platinum cobalt units", "change in pcu",
                          "pci/l", "picocuries/l") ~ "1",
      
      # No conversion possible or needed
      cleaned_unit == "[no units]" ~ NA_character_,
      
      # Default: No factor needed as unit is not being changed
      TRUE ~ "1"
    )
  ) %>%
  select(
    most_common_analyte,
    most_common_analyte_count,
    orig_unit,
    cleaned_unit,
    harmonized_unit,
    conversion_factor
  )

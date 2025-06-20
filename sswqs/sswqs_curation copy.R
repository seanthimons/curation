
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

#Download JS----
{
  cli::cli_alert_info('Downloading spec')

  cx <- v8()

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

  # ! TEMP Read in if something is goofed.
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

rm(parent_dat, units, use_class, sources, protection_dict, pollutants, pollutantRemap, entities, crit_dat)
}

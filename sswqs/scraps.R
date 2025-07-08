#OLD ---------------------------------------------------------------------

temp <- wqs_temp %>%
  #degree
  mutate(unit_name = str_remove_all(unit_name, pattern = '\\u00b0')) %>% #Removes µ
  mutate(
    unit_name = str_replace_all(
      unit_name,
      pattern = '\\u00b5',
      replacement = 'u'
    )
  ) %>%
  mutate(original_unit = unit_name) %>%
  ungroup()

#Diagnostic
w_units <- temp %>%
  count(unit_name) %>%
  arrange(desc(n))
rm(w_units)

unit_dict <- temp %>%
  distinct(unit_name, .keep_all = T) %>%
  select(unit_name, original_unit) %>%
  mutate(conversion = NA_real_) %>%
  relocate(original_unit, .before = unit_name)

rio::export(unit_dict, file = 'units_dict_dump.xlsx')

#Unit cleaning----

unit_dict <- rio::import(file = 'sswqs_units_curated.xlsx')

temp1 <- temp %>%
  select(-unit_name) %>%
  left_join(., unit_dict, join_by('original_unit')) %>%
  filter(unit_name != 'REMOVE') %>%
  filter(is.na(preferredName) | preferredName != "temperature") %>% #issues with records that differentials records
  rowwise() %>%
  mutate(
    needs_convert = !identical(unit_name, original_unit),
    orig_parsed_value = criterion_value,
    orig_range_l = range_l,
    orig_range_u = range_u
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(criterion_value, range_l, range_u),
      ~ case_when(
        needs_convert == TRUE ~ . * conversion,
        .default = .
      )
    )
  )


# missing protection data -------------------------------------------------

temp2 <- temp1 %>%
  mutate(
    meta = NA_character_,
    cit = 'State-Specific Water Quality Standards',
    protection = case_when(
      criteriatypeaquahumhlth == 'A' ~ 'Aquatic',
      criteriatypeaquahumhlth == 'H' ~ 'Human',
      criteriatypeaquahumhlth == 'O' ~ 'Organoleptic',
      .default = criteriatypeaquahumhlth
    ),
    sourcewater = case_when(
      criteriatypefreshsaltwater == 'S' ~ 'Salt Water',
      criteriatypefreshsaltwater == 'B' ~ 'Brackish',
      criteriatypefreshsaltwater == 'F' ~ 'Fresh Water',
      criteriatypefreshsaltwater == 'SW' ~ 'Storm Water',
      criteriatypefreshsaltwater == 'I' ~ 'Industry Process Water',
      criteriatypefreshsaltwater == 'MN' ~ 'Treated Municipal Wastewater',
      criteriatypefreshsaltwater == 'CW' ~ 'Onsite Collected Waters',
      .default = criteriatypefreshsaltwater
    ),
    duration = case_when(
      criteriatype_acutechronic == 'A' ~ 'Acute',
      criteriatype_acutechronic == 'C' ~ 'Chronic',
      criteriatype_acutechronic == 'S' ~ 'Sample',
      criteriatype_acutechronic == 'D' ~ 'Daily',
      criteriatype_acutechronic == 'W' ~ 'Weekly',
      criteriatype_acutechronic == 'M' ~ 'Monthly',
      criteriatype_acutechronic == 'Y' ~ 'Yearly',
      .default = criteriatype_acutechronic
    ),
    #NOTE Update to endpoint + combine wuth protection?
    enduse = case_when(
      criteriatype_waterorg == 'O' ~ 'Organism',
      criteriatype_waterorg == 'W' ~ 'Water & Organism',
      criteriatype_waterorg == 'A' ~ 'Agriculture',
      criteriatype_waterorg == 'L' ~ 'Landscaping',
      criteriatype_waterorg == 'NPR' ~ 'Centralized Non-Potable Reuse',
      criteriatype_waterorg == 'PWR' ~ 'Potable Water Reuse',
      criteriatype_waterorg == 'I' ~ 'Impoundments',
      criteriatype_waterorg == 'ER' ~ 'Environmental Restoration',
      criteriatype_waterorg == 'IND' ~ 'Industry',
      criteriatype_waterorg == 'NPWR' ~ 'Onsite Non-Potable Water Reuse',
      criteriatype_waterorg == 'LIV' ~ 'Livestock',
      .default = criteriatype_waterorg
    )
  ) %>%
  rename(
    local = use_class_name_location_etc
  ) %>%
  ungroup() %>%
  select(-c(criteriatypeaquahumhlth:criteriatype_waterorg)) %>%
  select(
    criterion_id:entity_name,
    final:preferredName,
    criterion_value,
    unit_name,
    IS_RANGE,
    range_l,
    range_u,
    local,
    meta:enduse
  ) %>%
  mutate(across(protection:enduse, ~ replace_na(., 'UNC')))

dtx <- temp2 %>%
  filter(final != 'bulk') %>%
  distinct(final, .keep_all = F)

dtx <- ct_details(query = dtx$final, projection = 'id') %>%
  select(dtxsid, preferredName)

temp3 <- temp2 %>%
  split(is.na(.$preferredName)) %>%
  map(., ~ rename(., dtxsid = final))

temp3$`TRUE` <- temp3$`TRUE` %>%
  select(-preferredName) %>%
  inner_join(., dtx, join_by(dtxsid))

temp3 <- list_rbind(temp3)

entity_list <- temp3 %>%
  distinct(., entity_name, .keep_all = F) %>%
  mutate(
    source = 'State-Specific Water Quality Standards',
    subsource = NA_character_,
    origin_category = case_when(
      entity_name %in%
        c(
          state.name, #from dataset package
          'California - Statewide',
          'California Region',
          'CTR - California Toxics Rule'
        ) ~
        'State',
      str_detect(entity_name, 'California Region') ~ 'State',
      entity_name %in%
        c(
          #NOTE Adjust this to mirror other data sets
          'EPA 304(a) Recommended Criteria',
          'NTR - National Toxics Rule'
        ) ~
        'Federal',
      .default = 'Other'
    ),
    origin_supercategory = NA_character_,
    origin_agency = NA_character_,
    data_category = 'Primary',
    priority_id = case_when(
      origin_category == 'State' ~ 1,
      origin_category == 'Federal' ~ 1,
      .default = 3 #For tribes and others
    )
  )

temp4 <- temp3 %>%
  left_join(., entity_list, join_by('entity_name')) %>%
  rename(is_range = IS_RANGE) %>%
  relocate(protection:enduse, .after = range_u)

# TODO Change to RDS format
# rio::export(temp4, file = paste0('sswqs_curated_', Sys.Date(), '.xlsx'))
#
# rm(list = ls())

# copy -------------------------------------------------------------------

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

  # ! NOTE: JS parsing here
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

  saveRDS(state_dat, file = "sswqs_state_dat.RDS")
  saveRDS(parent_dat, file = "sswqs_parent_dat.RDS")
}

# ! TEMP
state_dat <- readRDS(file = "sswqs_state_dat.RDS")
parent_dat <- readRDS(file = "sswqs_parent_dat.RDS")

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

  protection_dict <- crit_dat %>%
    select(protection) %>%
    distinct(protection) %>%
    mutate(
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
      # !
      location = case_when(
        str_detect(endpoint, 'Fresh') ~ 'freshwater',
        str_detect(endpoint, 'Salt') ~ 'saltwater',
        str_detect(endpoint, 'Brackish') ~ 'brackish'
      ),
      # !
      application = case_when(
        str_detect(endpoint, 'Human') ~ 'human health',
        str_detect(endpoint, 'Aquatic') ~ 'aquatic life',
        str_detect(endpoint, 'Organoleptic') ~ 'organoleptic'
      ),
      # !
      exposure = case_when(
        str_detect(endpoint, 'Water \\+ Organism') ~ 'water and organism',
        str_detect(endpoint, 'Organism Only') ~ 'organism only',
      ),
      # !
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

  rm(state_vars)

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

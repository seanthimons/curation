# Packages ----------------------------------------------------------------

{
  # CRAN Packages ----
  install_booster_pack <- function(package, load = TRUE) {
    # Loop through each package
    for (pkg in package) {
      # Check if the package is installed
      if (!requireNamespace(pkg, quietly = TRUE)) {
        # If not installed, install the package
        install.packages(pkg)
      }
      # Load the package
      if (load) {
        library(pkg, character.only = TRUE)
      }
    }
  }

  if (file.exists('packages.txt')) {
    packages <- read.table('packages.txt')

    install_booster_pack(package = packages$Package, load = FALSE)

    rm(packages)
  } else {
    # Packages ----

    booster_pack <- c(
      ## IO ----
      'fs',
      'here',
      'janitor',
      'rio',
      'tidyverse',
      #	'data.table',
      'mirai',
      # 'targets',
      # 'crew',

      ## DB ----
      #  'arrow',
      'nanoparquet',
      #  'duckdb',
      #  'duckplyr',
      #  'dbplyr',

      ## EDA ----
      'skimr',

      ## Web ----
      'rvest',
      'polite',
      #	'plumber',
      # 'plumber2', #Still experimental
      'httr2',

      ## Plot ----
      # 'paletteer',
      # 'ragg',
      # 'camcorder',
      # 'esquisse',
      # 'geofacet',
      # 'patchwork',
      # 'marquee',
      # 'ggiraph',
      # 'geomtextpath',
      # 'ggpattern',
      # 'ggbump',
      # 'gghighlight',
      # 'ggdist',
      # 'ggforce',
      # 'gghalves',
      # 'ggtext',
      # 'ggrepel', # Suggested for non-overlapping labels
      # 'gganimate', # Suggested for animations
      # 'ggsignif',
      # 'ggTimeSeries',
      # 'tidyheatmaps',

      ## Modeling ----
      # 'tidymodels',

      ## Shiny ----
      # 'shiny',
      # 'bslib',
      # 'DT',
      # 'plotly',

      ## Reporting ----
      # 'quarto',
      # 'gt',

      ## Spatial ----
      # 'sf',
      # 'geoarrow',
      # 'duckdbfs',
      # 'duckspatial',
      # 'ducksf',
      # 'tidycensus', # Needs API
      # 'mapgl',
      # 'dataRetrieval', # Needs API
      # 'StreamCatTools',

      ## Misc ----
      'devtools',
      # 'usethis',
      # 'pak',
      'remotes'
    )

    # ! Change load flag to load packages
    install_booster_pack(package = booster_pack, load = TRUE)
    rm(install_booster_pack, booster_pack)
  }

  # GitHub Packages ----
  github_packages <- c(
    "seanthimons/ComptoxR"
  )

  # Ensure remotes is installed
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }

  # Loop through each GitHub package
  for (pkg in github_packages) {
    # Extract package name from the "user/repo" string
    pkg_name <- sub(".*/", "", pkg)

    # Check if the package is installed
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      # If not installed, install the latest release from GitHub
      remotes::install_github(paste0(pkg, "@*release"))
    }
    # Load the package
    library(pkg_name, character.only = TRUE)
  }

  rm(github_packages, pkg, pkg_name)

  # Custom Functions ----

  `%ni%` <- Negate(`%in%`)

  # skim_count <- skim_with(
  # 	numeric = sfl(
  # 		n = length,
  # 		min = ~ min(.x, na.rm = T),
  # 		median = ~ median(.x, na.rm = T),
  # 		max = ~ max(.x, na.rm = T)
  # 	)
  # )

  # Camcorder ----

  # if(!dir.exists(here::here('output'))) {
  #   dir.create(here::here('output'))
  # }

  # gg_record(
  # 	here::here('output'),
  # 	device = "png",
  # 	width = 10,
  # 	height = 7,
  # 	units = "in",
  # 	dpi = 320
  # )

  # Theme ----

  # theme_custom <- function() {
  # 	theme_minimal() +
  # 		theme(
  # 			plot.background = element_rect(colour = "white"),
  # 			panel.grid.major = element_blank(),
  # 			panel.grid.minor = element_blank(),
  # 			strip.background = element_rect(colour = "white"),
  # 			axis.text.x = element_text(angle = 90L)
  # 		)
  # }

  setwd(here::here('epa', 'nwqs'))

  #' SRS search
  #'
  #' @param query
  #' @param method
  #'
  #' @returns
  #' @export

  srs_search <- function(query, method) {
    request(
      "https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/autoComplete/nameSearch"
    ) |>
      req_url_query(
        #begins, contains, exact
        term = query,
        qualifier = method
      ) |>
      req_headers(
        accept = "*/*"
      ) |>
      #req_dry_run()
      req_perform() %>%
      resp_body_json() %>%
      map(., as_tibble) %>%
      list_rbind()
  }

  #' SRS details
  #'
  #' @param query
  #'
  #' @returns
  #' @export

  srs_details <- function(query) {
    request(
      "https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/substance/itn/"
    ) |>
      req_url_path_append(query) %>%
      # req_url_query(
      #   excludeSynonyms = "true"
      # ) |>
      req_headers(
        accept = "application/json"
      ) |>
      #req_dry_run()
      req_perform() %>%
      resp_body_json() %>%
      pluck(., 1) %>%
      modify_at(
        "synonyms",
        ~ length(.x)
      ) %>%
      flatten() %>%
      compact() %>%
      map(
        .,
        ~ if (is.null(.x)) {
          NA
        } else {
          .x
        }
      ) %>%
      as_tibble()
  }
}
# Raw --------------------------------------------------------------------

# 403 error
# download.file(
# 	destfile = here::here("epa", "nwqs", "mn-npdws.xlsx"),
# 	url = 'https://www.web.health.state.mn.us/communities/environment/risk/docs/guidance/waterguidance.xlsx',
# 	mode = 'wb'
# )

## National Primary Drinking Water Regulations -----------------------------

request(
  'https://www.web.health.state.mn.us/communities/environment/risk/docs/guidance/waterguidance.xlsx'
) %>%
  # Add user agent to mimic a browser
  req_headers(
    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.11'
  ) %>%
  req_perform(
    path = here::here("epa", "nwqs", "mn-npdws.xlsx")
  )

raw <- rio::import(here::here("epa", "nwqs", "mn-npdws.xlsx")) %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names(
    replace = janitor:::mu_to_u
  )

npdw <- raw %>%
  select(
    chemical,
    cas_number,
    epa_mcl = epa_mcl_ug_l,
    epa_mclg = epa_mclg_ug_l,
    lowest_epa_ha = lowest_epa_health_advisory_ug_l,
    ha_type = type_of_ha_value
  )

request(
  "https://www.web.health.state.mn.us/communities/environment/risk/docs/guidance/gw/guidance.xlsx"
) %>%
  # Add user agent to mimic a browser
  req_headers(
    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.11'
  ) %>%
  req_perform(
    path = here::here("epa", "nwqs", "mn-guidance.xlsx")
  )

guidance <- rio::import(
  here::here("epa", "nwqs", "mn-guidance.xlsx"),
  sheet = 'AllGuidance'
) %>%
  janitor::row_to_names(2) %>%
  janitor::clean_names()


# cleaning ----------------------------------------------------------------

raw <- list_rbind(
  list('dw' = dw, 'alc' = alc, 'hhc' = hhc, 'oe' = oe),
  names_to = 'frame'
)

write_rds(raw, 'raw.RDS')

rm(alc, hhc, oe, dw)

raw_dat <- raw %>%
  distinct(analyte, casrn, .keep_all = F) %>%
  mutate(
    idx = 1:n(),
    cas_chk = webchem::as.cas(casrn),
    raw_name = str_remove_all(analyte, pattern = "\\([^()]*\\)") %>%
      str_squish()
  )

bad <- raw_dat %>%
  filter(is.na(cas_chk)) %>%
  select(-c(casrn, cas_chk))

bad_search <- ct_search(
  query = bad$raw_name,
  request_method = 'GET',
  search_method = 'exact'
)

bad_src <- bad_search %>%
  arrange(rank) %>%
  distinct(raw_search, .keep_all = T)

bad_dat <- inner_join(bad, bad_src, join_by(raw_name == raw_search)) %>%
  select(idx, dtxsid)

rm(bad, bad_search, bad_src)

good <- raw_dat %>%
  filter(!is.na(cas_chk)) %>%
  mutate(cas_chk = unname(cas_chk))

good_cas <- ct_search(
  query = good$cas_chk,
  request_method = 'GET',
  search_method = 'exact'
)

good_src <- good_cas %>%
  arrange(rank) %>%
  distinct(raw_search, .keep_all = T)

good_dat <- inner_join(good, good_src, join_by(cas_chk == raw_search)) %>%
  select(idx, dtxsid)

rm(good, good_cas, good_src)

raw_cur <- bind_rows(list(good_dat, bad_dat)) %>%
  left_join(raw_dat, ., join_by(idx)) %>%
  select(-raw_name, -casrn, -cas_chk) %>%
  arrange(dtxsid)

# hashing -----------------------------------------------------------------
{
  params <- raw_cur %>%
    filter(is.na(dtxsid)) %>%
    select(idx, analyte) %>%
    mutate(
      raw_search = str_remove_all(analyte, pattern = '\\*') %>%
        str_remove_all(., pattern = '\\(P\\)') %>%
        str_remove_all(., pattern = "\\s*\\([^()]*\\)$") %>%
        str_remove_all(., pattern = "\\s*\\([^()]*\\)$") %>%
        str_squish()
    )

  {
    srs_e <- map(
      params$raw_search,
      ~ srs_search(query = .x, method = 'exact'),
      .progress = T
    ) %>%
      set_names(., params$raw_search) %>%
      list_rbind(names_to = 'raw_search')

    srs_e_details <- map(
      srs_e$itn,
      ~ srs_details(query = .x),
      .progress = T
    ) %>%
      set_names(., srs_e$raw_search) %>%
      list_rbind(names_to = 'raw_search') %>%
      group_by(raw_search) %>%
      arrange(desc(synonyms)) %>%
      ungroup() %>%
      distinct(raw_search, .keep_all = T)

    missing <- params %>%
      filter(!raw_search %in% srs_e_details$raw_search) %>%
      select(raw_search) %>%
      unlist() %>%
      unname() %>%
      print()

    srs_c <- map(
      missing,
      ~ srs_search(query = .x, method = 'contains'),
      .progress = T
    ) %>%
      set_names(., missing) %>%
      list_rbind(names_to = 'raw_search')

    srs_c_details <- map(
      srs_c$itn,
      ~ srs_details(query = .x),
      .progress = T
    ) %>%
      set_names(., srs_c$raw_search) %>%
      list_rbind(names_to = 'raw_search') %>%
      group_by(raw_search) %>%
      arrange(desc(synonyms)) %>%
      ungroup() %>%
      distinct(raw_search, .keep_all = T)

    missing <- params %>%
      filter(
        !raw_search %in% c(srs_c_details$raw_search, srs_e_details$raw_search)
      ) %>%
      #select(raw_search) %>%
      #unlist() %>%
      #unname() %>%
      print()

    # ! NOTE Manual process here!
    missing <- missing %>%
      mutate(
        itn = case_when(
          raw_search == 'Hazard Index PFAS' ~ '1995414',
          raw_search == 'Radium 226 and Radium 228' ~ '701037'
        )
      ) %>%
      select(-analyte, -idx)

    missing_src <- map(missing$itn, ~ srs_details(query = .x)) %>%
      list_rbind()

    missing_dat <- missing %>%
      left_join(., missing_src, join_by(itn == internalTrackingNumber)) %>%
      rename(internalTrackingNumber = itn)

    rm(srs_e, srs_c)

    params_cur <- list(missing_dat, srs_e_details, srs_c_details) %>%
      list_rbind() %>%
      select(
        raw_search,
        internalTrackingNumber,
        systematicName,
        currentCasNumber,
        dtxsid
      ) %>%
      `colnames<-`(
        .,
        c(
          'raw_search',
          'itn',
          'preferredName',
          'casrn',
          'dtxsid'
        )
      )

    #TODO need to join against raw names for final list.

    missing <- params %>%
      anti_join(., params_cur, join_by(raw_search))

    print(missing)
  }
}

params_fin <- params_cur %>%
  left_join(params, ., join_by(raw_search)) %>%
  select(-casrn, -raw_search)

raw_cur <- raw_cur %>%
  filter(!is.na(dtxsid))

final <- bind_rows(params_fin, raw_cur) %>%
  mutate(
    preferredName = case_when(
      !is.na(dtxsid) ~ NA,
      is.na(dtxsid) ~ preferredName
    )
  )

dss <- final %>%
  filter(!is.na(dtxsid)) %>%
  select(-preferredName, -itn)

#NOTE Some duplicates here
dss_cur <- ct_details(dss$dtxsid) %>%
  select(-casrn)

dss <- dss %>%
  left_join(., dss_cur, join_by(dtxsid))

wq <- final %>%
  filter(!idx %in% dss$idx) %>%
  mutate(dtxsid = paste0('E', itn)) %>%
  select(-itn) %>%
  mutate(
    preferredName = str_remove_all(
      preferredName,
      pattern = ', from SDWA NPDWR|-- SWDA NPDWR'
    )
  )

final_dictionary <- bind_rows(wq, dss) %>%
  arrange(idx) %>%
  select(-idx) %>%
  distinct(.keep_all = T)


# Final pairing -----------------------------------------------------------
{
  ndwqs <- raw %>%
    #select(-casrn) %>%

    # NOTE debugging
    # anti_join(., final_dictionary, join_by(analyte)) %>%

    left_join(., final_dictionary, join_by(analyte)) %>%
    select(-year) %>%
    mutate(
      idx = 1:n(),
      #source = if_else(is.na(source), 'ND', source),
      notes = case_when(
        notes == '' ~ NA,
        .default = notes
      ),
      orig_value = value,
      value = str_remove_all(
        value,
        pattern = '\\u2014|ug/L|,|--|---|none|vacated|NP1'
      ),
      value = str_remove_all(value, pattern = 'TT.*|n/a.*'),
      value = str_replace_all(value, pattern = '\\u2013', '-'),
      value = str_squish(value),
      value = str_replace_all(value, pattern = 'zero', replacement = "0"),
      value = na_if(value, "")
    ) %>%
    filter(!is.na(value))

  # Simple value parsing ----
  n_1 <- ndwqs %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))

  # More complex value parsing ----
  n_2 <- ndwqs %>%
    filter(idx %ni% n_1$idx) %>%
    mutate(
      unit = case_when(
        preferredName == 'Alpha particle' ~ 'pCi/L',
        preferredName == 'Asbestos' ~ 'million fibers/L',
        preferredName == 'Beta Particles And Photon Emitters' ~ 'mrem/yr',
        preferredName == 'Methyl mercury(II) cation' ~ 'mg/kg',
        preferredName ==
          'Radium, isotope of mass 226 and/or radium, isotope of mass 228' ~
          'pCi/L',
        preferredName == 'Total coliforms' ~ '%',
        .default = unit
      ),
      value = case_when(
        preferredName == 'Perfluorooctanoic acid' |
          preferredName == 'Perfluorooctanesulfonate' ~
          orig_value %>%
            str_replace_all("\\R+", "\\__") %>%
            str_remove_all("\\t+"),
        preferredName == 'pH' | preferredName == 'Benzene' ~
          str_replace_all(
            value,
            pattern = '(?<=\\d)\\s*-\\s*(?=\\d)',
            replacement = "-"
          ),
        .default = value
      )
    ) %>%
    separate_longer_delim(., value, delim = '__') %>%
    mutate(
      value = case_when(
        preferredName == 'Alpha particle' ~ '15',
        preferredName == 'Asbestos' ~ '7',
        preferredName == 'Arsenic' ~ '0.01',
        preferredName == 'Beta Particles And Photon Emitters' ~ '4',
        preferredName == 'Methyl mercury(II) cation' ~ '0.3',
        preferredName ==
          'Radium, isotope of mass 226 and/or radium, isotope of mass 228' ~
          '5',
        preferredName == 'Total coliforms' ~ '5',
        preferredName == 'Uranium' ~ '30',
        value == 'Total' ~ NA,
        .default = value
      ),
      #value = str_squish(value),
      #value = str_remove_all(value, pattern = '[[:SPACE:]]')
    ) #%>% filter(!is.na(value))

  # ! NOTE May not be neded if ranges are handled properly.
  # {
  #   n_3$`TRUE` %<>%
  #     mutate(
  #       idx_r = 1:n()
  #     ) %>%
  #     separate_longer_delim(., value, delim = '-') %>%
  #     mutate(
  #       value = as.numeric(value),
  #       is_range = TRUE,
  #       n_r = NA
  #     ) %>%
  #     filter(!is.na(value)) %>%
  #     group_by(idx) %>%
  #     mutate(
  #       n_r = case_when(
  #         min(value) == value ~ 'Lower range',
  #         max(value) == value ~ 'Upper range'
  #       )
  #     ) %>%
  #     unite(., col = 'notes', n_r, notes, sep = '; ', remove = T, na.rm = T) %>%
  #     select(-idx_r) %>%
  #     ungroup()

  #   n_3 <- list_rbind(n_3)
  # }

  ndwqs_final <- bind_rows(
    n_2,
    n_1
  ) %>%
    ungroup() %>%
    mutate(
      idx = 1:n(),
      is_range = if_else(is.na(is_range), FALSE, is_range),
      endpoint = case_when(
        name == "mclg" ~ 'MCLG',
        name == "mcl" ~ 'MCL',
        name == "fw_ac" ~ 'Aquatic life',
        name == "fw_chr" ~ 'Aquatic life',
        name == "sw_ac" ~ 'Aquatic life',
        name == "sw_chr" ~ 'Aquatic life',
        name == "hh_wo" ~ 'Human health - Water & Organism',
        name == "hh_o" ~ 'Human health - Organism',
        .default = 'Organoleptic'
      ),
      sourcewater = case_when(
        str_detect(name, 'fw_') ~ 'Fresh water',
        str_detect(name, 'sw_') ~ 'Salt water',
        .default = NA
      ),

      data_category = 'Primary',

      duration = case_when(
        str_detect(name, '_ac') ~ 'Acute',
        str_detect(name, '_chr') ~ 'Chronic',
        .default = NA
      ),

      meta = NA,

      cit = case_when(
        frame == 'dw' ~ 'National Primary Drinking Water Regulations',
        frame != 'dw' ~
          'Clean Water Act 304 (a): National Recommended Water Quality Criteria'
      ),
    ) %>%
    arrange(idx) %>%
    mutate(
      value = case_when(
        preferredName == 'pH' ~ value,
        unit == "ug/l" ~ value / 1000,
        .default = value
      ),
      unit = case_when(
        preferredName == 'pH' ~ 'standard units',
        unit == "ug/l" ~ "mg/L",
        .default = unit
      ),
      # across(
      #   .cols = everything(),
      #   .fns = ~ if_else(is.na(.), "ND", as.character(.))
      # ),
      value = as.numeric(value)
    ) %>%
    select(
      -source,
      -frame,
      -name,
      -analyte,
      is_range,
      'DTXSID' = dtxsid,
      'CASRN' = casrn,
      'NAME' = preferredName,
      'SOURCE' = cit,
      'TOXVAL_TYPE' = endpoint,
      'TOXVAL_NUMERIC' = value,
      'TOXVAL_UNITS' = unit
    ) %>%
    mutate(
      'SUB_SOURCE' = 'EPA OW',
      'TOXVAL_TYPE_SUPERCATEGORY' = 'Media Exposure Guidelines',
      'QUALIFIER' = case_when(
        is_range == TRUE ~ '~',
        .default = '='
      ),
      'RISK_ASSESSMENT_CLASS' = 'Water',
      'STUDY_TYPE' = 'Media Exposure Guidelines',
      'STUDY_DURATION_VALUE' = "-999",
      'SPECIES_COMMON' = 'Human',
      'LATIN_NAME' = 'Homo sapiens',
      'SPECIES_SUPERCATEGORY' = 'Mammals',
      'EXPOSURE_ROUTE' = 'oral',
      .keep = 'unused'
    )
}

#write_rds(ndwqs_final, here('final', 'nwqs.RDS'))

#write_parquet(ndwqs_final, sink = here('final', 'nwqs.parquet'))

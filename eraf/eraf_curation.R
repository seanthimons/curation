{
  library(here)
  library(readxl)
  library(tidyverse)
  library(rio)
  library(janitor)
  library(ComptoxR)
}

raw_eraf <- rio::import(
  file = here("eraf", "SurfaceWaterBenchmarks_ERAF.WG18.xlsx")
)

names(raw_eraf) <-
  c(
    'chemical',
    'cas',
    'fw_chron',
    'fw_acute',
    'fw_source',
    'sw_chron',
    'sw_acute',
    'sw_source'
  )

eraf <- raw_eraf %>% filter(!is.na(cas))

eraf_fw <- eraf %>%
  select(chemical:fw_source) %>%
  pivot_longer(cols = c(fw_chron, fw_acute)) %>%
  rename(source = fw_source) %>%
  split(., ~ str_detect(.$source, pattern = ',')) %>%
  set_names(., c('good', 'bad')) %>%
  map(
    .,
    ~ {
      .x %>%
        mutate(source = str_remove_all(source, "\\s"))
    }
  ) %>%
  map(
    .,
    ~ {
      .x %>%
        mutate(
          source_fix = if_else(
            .$name == 'fw_chron',
            str_trim(str_extract(source, "^[^,]+")),
            str_trim(str_extract(source, "(?<=,).*$"))
          )
        ) %>%
        mutate(
          source_fix = case_when(
            is.na(source_fix) ~ source,
            .default = source_fix
          )
        )
    }
  ) %>%
  list_rbind()

eraf_sw <- eraf %>%
  select(chemical:cas, sw_chron:sw_source) %>%
  pivot_longer(cols = c(sw_chron, sw_acute)) %>%
  rename(source = sw_source) %>%
  split(., ~ str_detect(.$source, pattern = ',')) %>%
  set_names(., c('good', 'bad')) %>%
  map(
    .,
    ~ {
      .x %>%
        mutate(source = str_remove_all(source, "\\s"))
    }
  ) %>%
  map(
    .,
    ~ {
      .x %>%
        mutate(
          source_fix = if_else(
            .$name == 'sw_chron',
            str_trim(str_extract(source, "^[^,]+")),
            str_trim(str_extract(source, "(?<=,).*$"))
          )
        ) %>%
        mutate(
          source_fix = case_when(
            is.na(source_fix) ~ source,
            .default = source_fix
          )
        )
    }
  ) %>%
  list_rbind()

eraf <- list_rbind(list(eraf_fw, eraf_sw))

rm(eraf_fw, eraf_sw)

cit <- raw_eraf %>%
  .[329:368, 1] %>%
  str_split(., pattern = '-', n = 2, simplify = T) %>%
  as_tibble() %>%
  rename(key = V1, msg = V2) %>%
  mutate(across(key:msg, ~ str_trim(.x))) %>%
  filter(!is.na(key) & str_length(key) < 4) %>%
  add_row(key = '^*', msg = paste0(.$msg[1], " ", .$msg[2]))

eraf_cur <- eraf %>%
  mutate(
    .,
    units = 'ug/l',

    ft = str_remove_all(
      chemical,
      pattern = '[[:alnum:]]|\\(|\\)|\\.|\\,|\\/|\\-|\\s'
    ),
    ft = na_if(ft, ""),

    source_fix = na_if(source_fix, "--"),

    value = na_if(value, "--"),

    cas_fix = str_remove_all(cas, pattern = '[[:alpha:]]'),

    source_water = case_when(
      str_detect(name, pattern = 'fw') ~ 'FW',
      str_detect(name, pattern = 'sw') ~ 'SW'
    ),

    duration = case_when(
      str_detect(name, pattern = 'chron') ~ 'C',
      str_detect(name, pattern = 'acute') ~ 'A'
    )
  ) %>%
  left_join(., cit, join_by(ft == key), relationship = "many-to-many") %>%
  left_join(
    .,
    cit,
    join_by(source_fix == key),
    relationship = "many-to-many"
  ) %>%
  rename(meta = msg.x, cit = msg.y) %>%
  mutate(
    cit = case_when(
      source_fix == 'w' ~
        'Minimum freshwater acute value in ECOTOX divided by default ACR',
      source_fix == 'y' ~ 'Maine State Standard',
      .default = cit
    ),

    origin_category = case_when(
      str_detect(cit, pattern = 'EPA|National|GLI|OPP|ECOSAR|ECOTOX') ~
        'Federal',
      str_detect(cit, str_c(state.name, collapse = "|")) ~ "State",
      str_detect(cit, pattern = 'Michican') ~ 'State',
      str_detect(cit, pattern = 'Canada|CCME') ~ 'International',
      .default = 'Other'
    ) %>%
      fct_relevel(
        .,
        c(
          'Federal',
          'State',
          'International',
          'Acadmic',
          'Industry',
          'Other'
        )
      ),

    data_category = case_when(
      str_detect(cit, pattern = 'GLI|ECOSAR') ~ 'secondary',
      origin_category == 'Federal' | origin_category == 'State' ~ 'primary',
      origin_category == 'Academic' ~ 'secondary',
      origin_category == 'International' ~ 'primary',
      .default = 'secondary'
    ) %>%
      fct_relevel(., c('primary', 'secondary')),

    rank = as.numeric(origin_category) * as.numeric(data_category),

    enduse = case_when(
      str_detect(chemical, pattern = 'wildlife|Wildlife') ~ 'eco',
      str_detect(chemical, pattern = 'aquatic|Aquatic') ~ 'aquatic',
      .default = 'aquatic'
    ),
  ) %>%
  filter(!is.na(value)) %>% #removes records that don't have value
  select(
    -c(
      source,
      source_fix,
      ft,
      name
    )
  )

eraf_dis <- eraf_cur %>%
  distinct(cas_fix)

eraf_dis <- ComptoxR::ct_search(
  type = 'string',
  search_param = 'equal',
  query = eraf_dis$cas_fix
)

eraf_dict <- eraf_dis %>%
  arrange(rank) %>%
  distinct(searchValue, .keep_all = T) %>%
  select(dtxsid, preferredName, searchValue)

eraf_final <- eraf_cur %>%
  select(-cas) %>%
  left_join(., eraf_dict, join_by(cas_fix == searchValue)) %>%
  select(
    -c(
      chemical,
      cas_fix
    )
  )

write_rds(eraf_final, file = here(paste0('eraf_final_', Sys.Date(), '.RDS')))

rm(eraf, eraf_cur, eraf_dict, eraf_dis, raw_eraf, cit)

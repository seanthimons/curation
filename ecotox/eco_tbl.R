# packages ----------------------------------------------------------------

{
  source('ecotox.R')
  library(ComptoxR)
}


# Documentation -----------------------------------------------------------

# https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/technical-overview-ecological-risk-assessment-0
# https://sitem.herts.ac.uk/aeru/ppdb/en/docs/2_5eco.pdf

# query <- ct_list('NEUROTOXINS') %>%
#   pluck(., 1, 'dtxsids') %>%
#   ct_details(query = .)

# -------------------------------------------------------------------------

#need to filter for exposure types + groups to satisfy the dictionary from PPDB
#c('AQUA', 'ENV', 'ORAL', 'TOP', 'Unspecified', 'UNK')


eco_risk_tbl <- post_results(
  casrn = '50-00-0'
)
  left_join(test_type_rules, by = "eco_group", relationship = "many-to-many") %>%
  rowwise() %>%
  mutate(
    test_type = if_else(
      (is.null(effect.y) || (is.expression(effect.y) && eval(effect.y)) || effect.x %in% effect.y) &&
        (is.null(exposure_group.y) ||
          exposure_group.x %in% exposure_group.y ||
          (is.na(exposure_group.x) && any(is.na(exposure_group.y)))) &&
        (is.null(new_unit.y) || new_unit.x %in% new_unit.y) &&
        (is.null(endpoint.y) || endpoint.x %in% endpoint.y) &&
        (is.null(dur_check) || eval(dur_check)),
      test_type.y,
      NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(test_type)) %>%
  distinct(result_id, .keep_all = TRUE) %>%
  select(-ends_with(".y"), -dur_check) %>%
  rename_with(~ str_remove(.x, "\\.x$"), ends_with(".x"))


# Binned table -----------------------------------------------------------


eco_risk_bin_tbl <- eco_risk_tbl %>%
  select(
    test_cas,
    test_type,
    eco_group,
    new_value
  ) %>%
  fuzzy_left_join(
    risk_binning_rules,
    by = c(
      "eco_group" = "eco_group",
      "test_type" = "test_type",
      "new_value" = "lower_bound",
      "new_value" = "upper_bound"
    ),
    match_fun = list(`==`, `==`, `>`, `<=`)
  ) %>%
  select(
    test_cas,
    test_type = test_type.x,
    eco_group = eco_group.x,
    new_value,
    bin
  ) %>%
  mutate(
    super_int = case_when(
      bin == 'XH' ~ 6,
      bin == 'VH' ~ 5,
      bin == 'H' ~ 4,
      bin == 'M' ~ 3,
      bin == 'L' ~ 2,
      bin == 'VL' ~ 1,
      bin == 'ND' ~ 0,
      is.na(bin) ~ 0,
      .default = 0
    )
  ) %>%
  filter(!is.na(bin)) %>%
  group_by(test_cas, test_type, eco_group) %>%
  mutate(
    super_bin = ceiling(
      mean(super_int)
    ),
    super_bin = case_when(
      super_bin >= 6 ~ 'XH',
      super_bin == 5 ~ 'VH',
      super_bin == 4 ~ 'H',
      super_bin == 3 ~ 'M',
      super_bin == 2 ~ 'L',
      super_bin == 1 ~ 'VL',
      super_bin == 0 ~ 'ND',
      is.na(super_bin) ~ 'ND'
    )
  ) %>%
  ungroup() %>%
  distinct(
    test_cas,
    test_type,
    eco_group,
    super_bin
  ) %>%

  #Note need to remove duplicates
  pivot_wider(
    .,
    id_cols = test_cas,
    names_from = c(eco_group, test_type),
    names_sort = TRUE,
    values_from = c(
      super_bin
    )
  )
select(
  test_cas,
  Birds_acute,
  Fish_acute,
  Fish_chronic,
  Mammals_acute,
  Mammals_chronic,
  Bees_acute
) %>%
  mutate(
    h_count = rowSums(
      across(everything(), ~ str_detect(.x, "H"))
    ),
    na_count = rowSums(!is.na(.))
  ) %>%
  arrange((h_count))

library(DT)
proc_export <- eco_risk_bin_tbl %>%
  DT::datatable(
    .,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20)
    )
  ) %>%
  formatStyle(names(eco_risk_bin_tbl), textAlign = "center") %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("XH", "darkred")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("VH", "#dc3545")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("H", "#fd7e14")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("M", "#ffc107")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("L", "#28a745")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("VL", "darkgreen")
  ) %>%
  formatStyle(
    names(eco_risk_bin_tbl),
    backgroundColor = styleEqual("ND", "grey")
  )

proc_export

# testing -----------------------------------------------------------------

eco_risk_tbl %>%
  select(
    eco_group,
    family,
    genus,
    species,
    exposure_group,
    life_stage,
    test_type,
    endpoint,
    new_dur,
    new_unit
  ) %>%
  filter(
    eco_group == 'Invertebrates'
    #family == 'Megachilidae',
    #exposure_group == 'ORAL' | is.na(exposure_group),
    #life_stage == 'Adult',
    #endpoint == '',
    #new_unit == 'ug/bee',
    #new_dur == 240
  ) %>%
  group_by(
    family,
    endpoint,
    test_type,
    exposure_group,
    life_stage,
    new_unit
  ) %>%
  summarize(
    n = n(),
    min = min(new_dur),
    mean = mean(new_dur),
    max = max(new_dur)
  ) %>%
  print(n = Inf)

eco_risk_tbl %>%
  distinct(new_unit) %>%
  pull(1) %>%
  sort()

tbl(eco_con, 'results') %>%
  distinct(effect) %>%
  collect() %>%
  mutate(effect = str_remove_all(effect, pattern = "\\/")) %>%
  distinct() %>%
  left_join(
    .,
    tbl(eco_con, 'effect_codes') %>% collect(),
    join_by(effect == code)
  ) %>%
  arrange(description) %>%
  print(n = Inf)
select(
  eco_group,
  family,
  genus,
  species,
  exposure_group,
  life_stage,
  test_type,
  endpoint,
  new_dur,
  new_unit
) %>%
  filter(
    eco_group == 'Invertebrates'
    #family == 'Megachilidae',
    #exposure_group == 'ORAL' | is.na(exposure_group),
    #life_stage == 'Adult',
    #endpoint == '',
    #new_unit == 'ug/bee',
    #new_dur == 240
  ) %>%
  group_by(
    family,
    endpoint,
    test_type,
    exposure_group,
    life_stage,
    new_unit
  ) %>%
  summarize(
    n = n(),
    min = min(new_dur),
    mean = mean(new_dur),
    max = max(new_dur)
  ) %>%
  print(n = Inf)

eco_risk_tbl %>%
  distinct(new_unit) %>%
  pull(1) %>%
  sort()

tbl(eco_con, 'results') %>%
  distinct(effect) %>%
  collect() %>%
  mutate(effect = str_remove_all(effect, pattern = "\\/")) %>%
  distinct() %>%
  left_join(
    .,
    tbl(eco_con, 'effect_codes') %>% collect(),
    join_by(effect == code)
  ) %>%
  arrange(description) %>%
  print(n = Inf)

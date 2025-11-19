library(ComptoxR)

all_lists <- ct_lists_all()

query_chems <- ct_list(
  list_name = c(
    'EPAHFR',
    'EPAHFRTABLE2',
    'FRACFOCUS',
    'CALWATERBDS',
    'PRODWATER'
  )
) %>%
  ct_details(query = .) %>%
  mutate(casrn = str_remove_all(casrn, "-"))

q1 <- post_results(casrn = query_chems$casrn)

q2 <- q1 %>%
  left_join(query_chems, ., join_by(casrn == test_cas)) %>%
  group_by(
    preferredName,
    casrn,
    endpoint,
    #test_type,
    eco_group,
    #harmonized_life_stage
  ) %>%
  reframe(
    result = min(new_value),
    unit = new_unit
  ) %>%
  distinct() %>%
  filter(!is.na(result) & endpoint == 'EC50')


df <- tibble::tribble(
  ~time_val , ~time_unit ,
         10 , "days"     ,
          2 , "weeks"    ,
         48 , "hours"    ,
       3600 , "seconds"
)


# Request ----------------------------------------------------------------

oppt <- post_results(
  #latin_name = 'Anastrepha obliqua',
  #eco_group = 'Fish',
  casrn = '50-00-0'
) %>%

  left_join(
    .,
    unit_conversion,
    join_by(conc1_unit == orig)
  ) %>%
  left_join(
    .,
    duration_conversion,
    join_by(obs_duration_unit == code)
  ) %>%
  left_join(
    .,
    effect_conversion,
    join_by(effect == term)
  ) %>%
  select(-effect_definition) %>%
  mutate(
    #endpoint = str_remove_all(endpoint, pattern = '~|/'),
    #coalesce(conc1_mean, conc1_min, conc1_max),
    result = case_when(
      is.na(conc1_mean) ~ geometric.mean(c(conc1_min, conc1_max), na.rm = TRUE),
      .default = conc1_mean,
    ),
    final_result = result * conversion_factor_unit,
    duration = coalesce(
      obs_duration_mean,
      obs_duration_min,
      obs_duration_max
    ) %>%
      as.numeric(),
    final_duration = duration * conversion_factor_duration
    #.keep = 'unused'
  ) %>%
  # ! Filter here --------
  filter(
    cur_unit_duration == 'h',
    ,
    final_duration >= 24 & final_duration <= 96,
    #	,!is.na(conc1_mean)
    effect_group == 'MOR'
  )

group_by(
  test_cas,

  effect,
  endpoint,
  cur_unit_result
) %>%
  summarize(
    cur_unit_type,
    unit_domain,
    final_duration,
  )

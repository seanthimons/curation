# Tables --------------------------------------------------------------------------------
{
  # Read the serialized dictionary from DuckDB
  serialized_test_type_dictionary <- tbl(eco_con, 'dict_test_result_duration') %>%
    collect()

  # Reform the table for R parsing
  reformed_test_type_rules <- serialized_test_type_dictionary %>%
    mutate(
      # Reconstruct list-columns: split by '|', convert "NA" to NA, and ensure it's a list-column
      exposure_group = map(
        exposure_group,
        function(.x) {
          if (is.na(.x) || .x == "") {
            NULL # Original was NULL
          } else {
            parts <- str_split(.x, fixed("|")) %>%
              pluck(1)
            na_if(parts, "NA")
          }
        }
      ),
      unit = map(
        unit,
        function(.x) {
          if (is.na(.x) || .x == "") {
            NULL
          } else {
            parts <- str_split(.x, fixed("|")) %>%
              pluck(1)
            na_if(parts, "NA")
          }
        }
      ),
      endpoint = map(
        endpoint,
        function(.x) {
          if (is.na(.x) || .x == "") {
            NULL
          } else {
            parts <- str_split(.x, fixed("|")) %>%
              pluck(1)
            na_if(parts, "NA")
          }
        }
      ),
      # Reconstruct expression-columns: parse string back to expression, handle NA
      effect = map(
        as.character(effect),
        function(.x) {
          if (is.na(.x) || .x == "") {
            NULL
          } else {
            rlang::parse_expr(.x)
          }
        }
      ),
      duration = map(
        as.character(duration),
        function(.x) {
          if (is.na(.x) || .x == "") {
            NULL
          } else {
            rlang::parse_expr(.x)
          }
        }
      )
    )
}

# Testing -------------------------------------------------------------------------------
library(ComptoxR)

query <- ct_list(c('PRODWATER', 'FRACFOCUS', 'EPAHFR', 'EPAHFRTABLE2', 'CALWATERBDS'))
ct_details(query = .)

query_cas <- ct_search(
  query = 'Spirodiclofen',
  search_method = 'equal',
  request_method = 'GET'
) %>%
  pull(casrn) %>%
  str_remove_all(., "-")

query_cas <- query %>%
  pull(casrn) %>%
  str_remove_all(., "-")

p1 <- post_results(
  casrn = query_cas
)

p1 <- post_results(
  #casrn = '50-00-0',
  eco_group = "Flowers, Trees, Shrubs, Ferns",
  endpoint = c('BCF', 'BAF')
)

p1 <- post_results(
	casrn = ComptoxR::testing_chemicals$
)

eco_risk_tbl %>%
  group_by(eco_group) %>%
  reframe(
    ex = unique(exposure_group)
  ) %>%
  print(n = Inf)


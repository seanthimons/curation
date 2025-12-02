# Tables --------------------------------------------------------------------------------
{
  # Read the serialized dictionary from DuckDB
  serialized_test_type_dictionary <- tbl(eco_con, 'dict_test_result_duration') %>%
    collect()

  # Reform the table for R parsing
  reformed_test_type_rules <- serialized_test_type_dictionary %>%
    mutate(across(
      .cols = c(
        eco_group,
        exposure_group,
        unit,
        endpoint,
        effect,
        duration
      ),
      .fns = ~ map2(.x, cur_column(), function(item, col_name) {
        # Use map2 to get column name
        if (is.na(item) || item == "") {
          return(NULL)
        }

        # Only parse as an R expression if it's the 'duration' column
        # AND it contains characters indicative of a complex R expression.
        # This prevents unit strings like "mg/kg/d" from being parsed as expressions.
        if (col_name == "duration") {
          if (rlang::is_expression(item)) {
            rlang::parse_expr(item)
          } else {
            # For all other cases, treat as a simple string or a pipe-separated list of strings.
            na_if(str_split(item, fixed("|"))[[1]], "NA")
          }
        } else {
          # For all other columns, treat as a simple string or a pipe-separated list of strings.
          na_if(str_split(item, fixed("|"))[[1]], "NA")
        }
      })
    ))
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
  casrn = '1190931-27-1',
  eco_group = "Flowers/Trees/Shrubs/Ferns",
	#endpoint = 'default'
  endpoint = c('BCF', 'BAF')
)

expo_dur <- tbl(eco_con, 'tests') %>%
  select(test_id, contains('study_duration_')) %>%
	head(n = 10) %>% 
	collect() %>% 
	#select(test_id, contains('application_')) %>% 
  glimpse()

expo_dur %>% 
	pivot_longer(
    cols = -c(test_id,study_duration_unit, study_duration_comments),
    names_to = c("study_calc_type", '.value'),
    names_pattern = "(_op)?",
		values_drop_na = TRUE
  )


tbl(eco_con, 'tests') %>% glimpse()

tbl(eco_con, 'results') %>% filter(result_id == '2784910') %>% glimpse()

tbl(eco_con, 'duration_conversion') %>%
	filter(base_unit == 'days') %>% 
	glimpse()

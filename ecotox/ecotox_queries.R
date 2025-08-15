# functions ---------------------------------------------------------------

convert_units <- function(data, value_column, unit_column) {
  data %>%
    mutate(
      new_value = case_when(
        #Need to find a way of grabbing active ingredient values...

        !!sym(unit_column) == "ug/L" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppb" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppm" ~ !!sym(value_column),
        !!sym(unit_column) %in% c("g/bee", "grams per bee") ~
          !!sym(value_column) * 1e6,
        !!sym(unit_column) %in% c("mg/bee", "milligrams per bee") ~
          !!sym(value_column) * 1000,
        !!sym(unit_column) %in% c("ug/bee", "micrograms per bee") ~
          !!sym(value_column),
        TRUE ~ !!sym(value_column)
      ),
      new_unit = case_when(
        !!sym(unit_column) == "ug/L" ~ "mg/L",
        !!sym(unit_column) == "ppb" ~ "mg/L",
        !!sym(unit_column) == "ppm" ~ "mg/L",
        !!sym(unit_column) %in%
          c(
            "g/bee",
            "grams per bee",
            "mg/bee",
            "milligrams per bee",
            "ug/bee",
            "micrograms per bee"
          ) ~
          "ug/bee",
        TRUE ~ !!sym(unit_column)
      )
    )
}

convert_duration <- function(data, value_column, unit_column) {
  data %>%
    mutate(
      new_dur = case_when(
        !!sym(unit_column) == "days" ~ !!sym(value_column) * 24,
        !!sym(unit_column) == "weeks" ~ !!sym(value_column) * 7 * 24,
        TRUE ~ !!sym(value_column)
      ),
      new_dur_unit = "hours"
    )
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

weighted_average <- function(values, weights) {
  sum(values * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

# Queries -----------------------------------------------------------------
#
# query <- ComptoxR::testing_chemicals %>%
#   pull(casrn) %>%
#   str_remove_all(., "-")
#
# query <- ct_search(query = 'Spirodiclofen', search_method = 'equal', request_method = 'GET') %>%
#   pull(casrn) %>%
#   str_remove_all(., "-")
#
# ggplot(q1) +
#  aes(
#    x = result,
#    y = forcats::fct_reorder(dtxsid, qrt),
#    colour = eco_group,
#    shape = endpoint_group
#    ) +
#  geom_point() +
#  scale_color_hue(direction = 1) +
#  scale_x_continuous(trans = "log10") +
#  theme_classic() + facet_grid('endpoint_group')

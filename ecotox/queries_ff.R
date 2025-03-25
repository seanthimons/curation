library(ECOTOXr)

# functions ---------------------------------------------------------------

convert_units <- function(data, value_column, unit_column) {
  data %>%
    mutate(
      new_value = case_when(
        !!sym(unit_column) == "ug/L" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppb" ~ !!sym(value_column) / 1000,
        !!sym(unit_column) == "ppm" ~ !!sym(value_column),
        TRUE ~ !!sym(value_column)
      ),
      new_unit = case_when(
        !!sym(unit_column) == "ug/L" ~ "mg/L",
        !!sym(unit_column) == "ppb" ~ "mg/L",
        !!sym(unit_column) == "ppm" ~ "mg/L",
        TRUE ~ !!sym(unit_column)
      )
    )
}

# cleaning ----------------------------------------------------------------


con <- dbConnectEcotox()

lot <- dbListTables(con) %>% 
  as.list() %>% 
  set_names(., dbListTables(con))

chems <- dbReadTable(con, 'chemicals')


#test <- dbReadTable(con, 'tests')
#test_cols <- dbReadTable(con, 'tests') %>% colnames()
#species <- dbReadTable(con, 'species')

# test_filt <- tbl(con, 'tests') %>% 
#   filter(test_cas == '91203') #%>%  collect() %>% colnames()
# 
# species_filt <- tbl(con, 'species') %>% 
#   filter(phylum_division == 'Arthropoda')

# q1 <- inner_join(test_filt, species_filt, join_by('species_number')) %>% 
#   collect()

query <- ComptoxR::testing_chemicals %>% select(dtxsid) %>% unlist()

query <- tbl(con, 'chemicals') %>% 
  filter(dtxsid %in% query) %>% 
  collect()

missing <- chems$dtxsid %>% str_subset(., pattern = query)

q1 <- tbl(con, "tests") %>%
  filter(test_cas %in% query$cas_number) %>%
  inner_join(
    tbl(con, "species") %>% filter(
      #   #phylum_division == 'Arthropoda',
      #   genus == 'Lepomis'
      #ecotox_group == 'Fish'
    )
    ,join_by('species_number')
  ) %>%
  inner_join(
    tbl(con, 'results'),
    join_by('test_id')
  ) %>% 
  filter(
    endpoint %in% c('EC50', 'LC50', 'LD50', 'LOEC', 'LOEL', 'NOEC', 'NOEL'),
    #effect %in% c('MOR'),
    conc1_unit %in% c('ug/L', 'mg/L', 'ppm', 'ppb')
  ) %>% 
  collect() %>% 
  mutate(
    result = as.numeric(conc1_mean),
    eco_group = case_when(
      str_detect(ecotox_group,'Insects/Spiders') ~ 'Insects/Spiders',
      str_detect(ecotox_group,'Flowers, Trees, Shrubs, Ferns') ~ 'Flowers, Trees, Shrubs, Ferns',
      str_detect(ecotox_group,'Fungi') ~ 'Fungi',
      str_detect(ecotox_group,'Algae') ~ 'Algae',
      str_detect(ecotox_group,'Fish') ~ 'Fish',
      str_detect(ecotox_group,'Crustaceans') ~ 'Crustaceans',
      str_detect(ecotox_group,'Invertebrates') ~ 'Invertebrates',
      str_detect(ecotox_group,'Worms') ~ 'Worms',
      str_detect(ecotox_group,'Molluscs') ~ 'Molluscs',
      str_detect(ecotox_group,'Birds') ~ 'Birds',
      str_detect(ecotox_group,'Mammals') ~ 'Mammals',
      str_detect(ecotox_group,'Amphibians') ~ 'Amphibians',
      str_detect(ecotox_group,'Reptiles') ~ 'Reptiles',
      str_detect(ecotox_group,'Moss, Hornworts') ~ 'Moss, Hornworts',
      #str_detect('') ~ '',
      .default = ecotox_group
    ),
    endpoint_group = case_when(
      str_detect(endpoint, 'LOEC|LOEL') ~ 'LOEC | LOEL',
      str_detect(endpoint, 'EC50|LD50|LC50') ~ 'EC50 | LD50 | LC50',
      str_detect(endpoint, 'NOEL|NOEC') ~ 'NOEL | NOEC'
    )
  ) %>% 
  convert_units(., value_column = 'result', unit_column = 'conc1_unit') %>%  
  group_by(
    test_cas,
    endpoint,
    eco_group,
    #class,
    #genus
  ) %>% 
  reframe(
    endpoint,
    endpoint_group,
    result,
    avg_result = mean(new_value),
    new_unit,
    eco_group,
    effect
    # unit = conc1_unit
  ) %>% 
  filter(!is.na(result)) %>%
  left_join(., tbl(con, 'chemicals') %>% collect(), join_by('test_cas' == 'cas_number')) %>% 
  left_join(., tbl(con, 'effect_codes') %>% collect(), join_by('effect' == 'code')) %>% 
  filter(description %in% c(
    'Mortality', 
    'Population', 
    'Growth',
    'Development',
    'Reproduction',
    'Intoxication'
  )) %>% 
  #filter(dtxsid != 'DTXSID6034479') %>% 
  distinct() %>% 
  group_by(dtxsid, eco_group, endpoint_group) %>% 
  mutate(qrt = quantile(result, 0.25)) %>% 
  arrange(qrt) %>% 
  ungroup()

q1 %>% 
  distinct(., dtxsid, qrt, .keep_all = TRUE) %>% 
  mutate(
    endpoint_group = factor(endpoint_group, levels = c(
      'LOEC | LOEL',
      'NOEL | NOEC', 
      'EC50 | LD50 | LC50'
  ))) %>% 
ggplot() +
  aes(
    x = qrt,
    y = forcats::fct_reorder(dtxsid, qrt),
    colour = eco_group,
    shape = endpoint_group
  ) +
  geom_point(
    size = 2.0
  ) +
  scale_color_hue(direction = 1) +
  
  xlab('Concentration (mg/L)') +
  ylab('Compound') +
  
  guides(
    colour = guide_legend(title = 'Ecological Group'), 
    shape = guide_legend(title = 'Endpoint Group')
  ) +
  
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    guide = "axis_logticks") +
  theme_bw() + 
  
  #facet_wrap('endpoint_group', axes = 'all_x')
  facet_wrap('description', axes = 'all_x')


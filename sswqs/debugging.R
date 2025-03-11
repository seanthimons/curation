crit_dat %>%
  count(unit) %>%
  arrange(desc(n)) %>%
  left_join(., parent_dat$unit, join_by(unit == idx)) %>% 
  print(n = Inf)

crit_dat %>% 
  rename(orig_result = result) %>% 
  filter(
    analyte %in% c('136', '162','737')
    #unit == '103'
    #,is.na(unit)
    ) %>% 
  inner_join(., parent_dat$pollutants, join_by(analyte == idx)) %>% print(n =Inf)
  inner_join(., wqs_pollutants, join_by(analyte == idx)) %>% 
  left_join(., parent_dat$units, join_by(unit == idx)) %>% 
  inner_join(., result_idx_cur, join_by(orig_result == raw_result)) %>% 
  left_join(., entities, join_by(area == short_code)) %>% 
  View()

crit_dat %>% 
  count(analyte, unit) %>% 
  arrange(
    analyte,
    desc(n)
    ) %>% 
  get_dupes(analyte) %>% 
  inner_join(., wqs_pollutants, join_by(analyte == idx)) %>% 
  left_join(., parent_dat$units, join_by(unit == idx)) %>% 
  #head(n = 10)
  filter(!is.na(v), !is.na(unit)) %>% 
  print(n = Inf) %>% 
  View()


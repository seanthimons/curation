q1 <- crit_dat %>%
  inner_join(., wqs_pollutants, join_by(result == idx)) %>% 
  count(unit) %>%
  arrange(desc(n)) %>%
  left_join(., parent_dat$unit, join_by(unit == idx)) %>% 
  filter(!is.na(unit)) %>% 
  print(n = Inf)

q2 <- crit_dat %>% 
  count(analyte, unit) %>% 
  arrange(
    analyte,
    desc(n)
  ) %>% 
  get_dupes(analyte) %>% 
  inner_join(., wqs_pollutants, join_by(analyte == idx)) %>% 
  left_join(., parent_dat$units, join_by(unit == idx)) %>% 
  #head(n = 10)
  filter(!is.na(v), !is.na(unit)) 
  #filter(str_detect(unit.y, pattern = 'no'), str_detect(v, pattern = 'DTX', negate = T))

q3 <- crit_dat %>% 
  rename(orig_result = result) %>% 
  inner_join(., result_idx_cur, join_by(orig_result == raw_result)) %>% 
  filter(
    
    #analyte %in% q2$analyte,
    #unit %in% q2$unit
    #unit == '121'
    #,is.na(unit)
    
  ) %>% 
  inner_join(., parent_dat$pollutants, join_by(analyte == idx)) %>%
  inner_join(., wqs_pollutants, join_by(analyte == idx)) %>% 
  inner_join(., parent_dat$units, join_by(unit == idx)) %>% 
  left_join(., entities, join_by(area == short_code))

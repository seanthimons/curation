q1 <- crit_dat %>%
  rename(orig_result = result) %>%
  left_join(., wqs_pollutants, join_by(analyte == idx)) %>%
  inner_join(., result_idx_cur, join_by(orig_result == raw_result)) %>%
  count(unit) %>%
  arrange(desc(n)) %>%
  left_join(., parent_dat$unit, join_by(unit == idx)) %>%
  filter(!is.na(unit)) %>%
  print(n = Inf)

q2 <- crit_dat %>%
  rename(orig_result = result) %>%
  left_join(., wqs_pollutants, join_by(analyte == idx)) %>%
  inner_join(., result_idx_cur, join_by(orig_result == raw_result)) %>%
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
  filter(
    #str_detect(unit.y, pattern = 'no'),
    str_detect(v, pattern = 'DTX', negate = F)
  )

#DEBUG
# distinct(v) %>%
# unlist() %>%
# unname() %>%
# write_lines(., file = 'temp.txt')

q3 <- crit_dat %>%
  rename(orig_result = result) %>%
  filter(
    #analyte %in% q2$analyte,
    #unit %in% q2$unit
    analyte == '263'
    #unit == '122'
    #,is.na(unit)
  ) %>%
  inner_join(., result_idx_cur, join_by(orig_result == raw_result)) %>%
  inner_join(., wqs_pollutants, join_by(analyte == idx)) %>%

  inner_join(., parent_dat$units, join_by(unit == idx)) %>%
  left_join(., entities, join_by(area == short_code))


sswqs %>%
  filter(
    #is.na(local),
    #str_detect(name, 'California'),
    #analyte == '',
    dtxsid %in%
      c(
        'DTXSID2023981',
        'DTXSID7023982',
        'DTXSID3031022',
        'DTXSID70162189'
      ),
    #application == 'human health',
    general_usage == 'Water Supply'
  ) %>%
  select(
    name,
    region,
    # general_usage,
    local,
    application,
    #location,
    #exposure,
    #subtype,
    #analyte,
    result
  ) %>%
  mutate(result = as.numeric(result)) %>%
  filter(!is.na(result)) %>%
  arrange(region, result) %>%
  distinct(region, .keep_all = TRUE) %>%
  print(n = Inf) %>%
  glimpse()

classes <- sswqs %>%
  distinct(analyte, cas, dtxsid) %>%
  filter(!is.na(dtxsid)) %>%
  distinct(dtxsid) %>%
  pull(dtxsid) %>%
  chemi_classyfire(query = .)

classes %>%
  filter(!is.na(klass)) %>%
  summarize(
    kd = unique(klass) %>% length(),
    skd = unique(subklass) %>% length(),
    supd = unique(superklass) %>% length()
  )

func_usage <- sswqs %>%
  distinct(analyte, cas, dtxsid) %>%
  filter(!is.na(dtxsid)) %>%
  distinct(dtxsid) %>%
  pull(dtxsid) %>%
  ct_functional_use(.)

sswqs %>%
  filter(
    #is.na(local),
    str_detect(name, 'Puerto Rico'),
    #analyte == '',
    dtxsid %in% c(),
    #application == 'human health',
    general_usage == 'Water Supply'
  ) %>%
  select(
    name,
    region,
    general_usage,
    local,
    application,
    location,
    exposure,
    subtype,
    analyte,
    result,
    unit
  ) %>%
  mutate(result = as.numeric(result)) %>%
  filter(!is.na(result)) %>%
  print(n = Inf) %>%
  glimpse()

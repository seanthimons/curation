#OLD ---------------------------------------------------------------------


  temp <- wqs_temp %>%
    #degree
    mutate(unit_name = str_remove_all(unit_name, pattern = '\\u00b0')) %>% #Removes µ
    mutate(unit_name = str_replace_all(unit_name, pattern = '\\u00b5', replacement = 'u')) %>%
    mutate(original_unit = unit_name) %>%
    ungroup()

  #Diagnostic
  w_units <- temp %>%
    count(unit_name) %>%
    arrange(desc(n))
  rm(w_units)

  unit_dict <- temp %>%
    distinct(unit_name, .keep_all = T) %>%
    select(unit_name, original_unit) %>%
    mutate(conversion = NA_real_) %>%
    relocate(original_unit, .before = unit_name)

  rio::export(unit_dict, file = 'units_dict_dump.xlsx')

  #Unit cleaning----

  unit_dict <- rio::import(file = 'sswqs_units_curated.xlsx')

  temp1 <- temp %>%
    select(-unit_name) %>%
    left_join(., unit_dict, join_by('original_unit')) %>%
    filter(unit_name != 'REMOVE' ) %>%
    filter(is.na(preferredName) | preferredName != "temperature") %>% #issues with records that differentials records
    rowwise() %>%
    mutate(needs_convert = !identical(unit_name, original_unit),
           orig_parsed_value = criterion_value,
           orig_range_l = range_l,
           orig_range_u = range_u) %>%
    ungroup() %>%
    mutate(
      across(
        c(criterion_value, range_l, range_u), ~case_when(
          needs_convert == TRUE ~ .*conversion
          ,.default = .)
      ))


  # missing protection data -------------------------------------------------

  temp2 <- temp1 %>%
    mutate(
      meta = NA_character_,
      cit = 'State-Specific Water Quality Standards',
      protection = case_when(
        criteriatypeaquahumhlth == 'A' ~ 'Aquatic',
        criteriatypeaquahumhlth == 'H' ~ 'Human',
        criteriatypeaquahumhlth == 'O' ~ 'Organoleptic'
        ,.default = criteriatypeaquahumhlth),
      sourcewater = case_when(
        criteriatypefreshsaltwater == 'S' ~ 'Salt Water',
        criteriatypefreshsaltwater == 'B' ~ 'Brackish',
        criteriatypefreshsaltwater == 'F' ~ 'Fresh Water',
        criteriatypefreshsaltwater == 'SW' ~ 'Storm Water',
        criteriatypefreshsaltwater == 'I' ~ 'Industry Process Water',
        criteriatypefreshsaltwater == 'MN' ~ 'Treated Municipal Wastewater',
        criteriatypefreshsaltwater == 'CW'~ 'Onsite Collected Waters'
        ,.default = criteriatypefreshsaltwater),
      duration = case_when(
        criteriatype_acutechronic == 'A' ~ 'Acute',
        criteriatype_acutechronic == 'C' ~ 'Chronic',
        criteriatype_acutechronic == 'S' ~ 'Sample',
        criteriatype_acutechronic == 'D' ~ 'Daily',
        criteriatype_acutechronic == 'W' ~ 'Weekly',
        criteriatype_acutechronic == 'M' ~ 'Monthly',
        criteriatype_acutechronic == 'Y' ~ 'Yearly'
        ,.default = criteriatype_acutechronic),
      #NOTE Update to endpoint + combine wuth protection?
      enduse = case_when(
        criteriatype_waterorg == 'O' ~ 'Organism',
        criteriatype_waterorg == 'W' ~ 'Water & Organism',
        criteriatype_waterorg == 'A' ~ 'Agriculture',
        criteriatype_waterorg == 'L' ~ 'Landscaping',
        criteriatype_waterorg == 'NPR' ~ 'Centralized Non-Potable Reuse',
        criteriatype_waterorg == 'PWR' ~ 'Potable Water Reuse',
        criteriatype_waterorg == 'I' ~ 'Impoundments',
        criteriatype_waterorg == 'ER' ~ 'Environmental Restoration',
        criteriatype_waterorg == 'IND' ~ 'Industry',
        criteriatype_waterorg == 'NPWR' ~ 'Onsite Non-Potable Water Reuse',
        criteriatype_waterorg == 'LIV' ~ 'Livestock'
        ,.default = criteriatype_waterorg)) %>%
    rename(
      local = use_class_name_location_etc
    ) %>%
    ungroup() %>%
    select(-c(criteriatypeaquahumhlth:criteriatype_waterorg)) %>%
    select(criterion_id:entity_name, final:preferredName, criterion_value, unit_name, IS_RANGE, range_l, range_u, local, meta:enduse) %>%
    mutate(across(protection:enduse, ~replace_na(., 'UNC')))

  dtx <- temp2 %>%
    filter(final != 'bulk') %>%
    distinct(final, .keep_all = F)

  dtx <- ct_details(query = dtx$final, projection = 'id') %>%
    select(dtxsid, preferredName)

  temp3 <- temp2 %>%
    split(is.na(.$preferredName)) %>%
    map(., ~rename(., dtxsid = final))

  temp3$`TRUE` <- temp3$`TRUE` %>%
    select(-preferredName) %>%
    inner_join(., dtx, join_by(dtxsid))

  temp3 <- list_rbind(temp3)

  entity_list <- temp3 %>%
    distinct(., entity_name, .keep_all = F) %>%
    mutate(
      source = 'State-Specific Water Quality Standards',
      subsource = NA_character_,
      origin_category = case_when(
        entity_name %in% c(
          state.name, #from dataset package
          'California - Statewide',
          'California Region',
          'CTR - California Toxics Rule') ~ 'State',
        str_detect(entity_name, 'California Region') ~ 'State',
        entity_name %in% c(
          #NOTE Adjust this to mirror other data sets
          'EPA 304(a) Recommended Criteria',
          'NTR - National Toxics Rule') ~ 'Federal',
        .default = 'Other'),
      origin_supercategory = NA_character_,
      origin_agency = NA_character_,
      data_category = 'Primary',
      priority_id = case_when(
        origin_category == 'State' ~ 1,
        origin_category == 'Federal' ~ 1,
        .default = 3 #For tribes and others
      )
    )

  temp4 <- temp3 %>%
    left_join(., entity_list, join_by('entity_name')) %>%
    rename(is_range = IS_RANGE) %>%
    relocate(protection:enduse, .after = range_u)

  # TODO Change to RDS format
  # rio::export(temp4, file = paste0('sswqs_curated_', Sys.Date(), '.xlsx'))
  #
  # rm(list = ls())
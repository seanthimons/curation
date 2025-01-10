{
library(here)
library(cli)
library(janitor)
library(rvest)
library(httr)
library(tidyverse)
library(rio)
library(ComptoxR)
  
setwd(here('rais'))
}

# Established Regulatory Limits for Surface Water and Groundwater ---------
## ARARs; Applicable or Relevant and Appropriate Requirements ----

{
  
  #list options
  
  webpage <- read_html("https://rais.ornl.gov/tools/arar_search.php")
  
  # #gets the url
  # html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>% 
  #   html_attrs()
  # 
  # #gets the displayed value
  # html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>% 
  #   html_text()
  
  choices <- tibble(
    state = html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>% 
      html_text(), 
    url = html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>% 
      html_attrs() %>% unlist() %>% unname()) %>% 
    mutate(
      abv = str_remove_all(url, '&action=next'),
      abv = str_remove_all(abv, '.*='),
      caps = str_to_upper(abv)
      
    ) %>% 
    .[-1,] #removes empty choice by position
  
  rm(webpage)
  
  {
  cli::cli_alert_info('Begining ARAS search...')
  cli::cat_line()
  }
  
  arars <- pmap(choices, function(state, url, abv, caps){
    
    #  })
    # ({
    #   choices %>% map(., 14) %>% list2env(., .GlobalEnv)
    # 
    cli::cli_alert(state)
    cli::cat_line()
    
    webpage <- read_html(paste0("https://rais.ornl.gov", url))
    
    fields <- tibble(
      name = html_elements(webpage, xpath = '//*[@id="dualselect"]/div/fieldset') %>%
        html_attrs() %>%
        unlist() %>%
        unname(),
      value = html_elements(webpage, xpath = '//*[@id="dualselect"]/div/fieldset/legend') %>%
        html_text()
    ) %>%
      mutate(
        value = str_remove_all(value, pattern = "Select ") %>%
          str_remove_all(., pattern = '/|,|\\(|\\)') %>% 
          str_replace_all(., pattern = " ", replacement = "_")
        
      ) %>%
      head(., -1)
    
    field_list <- map(fields$value, function(value) {
      
      #value <- fields$value[2]
      opt <- paste0('//*[@id="arars', value, '-0"]/option')
      
      sam_pay <- list("Yes")
      sam_pay <- set_names(sam_pay, paste0("sam_", value))
      
      df <- list(
        name = html_elements(webpage, xpath = opt) %>%
          html_text(),
        val = html_elements(webpage, xpath = opt) %>%
          html_attrs() %>%
          unlist() %>%
          unname(),
        p_list = NA
      )
      
      dim <- length(df$val)
      
      df$p_list <- rep(paste0("arars", value, "[]"), length(df$val))
      
      df$p_list <- setNames(df$val, df$p_list) %>% as.list()
      
      df$p_list <- c(df$p_list, sam_pay)
      
      df$val <- NULL
      
      return(df)
    }) %>% 
      set_names(., fields$value)
    
    group_list <- map(field_list, ~pluck(., 'name') %>% as_tibble) %>%
      list_rbind(names_to = 'group') %>% 
      mutate(group = str_replace_all(group, pattern = '_', replacement = ' '))
    
    table_col_names <- map(field_list, ~pluck(., 'name')) %>%
      unlist() %>% 
      unname() %>% 
      append(., c('Analyte', 'CAS Number'), after = 0L)
    
    {
      chems <- tibble(
        name = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
          html_text(),
        value = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
          html_attrs() %>% unlist() %>% unname()
      )
      chems <- chems$value %>%
        map(., ~ list(
          "analysis[]" = .x
        )) %>%
        flatten()
      
      p1 <- list(
        "_qf__dualselect" = "",
        "action" = "Retrieve",
        "table_name" = "",
        "state" = abv
      )
      
      p2 <- map(field_list, ~ pluck(., "p_list")) %>% flatten()
      
      p3 <- list(
        "sam_Chem" = "Yes",
        "testSubmit" = "Retrieve"
      )
      
      payload <- c(p1, p2, chems, p3)
      
      rm(p1, p2, chems, p3, fields)
      }    
    
    req <- POST(
      url = "https://rais.ornl.gov/tools/arar_search.php",
      body = payload
      #,progress()
    )
    
    response <-
      content(req, as = "parsed")
    
    table_headers <- html_elements(response, xpath = '//*[@id="arars"]/thead/tr/th') %>%
      html_text2() %>%
      str_split(., pattern = "\t") %>%
      str_split(., pattern = "\n") %>%
      map(., ~ str_remove_all(., ",|[[:digit:]]")) %>%
      map(., ~ str_remove_all(., "NA|NANA"))
    
    units_list <- "ug/l|ug/L|pCi/L|mg/l|mg/L|mg/kg"
    
    table_ft <- html_elements(response, xpath = '//*[@id="arars"]/thead/tr/th') %>%
      html_text2() %>%
      str_split(., pattern = "\t") %>%
      str_split(., pattern = "\n") %>%
      map(., ~ str_remove_all(., "NA|NANA")) %>%
      map(., ~ str_remove_all(., units_list))
    
    tb_ft <- tibble(
      col = map(table_ft, ~ pluck(., 1)) %>% list_simplify(),
      ft = map(table_ft, ~ pluck(., 2, .default = NA)) %>% list_simplify()
    ) %>%
      separate_longer_delim(., ft, delim = ",") %>%
      mutate(
        #TODO follow up on where coercing to NA is happening
        ft = as.numeric(ft),
        ft = as.character(ft)
      ) %>%
      filter(!is.na(ft)) %>%
      filter(ft != "")
    
    rm(table_ft)
    
    table_units <- tibble(
      col = map(table_headers, ~ pluck(., 1)) %>%
        list_simplify(),
      units = map(table_headers, ~ pluck(., 2, .default = NA)) %>%
        list_simplify() %>%
        str_extract_all(., pattern = units_list)
    )
    
    table_units$col <- table_col_names
    
    rm(table_headers)
    
    dat_table <- html_elements(response, xpath = '//*[@id="arars"]') %>%
      html_table(., trim = T, na.strings = "") %>%
      pluck(., 1) %>% 
      set_names(., table_units$col) %>% 
      as_tibble(., .name_repair = "unique") %>% 
      mutate(across(everything(), as.character)) %>%
      pivot_longer(., cols = !matches("Analyte|CAS Number"), names_to = "source", values_to = "result", values_drop_na = T) %>%
      mutate(
        orig_result = result,
        ft = case_when(
          source != "Federal Water Quality Criteria Source" ~ str_extract(result, pattern = "(?<=\\s)\\S+"),
          .default = NA
        ),
        result = case_when(
          source != "Federal Water Quality Criteria Source" ~ str_extract(result, pattern = "^\\S+")
        )
      ) %>%
      separate_longer_delim(., ft, delim = ",")
    
    ft_page <- paste0("https://rais.ornl.gov/tools/includes/guide_", caps, "foot.html")
    
    ft_text <- read_html(ft_page) %>%
      html_elements(., xpath = '//*[@id="page-wrapper"]/section[2]/div/div/p') %>%
      html_text() %>%
      str_replace_all(., pattern = "\\r\\n", " ") %>%
      as_tibble() %>%
      mutate(
        ft = str_extract(value, pattern = "\\d{1,3}\\.") %>% str_remove_all(., pattern = "\\."),
        value = str_extract(value, pattern = "(?<=\\d{1,3}\\.).*") %>% str_squish()
      )
    
    header_footnotes <- left_join(tb_ft, ft_text, join_by(ft)) %>%
      select(-ft) %>%
      distinct() %>%
      group_by(col) %>%
      mutate(ft_num = paste0("ft", row_number())) %>%
      ungroup() %>%
      pivot_wider(., id_cols = c(col), values_from = "value", names_from = "ft_num") %>%
      unite(., "header_ft", contains("ft"), sep = " ", remove = T, na.rm = T)
    
    dat <- left_join(dat_table, header_footnotes, join_by(source == col)) %>%
      left_join(., table_units, join_by(source == col)) %>%
      #TODO follow up on where many-to-many relationships are happening; row 83 x-to-y, row 25 y-to-x
      left_join(., ft_text, join_by(ft)) %>%
      left_join(., group_list, join_by(source == value)) %>% 
      filter(!is.na(result)) %>%
      select(-ft) %>%
      clean_names() %>%
      unite(., "ft", c(header_ft, value), na.rm = T, remove = T, sep = " ") %>%
      rowwise() %>%
      mutate(units = paste(units)) %>%
      ungroup()
    
    return(dat)
  }) %>% 
    set_names(., choices$state) %>% 
    list_rbind(., names_to = 'state') %>% 
    rename(
      preferredName = analyte,
      cit = source,
      unit_name = units,
      criterion_value = result,
      meta = ft)

  write_rds(arars, 'rais_eco_arars.RDS')
  rm(list = ls())
}


# Ecological Benchmark Tool for Chemicals ---------------------------------

{
  # Read the HTML content of the website
  webpage <- read_html("https://rais.ornl.gov/tools/eco_search.php?select=chem")
  
  chems <- list(
    # val = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
    #   html_text(),
    dat = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>% 
      html_attrs() %>% unlist() %>% unname()
  ) %>% flatten()
  
  sources <- list(
    val = html_elements(webpage, xpath = '//*[@id="sources-0"]/option') %>% 
      html_text()
  ) %>% flatten()
  
  media <- list(
    val = html_elements(webpage, xpath = '//*[@id="media-0"]/option') %>% 
      html_text()
  ) %>% flatten()
  
  rm(webpage)
  
  {
    p1 <- list(
      '_qf__dualselect'= "",
      'select'= 'chem',
      'tname'= 'ECO_BENCH_LONG',
      'action'= 'Retrieve')
    
    media_payload <- 
      map(media, ~list(
        'media[]' = .x
      )) %>% flatten() %>% 
      append(., c('sam'= 'Yes'))
    
    sources_payload <- 
      map(sources, ~list(
        'sources[]'= .x
      )) %>% flatten() %>% 
      append(., c('sabs'= 'Yes'))
    
    payload <- c(p1, sources_payload, media_payload)
    
  }
  
  chems_payload <- map(chems, ~list(
    'analysis[]'= .x
  )) %>%
    flatten() %>%
    split(., rep(1:ceiling(length(.)/200), each = 200, length.out = length(.))) %>% 
    map(., ~append(., c('Submit' = 'Yes')))
  
  {
    cli::cli_alert_info('Ecological Benchmark Tool for Chemicals')
    cli::cat_line()
  }

  rais_tbl <- map(chems_payload, ~{
    
    payload_map <- append(payload, .x) %>% flatten()
    
    req <- POST(
      url = 'https://rais.ornl.gov/tools/eco_search.php',
      body = payload_map,
      progress() # progress bar
    )
    
    response <-
      content(req, as = 'parsed')
    
    response_ft <<- response
    
    tables <- map(media, ~{
      
      .x <- str_remove_all(.x, pattern = ' ')    
      #message(.x)
      
      opt = paste0('//*[@id="',.x,'"]')
      
      html_elements(response, xpath = opt) %>% 
        html_table() %>%
        pluck(., 1, .default = tibble()) %>% 
        mutate(across(everything(), as.character))
      
    }, .progress = T) %>%
      set_names(., media) %>%
      compact() %>% 
      list_rbind(., names_to = 'media')
    
    return(tables)
  }) %>%
    list_rbind() %>% 
    clean_names()
  
  rais_ft <- tibble(
    val = html_elements(response_ft, xpath = '//*[@id="page-wrapper"]/section[2]/div/ol/li') %>% 
      html_text()) %>% 
    mutate(num = 1:n() %>% as.character)
  
  rm(response_ft)
  
  eco_bench <- rais_tbl %>%
    mutate(orig_result = value_footnotes,
           result = value_footnotes %>%
             str_remove_all(., pattern = '\\s*\\([^\\)]+\\)') %>%
             as.numeric,
           ft_num =
             str_extract(value_footnotes, "\\(([^)]+)\\)") %>% str_remove_all(., "\\(|\\)| "),
           idx = 1:n()
    ) %>% 
    separate_longer_delim(., cols = ft_num, delim = ',') %>% 
    left_join(., rais_ft, join_by(ft_num == num)) %>% 
    rename(., ft = val) %>% 
    select(-c(value_footnotes, ft_num)) %>%
    group_by(idx) %>%
    mutate(ft_num = paste0("ft", row_number())) %>%
    ungroup() %>% 
    pivot_wider(., values_from = "ft", names_from = "ft_num") %>% 
    unite(., "ft", contains("ft"), sep = " ", remove = T, na.rm = T) %>% 
    select(-idx) %>% 
    rename(
      preferredName = analyte,
      cit = benchmark,
      local = organism, 
      unit_name = units,
      criterion_value = result,
      meta = ft)
  
  write_rds(eco_bench, 'rais_benchmarks_chemicals.RDS')

  rm(list = ls()[str_detect(ls(), pattern = 'eco_bench', negate = T)])

}

# Ecological Benchmark Tool for Radionuclides -----------------------------

{
  
  # Read the HTML content of the website
  webpage <- read_html("https://rais.ornl.gov/tools/eco_search.php?select=rad")
  
  chems <- list(
    # val = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
    #   html_text(),
    dat = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>% 
      html_attrs() %>% unlist() %>% unname()
  ) %>% flatten()
  
  sources <- list(
    val = html_elements(webpage, xpath = '//*[@id="sources-0"]/option') %>% 
      html_text()
  ) %>% flatten()
  
  media <- list(
    val = html_elements(webpage, xpath = '//*[@id="media-0"]/option') %>% 
      html_text()
  ) %>% flatten()
  
  rm(webpage)
  
  {
    p1 <- list(
      '_qf__dualselect'= "",
      'select'= 'rad',
      'tname'= 'ECO_BENCH_LONG_RAD',
      'action'= 'Retrieve')
    
    media_payload <- 
      map(media, ~list(
        'media[]' = .x
      )) %>% flatten() %>% 
      append(., c('sam'= 'Yes'))
    
    sources_payload <- 
      map(sources, ~list(
        'sources[]'= .x
      )) %>% flatten() %>% 
      append(., c('sabs'= 'Yes'))
    
    payload <- c(p1, sources_payload, media_payload)
    
  }
  
  chems_payload <- map(chems, ~list(
    'analysis[]'= .x
  )) %>%
    flatten() %>%
    split(., rep(1:ceiling(length(.)/200), each = 200, length.out = length(.))) %>% 
    map(., ~append(., c('Submit' = 'Yes')))
  
  {
    cli::cli_alert_info('Ecological Benchmark Tool for Radionuclides')
    cli::cat_line()
  }
  
  rais_tbl <- map(chems_payload, ~{
    
    payload_map <- append(payload, .x) %>% flatten()
    
    req <- POST(
      url = 'https://rais.ornl.gov/tools/eco_search.php',
      body = payload_map,
      progress() # progress bar
    )
    
    response <-
      content(req, as = 'parsed')
    
    response_ft <<- response
    
    tables <- map(media, ~{
      
      .x <- str_remove_all(.x, pattern = ' ')    
      #message(.x)
      
      opt = paste0('//*[@id="',.x,'"]')
      
      html_elements(response, xpath = opt) %>% 
        html_table() %>%
        pluck(., 1, .default = tibble()) %>% 
        mutate(across(everything(), as.character))
      
    }, .progress = T) %>%
      set_names(., media) %>%
      compact() %>% 
      list_rbind(., names_to = 'media')
    
    return(tables)
  }) %>%
    list_rbind() %>% 
    clean_names()
  
  rais_ft <- tibble(
    val = html_elements(response_ft, xpath = '//*[@id="page-wrapper"]/section[2]/div/ol/li') %>% 
      html_text()) %>% 
    mutate(num = 1:n() %>% as.character)
  
  rm(response_ft)
  
  rads_bench <- rais_tbl %>%
    mutate(orig_result = value_footnotes,
           result = value_footnotes %>%
             str_remove_all(., pattern = '\\s*\\([^\\)]+\\)') %>%
             as.numeric,
           ft_num =
             str_extract(value_footnotes, "\\(([^)]+)\\)") %>% str_remove_all(., "\\(|\\)| "),
           idx = 1:n()
    ) %>% 
    separate_longer_delim(., cols = ft_num, delim = ',') %>% 
    left_join(., rais_ft, join_by(ft_num == num)) %>% 
    rename(., ft = val) %>% 
    select(-c(value_footnotes, ft_num)) %>%
    group_by(idx) %>%
    mutate(ft_num = paste0("ft", row_number())) %>%
    ungroup() %>% 
    pivot_wider(., values_from = "ft", names_from = "ft_num") %>% 
    unite(., "ft", contains("ft"), sep = " ", remove = T, na.rm = T) %>% 
    select(-idx) %>% 
    rename(
      preferredName = radionuclide,
      cit = benchmark,
      local = organism, 
      unit_name = units,
      criterion_value = result,
      meta = ft)
  
  write_rds(rads_bench, 'rais_benchmarks_rads.RDS')
  
  rm(list = ls()[str_detect(ls(), pattern = 'rads_bench|eco_bench', negate = T)])
}


# Toxicity Values and Physical Parameters ---------------------------------
{
  ## Chemical Specific Parameters --------------------------------------------
  {
  webpage <- read_html("https://rais.ornl.gov/cgi-bin/tools/TOX_search?select=chemtox")
  
  chems <- list(
    dat = html_elements(webpage, xpath = '//*[@id="analysis_all"]/option') %>% 
      html_attrs() %>% unlist() %>% unname()
  ) %>% flatten()
  
  endpoints <- list(
    val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
      html_text(., trim = T)
  ) %>% flatten()
  
  short_code <- map(endpoints, ~{
    str_extract(.x, "\\(([^)]*)\\)") %>%
      str_remove_all("[()]") %>%
      str_trim()
  }) %>% unlist()
  
  c_names <- map(endpoints, ~{
    str_split_fixed(.x, pattern = "\\(", n = 2) %>%
      .[, 1] %>%
      str_trim()
  }) %>% 
    unlist()
  
  units <- map(endpoints, ~{
    str_extract(.x, "\\([^()]*\\)(?=[^()]*$)") %>%
      str_remove_all("[()]") %>%
      str_trim()
  }) %>% unlist()
  
  options <- list(
    val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>% 
      html_attrs() %>% unlist() %>% unname()
  ) %>% flatten()
  
  unit_table <- tibble(name = c_names, units = units)
  
  rm(webpage, endpoints)

  {
    p1 <- list(
      "_submitted_form" = 1,
      "action" = "next",
      'select' = 'chemtox',
      'selectall' = 'no',
      'table' = 'raistox'
      )
    
    ops <- map(options, ~list(
      'fields' = .x
    )) %>%
      flatten()
    
    p2 <- list(
      'alloptions' = 'all',
      '_submit' = 'Retrieve'
    )
  
  chems_payload <- map(chems, ~list(
    'analysis'= .x
  )) %>%
    flatten() %>%
    split(., rep(1:ceiling(length(.)/200), each = 200, length.out = length(.)))
  }
  
  {
    cli::cli_alert_info('Toxicity Values and Physical Parameters for Chemicals')
    cli::cat_line()
  }
  
  
  chemtox <- imap(chems_payload, ~{
    
    cli_alert_info(.y)
    
    payload_map <- c(p1, .x, ops, p2) %>% flatten()
    
    req <- POST(
      url = 'https://rais.ornl.gov/cgi-bin/tools/TOX_search',
      body = payload_map,
      progress() # progress bar
    )

    response <-
      content(req, as = 'parsed')
    
    table <- response %>% 
      html_table() %>% 
      pluck(., 2) %>% 
      mutate(idx = 1:n(), .before = 'Chemical')
    
    dat <- table %>% 
      select(1:3, seq(4, ncol(table), by = 2)) %>% 
      set_names(., c('idx', 'compound', 'casrn', c_names)) %>% 
      pivot_longer(., cols = !c(1:3), values_drop_na = T)
  
    refs <- table %>% 
      select(1:3, seq(5, ncol(table), by = 2)) %>% 
      set_names(., c('idx', 'compound', 'casrn', c_names)) %>% 
      mutate(
        across(!c(idx:casrn), as.character),
        across(!c(idx:casrn), ~na_if(., ""))) %>% 
      pivot_longer(., cols = !c(1:3), values_to = 'ref',values_drop_na = T)
    
    df <- left_join(dat, refs, join_by(idx, compound, casrn, name)) %>% 
      left_join(unit_table, join_by('name'))
    
    return(df)
  }) %>% list_rbind()
  
  write_rds(chemtox, 'rais_chemtox.RDS')
  
  rm(list = ls())
  }
  
  ## Radionuclide Specific Parameters ----------------------------------------
  {
    webpage <- read_html("https://rais.ornl.gov/cgi-bin/tools/TOX_search?select=radspef")
    
    chems <- list(
      dat = html_elements(webpage, xpath = '//*[@id="analysis_all"]/option') %>% 
        html_attrs() %>% unlist() %>% unname()
    ) %>% flatten()
    
    endpoints <- list(
      val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
        html_text(., trim = T)
    ) %>% flatten()
    
    short_code <- map(endpoints, ~{
      str_extract(.x, "\\(([^)]*)\\)") %>%
        str_remove_all("[()]") %>%
        str_trim()
    }) %>% unlist()
    
    c_names <- map(endpoints, ~{
      str_split_fixed(.x, pattern = "\\(", n = 2) %>%
        .[, 1] %>%
        str_trim()
    }) %>% 
      unlist()
    
    units <- map(endpoints, ~{
      str_extract(.x, "\\([^()]*\\)(?=[^()]*$)") %>%
        str_remove_all("[()]") %>%
        str_trim()
    }) %>% unlist()
    
    options <- list(
      val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>% 
        html_attrs() %>% unlist() %>% unname()
    ) %>% flatten()
    
    unit_table <- tibble(name = c_names, units = units) %>% 
      filter(name %in% c('ARAR', 'Inhalation Exposure Factor', 'Maximum Contamination Limit', 'Specific Activity'))
    
    rm(webpage, endpoints)
    
    {
      p1 <- list(
        "_submitted_form" = 1,
        "action" = "next",
        'select' = 'radspef',
        'selectall' = 'no',
        'table' = 'radparams'
      )
      
      ops <- map(options, ~list(
        'fields' = .x
      )) %>%
        flatten()
      
      p2 <- list(
        'alloptions' = 'all',
        '_submit' = 'Retrieve'
      )
      
      chems_payload <- map(chems, ~list(
        'analysis'= .x
      )) %>%
        flatten() %>%
        split(., rep(1:ceiling(length(.)/200), each = 200, length.out = length(.)))
    }
    
    {
      cli::cli_alert_info('Toxicity Values and Physical Parameters for Radionuclides')
      cli::cat_line()
    }
    
    radtox <- imap(chems_payload, ~{
      
      cli_alert_info(.y)
      
      payload_map <- c(p1, .x, ops, p2) %>% flatten()
      
      req <- POST(
        url = 'https://rais.ornl.gov/cgi-bin/tools/TOX_search',
        body = payload_map,
        progress() # progress bar
      )
      
      response <-
        content(req, as = 'parsed')
      
      table <- response %>% 
        html_table() %>% 
        pluck(., 2) %>% 
        mutate(idx = 1:n(), .before = 'Radionuclide')
      
      dat <- table %>% 
        set_names(., c('idx', 'compound', c_names)) %>% 
        select(-28) %>% 
        mutate(across(!c(idx), as.character)) %>% 
        pivot_longer(., cols = !c(1:2), values_drop_na = T)
      
      df <- inner_join(dat, unit_table, join_by('name'))
      
      return(df)
    }) %>% list_rbind()
    
      write_rds(radtox, 'rais_radstox.RDS')
      
      rm(list = ls())
  }
  
}
# TODO stopped here
# Radionuclide Slope Factors ----------------------------------------------

# https://rais.ornl.gov/cgi-bin/tools/TOX_search?select=radslopes

# Public Remediation Goals  --------------------------------------------------------------------
{
  ## Chemical PRG ------------------------------------------------------------
  
  #TODO schedule for curation
  
  # https://rais.ornl.gov/cgi-bin/prg/PRG_search?select=chem
  
  
  ## Radionuclide PRG --------------------------------------------------------
  
  # https://rais.ornl.gov/cgi-bin/prg/PRG_search?select=rad
  
}



# Combined Benchmarks -----------------------------------------------------

#COMEBAK - Fix up this area
arars <- read_rds('eco_arars_ornl.RDS')

rais <- rais %>%
  bind_rows(rais, arars) %>% 
  rename(
    preferredName = analyte,
    cit = benchmark,
    local = organism, 
    unit_name = units,
    criterion_value = result,
    meta = ft) %>% 
  mutate(
    local = str_replace_all(local, 'Not specified|Not Specified', NA_character_),
    protection = 'Ecological',
    origin_category = case_when(
      str_detect(source, 'NOAA|OPP') ~ 'Federal',
      str_detect(source, 'EPA') ~ 'Federal',
      str_detect(source, 'California|Florida|Illinois|New Jersey|New York DEC|Ohio|Washington') ~ 'State',
      str_detect(source, 'Australia and New Zealand|British Columbia|Canada|ECC Canada|Dutch Soil Protection Act|Ontario|UK') ~ 'International',
      str_detect(source, 'LANL|ORNL') ~ 'Academic',
      str_detect(source, 'SETAC ECW') ~ 'Academic',
      #source == '' ~ 'Industry',
      str_detect(source, 'Consensus-Based') ~ 'Other',
      .default = 'Missing'
    ), 
    priority_id = 1,
    data_category = 'primary', 
    criterion_id = paste0('rais_', 1:n()))

# Total compounds ---------------------------------------------------------

rais_compounds <- distinct(rais, preferredName, cas, .keep_all = F) %>% 
  rename(orig_name = preferredName, 
         orig_cas = cas)

# CAS ---------------------------------------------------------------------

#TODO check sequence on this
rais_cas <- ComptoxR::ct_search(type = 'string', search_param = 'equal', query = rais_compounds$orig_cas) %>%
  filter(!is.na(searchName))

rais_cas_final <- rais_cas %>% 
  arrange(rank) %>% 
  select(dtxsid,casrn, preferredName, searchValue) %>% 
  distinct(searchValue, .keep_all = T)

rais_compounds <- left_join(rais_compounds, rais_cas_final, by = c('orig_cas' = 'searchValue'))

rais_compounds <- rais_compounds %>% 
  split(is.na(.$preferredName))

rais_compounds$`TRUE` <- rais_compounds$`TRUE` %>% 
  select(orig_name:orig_cas)

# Name --------------------------------------------------------------------

rais_name <- ComptoxR::ct_search(type = 'string', search_param = 'equal', query = rais_compounds$`TRUE`$orig_name) %>%
  filter(!is.na(searchName))

rais_name_final <- rais_name %>% 
  arrange(rank) %>% 
  select(dtxsid,casrn, preferredName, searchValue) %>% 
  distinct(searchValue, .keep_all = T)

rais_compounds$`TRUE` <- left_join(rais_compounds$`TRUE`, rais_name_final, by = c('orig_name' = 'searchValue'))

rais_compounds <- list_rbind(rais_compounds)

rais_compounds <- rais_compounds %>% 
  split(is.na(.$preferredName))

rio::export(rais_compounds$`TRUE`, file = paste0('rais_bad_',Sys.Date(),'.csv'))

rais_compounds <- list_rbind(rais_compounds)

rais_compounds <- rais_compounds %>% 
  filter(!is.na(preferredName))

rais <- rais %>% 
  rename(orig_name = preferredName, 
         orig_cas = cas) %>% 
  left_join(., rais_compounds, join_by(orig_name, orig_cas))

# Final export ------------------------------------------------------------

rio::export(rais, file = paste0('rais_curated_',Sys.Date(), '.RDS'))


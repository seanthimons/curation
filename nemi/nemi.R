{
  library(here)
  library(cli)
  library(janitor)
  library(rvest)
  library(httr)
  library(tidyverse)
  library(rio)
  library(ritis)
  library(textreuse)
  #library(ctxR)
  library(ComptoxR) #1.2.2.9003
  
  setwd(here('nemi'))
  
  load('.RData')
}


# Functions ---------------------------------------------------------------

safe_read_html <- possibly(read_html)

# Download ----------------------------------------------------------------

page <- read_html('https://www.nemi.gov/methods/browse_methods/?')

h3_elements <- page %>%
  html_elements(., 'h3') %>% html_text(., trim = T)

h4_elements <- page %>%
  html_elements(., 'h4') #%>% html_text(., trim = T)

h3_elements %>%
  html_elements(., 'h4') %>% 
  html_text(., trim = T)

l_max <- length(h3_elements)

tax_tree <- map(seq(1:l_max), ~{
  df <- page %>% 
    html_elements(., xpath = paste0('//*[@id="browse-methods-content-div"]/div[',.x,']')) %>%
    html_elements(., 'h4') %>% 
    html_text(., trim = T) %>%
    str_remove_all(., pattern = "-.*$") %>% 
    str_squish() %>% 
    as.list()
}) %>% 
  set_names(., page %>%
              html_elements(., 'h3') %>%
              html_text(., trim = T) %>% 
              str_remove_all(., pattern = "-.*$") %>% 
              str_squish() %>% 
              str_to_title())


raw_tbls <- 
  tax_tree %>% 
  unname() %>% 
  imap(., ~{
  
  idx <- .y
  len <- length(.x)
  
  h3 <- names(tax_tree[idx])
  
  map(seq(1:len), ~{
    
      cli_alert(paste0(h3,': ', tax_tree[[idx]][[.x]]))
      cli_text("")

      df <- 
        page %>%
        html_elements(., xpath = paste0(
              '//*[@id="browse-methods-content-div"]/div[',idx,']/div[3]/div[',.x,']/div/table')) %>%
        html_table() %>% 
        pluck(., 1) %>% 
        set_colnames(., c(
          'method_id',
          'method_descriptive_name',
          'method_source',
          'x4',
          'contact',
          'x6',
          'url'
          )) %>% 
        #as_tibble(., .name_repair = 'universal') %>%
        clean_names() %>% 
        filter(.,
               str_detect(method_id, pattern = 'URL|Contact', negate = T)
               ) %>% 
        select(-c(x4, x6, url)) %>% 
        mutate(method_source = str_remove_all(method_source, pattern = "\n.*$"))
  }) %>% 
    set_names(., tax_tree[[idx]]) %>% 
    list_rbind(., names_to = 'sub_cat')
  
}) %>%
  set_names(., names(tax_tree)) %>% 
  list_rbind(., names_to = 'super_cat')

links <- page %>% 
  html_elements(., "a") %>%
  map(., ~ tibble(
    url = html_attr(.x, "href"),
    text = html_text(.x, trim = TRUE)
  )) %>% 
  list_rbind() %>% 
  filter(., str_detect(url, pattern = 'method_summary', negate = F))

super_tbl <- left_join(raw_tbls, links, join_by(method_id == text)) %>% 
  #NOTE Removes weird pages
  #COMBAK Perhaps later...
  filter(super_cat != 'Statistical')

url_fs <- super_tbl %>% 
  select(method_id, url) #%>%
  #.[101:103, ]
#  slice_sample(n = 10)

fact_sheets <- url_fs$url%>% 
  imap(., ~{
  
    url <- paste0('https://www.nemi.gov', .x)
    max_int <- length(super_tbl$url)
    
    cli_alert(paste0(.y, ' / ', max_int, ': ', url))
    cli_text("")
    
    #HACK to work around dead pages
    page <- safe_read_html(url)

    if(is.null(page)){
      cli_alert_danger(paste0(url,' failed!'))
      cli_text("")
      return(NULL)

    }else{

      df <- list()
      
      df$fs <- page %>% 
        html_elements(., xpath = '//*[@id="method-tab"]/table[1]') %>% 
        html_table() %>%
        pluck(.,1) %>% 
        set_colnames(., c('name', 'value')) %>% 
        select(name, value)
      
      
      df$analytes <- page %>% 
        html_elements(., xpath = '//*[@id="analytes-tab"]/table[1]') %>% 
        html_table() %>% 
        pluck(.,1)
      
      df[["analytes"]][["Analyte"]] <- page %>% 
        html_elements(., xpath = '//*[@id="analytes-tab"]/table[1]') %>% 
        html_elements(., 'b') %>% 
        html_text()
      
      return(df)

    }
  }, .progress = T) %>% 
  set_names(., url_fs$method_id)

errors <- fact_sheets %>% 
  keep(., is.null)

write_rds(errors, 'nemi_method_errors.RDS')


# Cleaning ----------------------------------------------------------------


fact_sheets <- fact_sheets %>% compact()

front_page <- fact_sheets %>% 
  map(., ~{
    pluck(., 'fs') %>% 
    mutate(idx = 1) %>% 
    pivot_wider(., id_cols = idx, names_from = name, values_from = value) %>% 
    clean_names()
  },.progress = T) %>% 
  list_rbind(names_to = 'method_id') %>% 
  select(-idx)

analyte_list <- fact_sheets %>% 
  map(., ~{
    pluck(., 'analytes') %>% 
    mutate(across(everything(), as.character))
    },.progress = T) %>% 
  list_rbind(names_to = 'method_id') %>% 
  clean_names()

raw_analyte <- analyte_list %>%
  select(analyte) %>% 
  distinct()

cur_analyte <- raw_analyte %>% 
  rename(orig_name = analyte) %>% 
  mutate(
    idx = 1:n(),
    n = str_count(orig_name, pattern = "\\("), 
    raw_cas = 
            str_extract(orig_name, "\\([^()]*\\)$") %>%
            str_remove_all("^\\(|\\)$"),
    is_cas = webchem::as.cas(raw_cas),
    cas_chk = webchem::is.cas(raw_cas),
    raw_name = 
            str_remove(orig_name, "\\([^()]*\\)$") %>% 
            str_to_title()
      #%>% str_remove_all(., "\\([^()]*\\)$")
  ) %>% 
  split(.$cas_chk) %>% 
  set_names(., c('biol', 'chems'))



## chems -------------------------------------------------------------------

#NOTE stopped here - need to fix this search
chems_search <- ct_search(type = 'string', search_param = 'equal', query = cur_analyte$chems$is_cas)

chems <- cur_analyte$chems %>% 
  left_join(., chems_search, join_by('is_cas' == 'raw_search')) %>% 
  select(
    'orig_name',
    'idx',
    'n',
    #'raw_cas',
    #'is_cas',
    #'cas_chk',
    #'raw_name',
    #'searchValue',
    'dtxsid',
    #'dtxcid',
    'casrn',
    #'smiles',
    'preferredName',
    #'searchName',
    #'rank',
    #'hasStructureImage',
    #'isMarkush',
    #'searchMsgs',
    #'suggestions',
    #'isDuplicate',
  )

biol <- cur_analyte$biol %>% 
  
  separate_wider_delim(., cols = raw_cas, delim = '-', names = c('service', 'ident')) %>% 
  
  


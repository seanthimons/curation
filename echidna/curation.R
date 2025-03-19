# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(janitor)
  library(magrittr)
  library(tidyverse)
  library(httr)
  library(httr2)
  library(rvest)
  # library(ComptoxR)
  # library(stringdist)
  # library(fuzzyjoin)
  
  
  setwd(here('echidna'))
  load('echidna_raw_dat.Rdata')
}


# raw ---------------------------------------------------------------------

session <- 
  session("https://www.echidnacec.com/index.php")

login <- 
  session %>% 
  read_html() %>% 
  html_form() %>%
  pluck(1) %>% 
  html_form_set(
    username = Sys.getenv('ECHIDNA_USER'),
    password = Sys.getenv('ECHIDNA_PASSWORD')
  )

logged_in <- session %>%
  session_submit(login)

chems_summary <- 
  logged_in %>% 
  session_jump_to('https://www.echidnacec.com/view_PG.php') %>% 
  html_elements(., xpath = '/html/body/div/div[2]/div') %>% 
  #NOTE doesn't return full names...
  html_table()

chems_list <- chems_summary %>% 
  map(., ~pluck(., 'Chemical name')) %>% 
  list_c()

tags <- logged_in %>% 
  session_jump_to('https://www.echidnacec.com/view_PG.php') %>% 
  html_elements(., xpath = '/html/body/div/div[2]') %>% 
  html_elements(., "a") %>% 
  html_attr('href')

save.image('echidna_raw_dat.RData')

# chemical data page requests ---------------------------------------------

setwd(here('echidna', 'raw'))

as.list(tags) %>% 
  set_names(chems_list) %>% 
  #DEBUG testing here
  #.[1:10] %>% 
  map(., 
      possibly( ~{
        cli::cli_text(.x)
        logged_in %>% 
          session_jump_to(paste0('https://www.echidnacec.com/', .x)) %>% 
          read_html() %>% 
          xml2::write_html(., file = paste0(str_remove(.x, pattern = 'view.php\\?cid='), '.html'))
        #Sys.sleep(5)
      }), 
      #otherwise = NULL),
      .progress = T)



# Cleaning ----------------------------------------------------------------


chems_files <- 
  as.list(tags) %>% 
  #HACK removed the naming because it was truncated
  #set_names(chems_list) %>% 
  map(., ~str_remove(.x, pattern = 'view.php\\?cid='))

job::job({
chem_dat <- chems_files %>% 
  map2(., seq_along(.), ~{
    
    
    cli::cli_text(paste0( .y,"/", length(chems_files),": ", .x))
    
    page <- read_html(paste0(.x, '.html'))
    
    sections <- page %>%
      html_elements(., 'h3') %>%
      html_text() %>%
      as.list() %>% 
      modify_at(., 1, ~str_remove_all(.x, pattern = ' - .*')) %>% 
      map(., ~str_remove_all(.x, pattern = '\\r\\n')) %>% 
      { if(length(.) == 1){list(1, 2)}else{.}}
    
    dat <- imap(sections, possibly(~{
      .x <- page %>% 
        html_elements(., xpath = paste0('/html/body/div[1]/div/table[', .y, ']')) %>% 
        html_table() %>%
        pluck(1) #%>%  compact()
      #set_names(.x)
    }, 
    otherwise = NA)
    ) %>% 
      compact()
    
    #  keep(~ all(c("val", "param") %in% names(.x)))
    {
      d1 <- dat %>% 
        keep(~all(names(.x) %in% c("Parameter", 'Value'))) %>% 
        list_rbind() %>% 
        rename(
          dat = Parameter,
          val = Value
        ) %>% 
        mutate(
          val = na_if(val, "N/A"),
          val = na_if(val, "")
        )
      
      d2 <- dat %>% 
        keep(~all(c("Document") %in% names(.x))) %>% 
        { if (length(.) == 0) {NULL} else list_rbind(.) %>% 
            pivot_longer(., cols = !Document, names_to = 'dat', values_to = 'val', values_drop_na = F, values_transform = as.character) %>% 
            rename(source = Document) }
      
      d3 <- dat %>% 
        keep(~all(c("HQ?") %in% names(.x))) %>% 
        {if (length(.) == 0) {NULL} else list_rbind(.) %>% 
            pivot_longer(., cols = any_of(c('Water type', 'Endpoint')), names_to = 'Endpoint', values_to = 'dat', values_drop_na = T, values_transform = as.character) %>% 
            pivot_longer(., cols = any_of(c('Concentration', 'Dose')), values_to = 'val', values_drop_na = T, values_transform = as.character)}
      
      d4 <- dat %>% 
        keep(~all(c("Treatment category") %in% names(.x))) %>% 
        {if (length(.) == 0) {NULL} else list_rbind(.) %>%
            select(-Note)}
      
      d5 <- dat %>% 
        keep(~all(c("Confidence") %in% names(.x))) %>% 
        {if (length(.) == 0) {NULL} else list_rbind(.) }
      
      list(
        d1, d2, d3, d4, d5 
      ) %>% 
        compact()
      
      }
    
  }, .progress = T)
})


# Cleaning ----------------------------------------------------------------



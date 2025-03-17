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
  set_names(chems_list) %>% 
  map(., ~str_remove(.x, pattern = 'view.php\\?cid='))


#chems_files
q1 <- 
  chems_files %>% 
  keep_at(1:20)
  #sample(., size = 20)

q2 <- q1 %>% 
  .[1] %>% 
  imap(., ~{
    cli::cli_text(.y)
    page <- read_html(paste0(.x, '.html'))
    
    sections <- page %>%
      html_elements(., 'h3') %>%
      html_text() %>%
      as.list()
    
    sections[1] <- str_remove_all(sections[1], pattern = ' - .*')
    
    # sections <- sections %>%
    #   keep(~ .x %in% c(
    #     'General Information',
    #     'PBT Prioritisation',
    #     'Chronic toxicity hazard assessment',
    #     'Reason for Inclusion in ECHIDNA',
    #     'Relevant Australian Registration Authority',
    #     'Google Trends (BETA)',
    #     'Current Potable Water Guidelines',
    #     'Current Ecosystem Protection Guidelines',
    #     'Ecotoxicity Data \n',
    #     'Toxicity Data \n',
    #     'Occurrence Data',
    #     'Removal Data',
    #     'Risk Quotients',
    #     'Bayesian network model for prioritisation of research into CECs'
    #   )) %>% 
    #   unlist() %>% 
    #   str_squish()
    
    
    dat <- 
    
    dat <- page %>% 
      html_elements(., xpath = '/html/body/div[1]/div/table') %>% 
      html_table()
    
    # dat <- dat %>% 
    #   discard_at(., 1) %>% 
    #   set_names(sections)
    
    list(
      'sections' = sections,
      'tables' = dat
    )
    
  })

page <- read_html(paste0('38', '.html'))

sections <- page %>%
  html_elements(., 'h3') %>%
  html_text() %>%
  as.list()

sections[1] <- str_remove_all(sections[1], pattern = ' - .*')

dat <- imap()

dat <- page %>% 
  html_elements(., xpath = '/html/body/div[1]/div/table') %>% 
  html_table()
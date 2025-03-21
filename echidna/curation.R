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
  map(., ~str_remove(.x, pattern = 'view.php\\?cid=')) #%>% sample(., size = 20)

job::job({
  setwd(here('echidna', 'raw'))
  
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
        
        tbl <- page %>% 
          html_elements(., xpath = paste0('/html/body/div[1]/div/table[', .y, ']')) %>% 
          html_table(., na.strings = "") %>%
          pluck(1)
        
        meta <-  page %>%  
          html_elements(., xpath = paste0('/html/body/div[1]/div/table[', .y, ']')) %>% 
          map(., possibly(~{
            rows <- .x %>% html_elements("tr")
            map2(
              rows[-1],  # Ignore the first row (header)
              seq_along(rows[-1]),  # Adjust indices for rows
              ~ {
                cells <- .x %>% html_elements("td")
                map2(
                  cells,
                  seq_along(cells),
                  ~ {
                    spans <- html_elements(.x, "span")
                    tibble(
                      column = .y,
                      title = if (length(spans) > 0) html_attr(spans, "title") else NA
                    )
                  }
                ) %>%
                  list_rbind() %>%
                  distinct(., column, .keep_all = T) %>%
                  pivot_wider(names_from = column, values_from = title)
                
                
              }) %>% list_rbind() %>% select(where(~ !all(is.na(.x))))
          }, otherwise = NA)
          ) %>% pluck(., 1)
        
        list(tbl, meta) %>%
          compact() %>%
          list_cbind() #%>% select(where(~ !all(is.na(.x))))
      }, 
      otherwise = NA)) %>%
        compact() %>% 
        map(~ {
          .x %>%
            select(-any_of("Note")) %>%
            rename_with(~ if_else(.x %in% as.character(1:10), "Note", .x))
        })
      
      {
        d1 <- dat %>% 
          keep(~all(names(.x) %in% c("Parameter", 'Value', 'Note'))) %>% 
          list_rbind() %>% 
          rename(
            dat = Parameter,
            val = Value
          ) %>% 
          mutate(
            val = na_if(val, "N/A"),
            val = na_if(val, "")
          ) %>% 
          filter(
            str_detect(dat, pattern = 'Listed|Detected|Reported|Requested|3D|Note', negate = TRUE)
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
          general = d1,
          guidelines = d2, 
          eco_and_occurance = d3,
          treatment = d4, 
          bayes_model_prioritization = d5
        ) %>% 
          compact()
        
        }
      
    }, .progress = T) %>% 
    set_names(., chems_files)
  setwd(here('echidna'))
  
  # Cleaning ----------------------------------------------------------------
  {
    
    general <- chem_dat %>% 
      map(., ~pluck(., 'general')) %>% 
      list_rbind(names_to = 'idx')
    # pivot_wider(
    #   ., 
    #   names_from = dat, 
    #   values_from = val, 
    #   values_fill = NA
    # ) %>% 
    # select(where(~ !all(is.na(.x)))) %>% 
    
    
    chems <- general %>% 
      filter(dat %in% c('Chemical name:', 'CASRN:', 'DTXSID :')) %>%
      select(-Note) %>% 
      pivot_wider(
        .,
        names_from = dat,
        values_from = val,
        values_fill = NA
      ) %>% 
      mutate(idx = as.integer(idx))
    
    guidelines <- chem_dat %>% 
      map(., ~pluck(., 'guidelines')) %>% 
      list_rbind(names_to = 'idx') %>% 
      pivot_wider(
        ., 
        names_from = dat, 
        values_from = val, 
        values_fill = NA
      ) %>% 
      mutate(
        idx = as.integer(idx),
        across(where(is.character), ~na_if(.x, ""))
      ) %>% 
      inner_join(chems, ., join_by(idx))
    
    eco <- chem_dat %>% 
      map(., ~pluck(., 'eco_and_occurance')) %>% 
      list_rbind(names_to = 'idx') %>% 
      mutate(idx = as.integer(idx)) %>% 
      inner_join(chems, ., join_by(idx)) %>% 
      select(where(~ !all(is.na(.x))))
    
    treatment <- chem_dat %>% 
      map(., ~pluck(., 'treatment')) %>% 
      list_rbind(names_to = 'idx') %>% 
      mutate(
        idx = as.integer(idx), 
        `Treatment category` = na_if(`Treatment category`, "") %>% 
          str_replace_na(.),
        `Treatment type` =  na_if(`Treatment type`, "") %>% 
          str_replace_na(.)
      ) %>% 
      # pivot_wider(
      #   ., 
      #   names_from = 'Treatment type', 
      #   values_from = 'Percent removal',
      #   values_fill = NA
      # ) %>% 
      inner_join(chems, ., join_by(idx)) %>% 
      select(where(~ !all(is.na(.x))))
    
    bayes_model <- chem_dat %>% 
      map(., ~pluck(., 'bayes_model_prioritization')) %>% 
      list_rbind(names_to = 'idx') %>% 
      mutate(idx = as.integer(idx)) %>% 
      inner_join(chems, ., join_by(idx)) %>% 
      select(where(~ !all(is.na(.x))))
    
    
    chem_cur <- list(
      chems = chems,
      general = general,
      guidelines = guidelines, 
      eco_and_occurance = eco,
      treatment = treatment, 
      bayes_model_prioritization = bayes_model
    )
    
    writexl::write_xlsx(chem_cur, path = paste0(here('echidna','echidna_dump.xlsx')))
    
  }
})


# debugging ---------------------------------------------------------------

#page <- read_html(here('echidna', 'raw', '1307.html'))



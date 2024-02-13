#Packages----
{
  library(here)
  library(rio)
  library(tidyverse)
  library(cli)
  library(janitor)
  library(vroom)
  library(feather)
  library(hashr)
  library(stringdist)
}



#Downloads the FF db----
temp <- tempfile()
download.file('https://fracfocusdata.org/digitaldownload/FracFocusCSV.zip', temp)

if(dir.exists(here('temp'))){
  cli::cli_alert('Temp directory found!')
  unlink(here('temp'), recursive = TRUE)
  cli::cli_alert_success('Created temp dir')
  dir.create(here('temp'))
}else{
  cli::cli_alert_success('Created temp dir')
  dir.create(here('temp'))
}

unzip(temp, exdir = here('temp'))



#creates folders for the files----

##Disclosure----
if(dir.exists(here('disc'))){
  unlink(here('disc'), recursive = TRUE)
  dir.create(here('disc'))
}else{
  dir.create(here('disc'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'DisclosureList_[[:digit:]].csv$', full.names = TRUE)
file.copy(files_to_copy, here('disc'))
file.remove(files_to_copy)

##Water-----
if(dir.exists(here('water'))){
  unlink(here('water'), recursive = TRUE)
  dir.create(here('water'))
}else{
  dir.create(here('water'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'WaterSource_[[:digit:]].csv$', full.names = TRUE)
file.copy(files_to_copy, here('water'))
file.remove(files_to_copy)

##readme----
if(dir.exists(here('readme'))){
  unlink(here('readme'), recursive = TRUE)
  dir.create(here('readme'))
}else{
  dir.create(here('readme'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'readme csv.txt$', full.names = TRUE)
file.copy(files_to_copy, here('readme'))
file.remove(files_to_copy)

##frac records----
if(dir.exists(here('frac'))){
  unlink(here('frac'), recursive = TRUE)
  dir.create(here('frac'))
}else{
  dir.create(here('frac'))
}

files_to_copy <- list.files(path = here('temp'), pattern = 'FracFocusRegistry_\\d+.csv', full.names = TRUE)
file.copy(files_to_copy, here('frac'))
file.remove(files_to_copy)

rm(files_to_copy)
rm(temp)


#Stitches the db together-----
file_list <- list.files(path = "./frac/", pattern = '^FracFocusRegistry_\\d+.csv', full.names = TRUE)

frac_raw <- map_dfr(file_list, 
                     ~vroom::vroom(.x,
                                   col_types = cols(
                                     APINumber = col_character(),
                                     ClaimantCompany = col_character()
                                     ),
                                   na = c("", "NA", " ")),
                                  .id = "id",
                                  .progress = TRUE
                     ) %>% 
  clean_names()

write_feather(frac_raw,
              paste0('frac_raw','_', Sys.Date())
              )

rm(file_list)
gc()
unlink('frac', recursive = TRUE)
unlink('temp', recursive = TRUE)

#FF cleaning----
##Meta data list for number of obs lost to bad data----
  ff_meta_data <- vector(mode = 'list', length = 9L)
  ff_meta_data <- setNames(ff_meta_data, c('frac_raw',
                                           'ff_chemlist',
                                           'good_chem',
                                           'bad_chem',
                                           'bad_chem_cas_fail',
                                           'bad_chem_no_cas_match',
                                           'name_match',
                                           'bad_chem_no_name_match',
                                           'unique'))
  
  ff_meta_data$frac_raw <- nrow(frac_raw)
  
  ###Chem list----
  
  chem_list <- frac_raw %>%
    count(ingredient_common_name, cas_number) %>% 
    mutate(orig_cas = cas_number,
          cas_number = str_remove_all(cas_number,  "[^0-9]")
           ,cas_number = ifelse(cas_number == "", NA_character_, cas_number)
           ,cas_number = webchem::as.cas(cas_number)
           ) %>%
    rowwise() %>% 
    mutate(cas_number = webchem::as.cas(cas_number)) %>% 
    arrange(desc(n)) %>% 
    filter(!is.na(cas_number)
          # !is.na(ingredient_common_name) & 
           ) %>% 
    mutate(cas_chk = webchem::is.cas(cas_number)) %>% 
    ungroup()
  
  ##Product dictionary list-----
  product_family <- rio::import('products.xlsx') %>% 
  clean_names() %>% 
    mutate(
      spec_use = str_remove_all(spec_use, pattern = "\\(.*?\\)"),
      token = str_to_lower(spec_use),
      token = str_remove_all(token, pattern = "[^A-Za-z]")) %>% 
    rename(purpose = function_category,
           orig_purpose = description
           ) %>% 
    arrange(token) 
  
  #Within universe
  pf <- stringdistmatrix(
    product_family$token,
    product_family$token,
    method = 'jw',
    p = 0.1,
    useNames = "strings"
  )%>% 
    as.data.frame() %>% 
    mutate_all(~ ifelse(. > 0.2, NA, .)) %>% 
    rownames_to_column('token') %>% 
    pivot_longer(-token, names_to = 'family', values_to = 'dist') %>% 
    filter(!is.na(dist)) %>%
    arrange(token, dist)
  
  usage <- frac_raw %>% 
    filter(!is.na(cas_number) & !is.na(ingredient_name)) %>%
    filter(purpose != 'Ingredient Container Purpose') %>% 
    count(purpose) %>% 
    arrange(desc(n)) %>% 
    mutate(
      orig_use = purpose,
      purpose = stringr::str_to_lower(purpose),
      length = str_length(purpose),
      comma = str_count(orig_use, pattern = ',')
           ) %>% 
    filter(length > 2 & n > 2 & comma <= 2)
   
  temp <- usage %>% 
    mutate(token = str_remove_all(purpose, pattern = "[^A-Za-z]"),
           token = na_if(token, "")) %>%
    distinct(token, .keep_all = T) %>%
    filter(!is.na(token)) %>% 
    arrange(token) %>% 
    select(-c(length, comma))
  
  dist <- stringdistmatrix(
                     temp$token,
                     product_family$token,
                     method = 'jw',
                     p = 0.1,
                     useNames = "strings"
                     )
 
  dist2 <- stringdistmatrix(
    temp$token, 
    product_family$token, 
    method = 'jw', 
    useNames = 'strings',
    p = 0.1
   ) %>% 
    as.data.frame() %>% 
    mutate_all(~ ifelse(. > 0.2, NA, .)) %>% 
    rownames_to_column('token') %>% 
    pivot_longer(-token, names_to = 'family', values_to = 'dist') %>% 
    filter(!is.na(dist)) %>%
    arrange(token, dist)

  #missing HERE-----
  prod_missing <- temp %>% 
    filter(!(token %in% unique(dist2$token))) %>% 
    stringdistmatrix(
      .$token, 
      product_family$token, 
      method = 'lcs', 
      useNames = 'strings',
      #p = 0.1
    ) %>% 
    as.data.frame() %>% 
    #mutate_all(~ ifelse(. > 0.2, NA, .)) %>% 
    rownames_to_column('token') %>% 
    pivot_longer(-token, names_to = 'family', values_to = 'dist') %>% 
    #filter(!is.na(dist)) %>%
    arrange(token, dist)
  
  
  
  hc <- hclust(
      stringdistmatrix(
        c(hf_family$token, temp$token), 
        method = 'jw', 
        useNames = 'strings',
        p = 0.1
        )
      )
  
  df <- data.frame(word = c(hf_family$token, temp$token), 
                   clust = cutree(hc, h = )) %>%
    mutate(family = hf_family$token[clust])
  
  
  ##########################HERE
  
  rownames(dist_mat) <- usage$purpose
  colnames(dist_mat) <- usage$purpose
  
  
  
  #####STOP------
  ff_chem_list <- frac_full %>% 
    #removes wells that do not report water volumes
    #filter(!is.na(total_base_water_volume)) %>%
    #filters out non-reporting rows cas number
    filter(grepl('[[:punct:]]', ingredient_key)) %>% 
    #removes non-cas numbers
    filter(!grepl('[[:alpha:]]', cas_number))
  #removes dashes from cas number
  ff_chem_list$cas_number <-  gsub('[[:punct:]]','', ff_chem_list$cas_number)
  #converts cas string to actual numbers
  ff_chem_list$cas_number <- as.numeric(ff_chem_list$cas_number)
  #regen cas numbers
  ff_chem_list$cas_number <- cas_conv(ff_chem_list$cas_number)
  toc()
  
  #RM OUT THE FRAC FULL FILE----
  message(red('\nRemoving parent file\n'))
  rm(frac_full, envir = globalenv())
  
  #Checks for valid cas numbers----
  message('Checking for valid compounds \n')
  tic()
  cas_list <- unique(ff_chem_list$cas_number) %>% 
    data.frame() %>%  
    rename("cas_number" = '.')
  cas_list[,'cas_chk'] <- NA 
  cas_list$cas_chk <- is.cas(cas_list$cas_number)
  toc()
  
  
  ###joins against master data----
  message('\nComparing against parent data...\n')
  tic()
  ff_chem_list <- left_join(ff_chem_list, cas_list, by = "cas_number")
  ff_meta_data$ff_chemlist <- nrow(ff_chem_list)
  toc()
  rm(cas_list)
  
  #Saves init chemical list----
  message(green('\nSaving init chemical list...\n'))
  write_feather(ff_chem_list, paste0('~/curation/cache/ff_chem_list','_', Sys.Date()))
  
  #Splits the ffdb into good and bad data----
  ###loads DTX matching file----
  message('\nLoading DSSTOX dictionary file...\n')
  tic()
  setwd('~/curation/dtx_map')
  dtx_map <- rio::import(list.files('.', pattern = "\\.csv$", full.names = TRUE))
  janitor::clean_names(dtx_map)
  setwd('~/curation/')
  dtx_temp <- dplyr::select(dtx_map, dtxsid, casrn, preferredName)
  toc()
  
  #Unique list of compounds based on cas number----
  message('\nComparing against parent data...')
  tic()
  unique_chems <- distinct(ff_chem_list, ingredient_name, cas_number, cas_chk)
  unique_chems <- left_join(unique_chems, dtx_temp, by = c('cas_number' = 'casrn'))
  ff_meta_data$unique <- length(unique(unique_chems$ingredient_name))
  toc()
  
  #Valid DTX----
  message(green('\nCreating validated chemical list called good_chem...\n'))
  tic()
  good_chem <- filter(unique_chems, !is.na(dtxsid))
  ff_meta_data$good_chem <- length(unique(good_chem$preferredName))
  toc
  
  #Invalid DTX----
  message(red('\nCreating invalid chemical list (no DTX match) called bad_chem....\n'))
  tic()
  bad_chem <- filter(unique_chems, is.na(dtxsid))
  ff_meta_data$bad_chem <- nrow(bad_chem)
  toc()
  
  ###Invalid cas numbers----
  message(red('\nCreating invalid chemical list (no DTX match and bad CASRN) called bad_chem_cas_fail....\n'))
  tic()
  bad_chem_cas_fail <- filter(bad_chem, cas_chk == FALSE)
  ff_meta_data$bad_chem_cas_fail <- nrow(bad_chem_cas_fail)
  toc()
  
  ###Valid cas numbers, but no valid match to dtx list----
  message(yellow('\nCreating valid CASRN (but no valid match to DTX) list...\n'))
  message(yellow('bad_chem_no_cas_match\n'))
  tic()
  bad_chem_no_cas_match <- filter(bad_chem, cas_chk == TRUE)
  ff_meta_data$bad_chem_no_cas_match <- nrow(bad_chem_no_cas_match)
  toc()
  
  ###Valid compounds by name match, various cas numbers in original dataset----
  message(yellow('\nCreating valid compounds by name match, various CASRN numbers in parent dataset\n'))
  tic()
  name_match <- inner_join(dplyr::select(bad_chem, ingredient_name:cas_chk), dtx_temp, by = c('ingredient_name' = 'preferredName')) %>% 
    distinct(cas_number, dtxsid, .keep_all = TRUE)
  ff_meta_data$name_match <- nrow(name_match)
  toc()
  ###No valid name or cas number match, for follow up----
  message(red('\nNo valid name or CASRN match, for follow up...\n'))
  tic()
  bad_chem_no_name_match <- anti_join(dplyr::select(bad_chem, ingredient_name:cas_chk), dtx_temp, by = c('ingredient_name' = 'preferredName')) 
  ff_meta_data$bad_chem_no_name_match <- nrow(bad_chem_no_name_match)
  rm(dtx_map, dtx_temp)
  toc()
  
  #Fixes warnings----
  options(warn = defaultW)
  
  #Saves data----
  ##Bad data----
  save_data <- list(bad_chem,
                    bad_chem_cas_fail,
                    bad_chem_no_cas_match,
                    bad_chem_no_name_match)
  
  names(save_data) <- c('bad_chem',
                        'bad_chem_cas_fail',
                        'bad_chem_no_cas_match',
                        'bad_chem_no_name_match'
  )
  setwd('~/curation/bad_data')
  lapply(1:length(save_data), function(i) write.csv(save_data[[i]], 
                                                    file = paste0(names(save_data[i]), '_', Sys.Date(), ".csv"),
                                                    row.names = FALSE))
  rm(save_data,
     bad_chem,
     bad_chem_cas_fail,
     bad_chem_no_cas_match,
     bad_chem_no_name_match)
  
  ##Good data----
  save_data <- list(good_chem,
                    name_match, 
                    unique_chems)
  
  names(save_data) <- c('good_chem',
                        'name_match', 
                        'unique_chems')
  
  setwd('~/curation/cache')
  lapply(1:length(save_data), function(i) write.csv(save_data[[i]], 
                                                    file = paste0(names(save_data[i]), '_', Sys.Date(), ".csv"),
                                                    row.names = FALSE))
  rm(save_data)
  setwd('~/curation')
  

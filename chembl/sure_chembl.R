# Packages ----------------------------------------------------------------

{
	library(here)
	library(rio)
	library(janitor)
	library(tidyverse)
	library(rvest)
	library(httr2)
	library(arrow)

	setwd(here('chembl'))
	
}


# Downloads page ---------------------------------------------------------

#Not needed
# folder_list <- read_html("https://ftp.ebi.ac.uk/pub/databases/chembl/SureChEMBL/bulk_data/") %>% 
# 	html_table() %>% 
# 	pluck(1) %>% 
# 	janitor::remove_empty(which = c("rows", "cols")) %>% 
# 	janitor::clean_names() %>% 
# 	mutate(across(everything(), ~na_if(.x, ""))) %>% 
# 	janitor::remove_empty(which = c("rows", "cols")) %>% 
# 	select(name, last_modified) %>% 
# 	filter(!str_detect(name, pattern = 'latest/'))

# folder_list %>% 	
# mutate(last_modified = lubridate::ymd_hm(last_modified)) %>% 
# 	arrange(desc(last_modified))

download.file("https://ftp.ebi.ac.uk/pub/databases/chembl/SureChEMBL/bulk_data/latest/compounds.parquet", destfile = here('final', 'sure_chembl.parquet'))

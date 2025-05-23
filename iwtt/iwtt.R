# Packages ----------------------------------------------------------------

{
    library(rio)
    library(janitor)
    library(tidyverse)
    library(here)
    library(httr)
    library(rvest)
    library(polite)
    library(duckdb)
    library(duckplyr)
  
  setwd(here('iwtt'))
  
  iwtt_con <- dbConnect(
    duckdb(),
    dbdir = 'iwtt_db.duckdb'
    #dbdir = ":memory:"
  )
  
}

# Download ----------------------------------------------------------------

#ZIP
read_html_live(url = 'https://watersgeo.epa.gov/iwtt/download-database') %>% 
  html_elements(., xpath = '//*[@id="root"]/div/div/section/section/ul') %>% 
  html_elements(., 'a') %>% 
  html_attr('href') %>% 
  str_remove_all(., "./assets/") %>% 
  map(., ~{
    download.file(
      url = paste0('https://watersgeo.epa.gov/iwtt/assets/', .x),
      destfile = .x,
      mode = 'wb'
    )
  })

#treatment technology descriptions 

# technology_descriptions <- read_html_live(url = 'https://watersgeo.epa.gov/iwtt/treatment-technologies') %>% 
#   html_elements(., xpath = '//*[@id="root"]/div/div/section/section/table') %>% 
#   html_table() %>% 
#   pluck(., 1) %>% 
#   mutate(across(everything(), ~na_if(.x, 'N/A')))

dbWriteTable(
  iwtt_con, 
  name = 'treatment_descriptions',
  value = read_html_live(url = 'https://watersgeo.epa.gov/iwtt/treatment-technologies') %>% 
    html_elements(., xpath = '//*[@id="root"]/div/div/section/section/table') %>% 
    html_table() %>% 
    pluck(., 1) %>% 
    mutate(across(everything(), ~na_if(.x, 'N/A'))) %>% 
    clean_names(),
  overwrite = TRUE
  )

# Unzip --------------------------------------------------------------------

dir.create(here('iwtt', 'raw'))

setwd(here('iwtt', 'raw'))

unzip(zipfile = here('iwtt', list.files(here('iwtt'), pattern = '.zip'))) 

unlink(here('iwtt', list.files(here('iwtt'), pattern = '.zip')))

# Load --------------------------------------------------------------------

iwtt_con <- dbConnect(
  duckdb(),
  dbdir = 'iwtt_db.duckdb'
  #dbdir = ":memory:"
  )

lof <- list.files(here('iwtt', 'raw'), pattern = '.csv')

map(lof, ~{
  
  df <- data.table::fread(
    input = here('iwtt', 'raw', .x),
    na.strings = c('NA')
    )%>% 
    mutate(
      across(where(is.character), stringi::stri_enc_tonative),
      across(where(is.character), ~na_if(.x, ""))
      ) %>% 
    janitor::clean_names()

  dbWriteTable(
    conn = iwtt_con,
    name = str_to_lower(
      str_remove(.x, pattern = ".csv")),
    value = df,
    overwrite = TRUE
    )
}, .progress = TRUE)

rm(lof)

unlink(here('iwtt', 'raw'), recursive = TRUE)

iwtt_dict <- rio::import_list(file = list.files(here('iwtt'), pattern = '.xlsx')) %>% 
  discard_at(., 1) %>% 
  map(., clean_names) %>% 
  map(., remove_empty) %>% 
  set_names(., c('table_metadata', 'value_metadata'))

imap(iwtt_dict, ~{
  
  dbWriteTable(
    conn = iwtt_con,
    name = .y,
    value = .x,
    overwrite = TRUE
  )
}, .progress = TRUE)

dbListTables(iwtt_con)

rm(iwtt_dict)

unlink(list.files(path = here('iwtt'), pattern = '.xlsx'))

# Transform ---------------------------------------------------------------

dbListTables(iwtt_con)

## parameter ---------------------------------------------------------------

dbWriteTable(
  conn = iwtt_con, 
  name = "",
  value = left_join(
  tbl(iwtt_con, 'parameter') %>% 
    select(),
  tbl(iwtt_con, 'key_parameter_code') %>% 
    select(
      'paramid',
      'pollutant_search_term',
      'cas_nmbr',
      'category',
      'category_2'
    ),
  join_by('paramid')
  )
)

left_join(
  tbl(iwtt_con, 'parameter'),
  tbl(iwtt_con, 'key_parameter_code') %>% 
    select(
      'paramid',
      'pollutant_search_term',
      'cas_nmbr',
      'category',
      'category_2'
    ),
  join_by('paramid')
)

tbl(iwtt_con, '')

## treatment ---------------------------------------------------------------

#top level 
tbl(iwtt_con, 'treatment_system') %>%
  filter(psc_code == '435') %>% 
  glimpse()

#Order of treatment
tbl(iwtt_con, 'treatment_units')

#technologies
tbl(iwtt_con, 'key_treatment_tech_codes') %>% 
  distinct(tt_code, .keep_all = TRUE) %>% 
  #select(-tt_id, -tt_variation) %>% 
  arrange(tt_category) %>% 
  print(n = Inf) %>% 
  distinct(tt_category)
  
# reference info ----------------------------------------------------------

#PENDING

# Assuming your data frame is named iwtt_df and has the column 'treatment_technology_name'
# Make sure the names in your data frame match exactly (case-sensitive)

iwtt_df <- iwtt_df %>%
  mutate(
    Treatment_Group = case_when(
      treatment_technology_name == "Adsorptive Media" ~ "Adsorptive",
      treatment_technology_name == "Advanced Oxidation Processes, Not Classified Elsewhere (NEC)" ~ "Chemical",
      treatment_technology_name == "Aeration" ~ "Physical",
      treatment_technology_name == "Aerobic Biological Treatment" ~ "Biological",
      treatment_technology_name == "Aerobic Fixed Film Biological Treatment" ~ "Biological",
      treatment_technology_name == "Aerobic Suspended Growth" ~ "Biological",
      treatment_technology_name == "Alkaline Chlorination" ~ "Chemical",
      treatment_technology_name == "Anaerobic Biological Treatment" ~ "Biological",
      treatment_technology_name == "Anaerobic Fixed Film Biological Treatment" ~ "Biological",
      treatment_technology_name == "Anaerobic Membrane Bioreactor" ~ "Hybrid (Biological/Physical)",
      treatment_technology_name == "Anaerobic Suspended Growth" ~ "Biological",
      treatment_technology_name == "Bag and Cartridge Filtration" ~ "Physical",
      treatment_technology_name == "Ballasted Clarification" ~ "Physical",
      treatment_technology_name == "Bioaugmentation" ~ "Biological",
      treatment_technology_name == "Biofilm Airlift Suspension Reactor" ~ "Biological",
      treatment_technology_name == "Biological Activated Carbon Filters" ~ "Hybrid (Biological/Adsorptive)",
      treatment_technology_name == "Biological Nutrient Removal" ~ "Biological",
      treatment_technology_name == "Biological Treatment" ~ "General/Category",
      treatment_technology_name == "Capacitive Deionization" ~ "Electrochemical",
      treatment_technology_name == "Centrifugal Separators" ~ "Physical",
      treatment_technology_name == "Chemical Disinfection" ~ "Chemical",
      treatment_technology_name == "Chemical Nitrogen Removal" ~ "Chemical",
      treatment_technology_name == "Chemical Oxidation" ~ "Chemical",
      treatment_technology_name == "Chemical Phosphorous Removal" ~ "Chemical",
      treatment_technology_name == "Chemical Precipitation" ~ "Chemical",
      treatment_technology_name == "Chemical Treatment" ~ "General/Category",
      treatment_technology_name == "Clarification" ~ "Physical",
      treatment_technology_name == "Cloth Filtration" ~ "Physical",
      treatment_technology_name == "Constructed Wetlands" ~ "Biological",
      treatment_technology_name == "Controlled Hydrodynamic Cavitation" ~ "Physical",
      treatment_technology_name == "Crystallization" ~ "Physical",
      treatment_technology_name == "Dechlorination" ~ "Chemical",
      treatment_technology_name == "Degasification" ~ "Physical",
      treatment_technology_name == "Denitrification Filters" ~ "Hybrid (Biological/Physical)",
      treatment_technology_name == "Dissolved Air Flotation" ~ "Physical",
      treatment_technology_name == "Dissolved Gas Flotation" ~ "Physical",
      treatment_technology_name == "Distillation" ~ "Thermal",
      treatment_technology_name == "Electrocoagulation" ~ "Electrochemical",
      treatment_technology_name == "Electrodialysis" ~ "Electrochemical",
      treatment_technology_name == "Enhanced Biological Phosphorus Removal" ~ "Biological",
      treatment_technology_name == "Evaporation" ~ "Thermal",
      treatment_technology_name == "Flow Equalization" ~ "Physical",
      treatment_technology_name == "Forward Osmosis" ~ "Physical",
      treatment_technology_name == "Gasification" ~ "Thermal",
      treatment_technology_name == "Granular Activated Carbon Adsorption" ~ "Adsorptive",
      treatment_technology_name == "Granular Sludge Sequencing Batch Reactor" ~ "Biological",
      treatment_technology_name == "Granular-Media Filtration" ~ "Physical",
      treatment_technology_name == "Hydrolysis, Alkaline or Acid" ~ "Chemical",
      treatment_technology_name == "Integrated Fixed Film Activated Sludge" ~ "Biological",
      treatment_technology_name == "Ion Exchange" ~ "Chemical",
      treatment_technology_name == "Liquid Extraction" ~ "Physical",
      treatment_technology_name == "Mechanical Pre-Treatment" ~ "Physical",
      treatment_technology_name == "Media Filtration" ~ "General/Category",
      treatment_technology_name == "Membrane Bioreactor" ~ "Hybrid (Biological/Physical)",
      treatment_technology_name == "Membrane Distillation" ~ "Thermal",
      treatment_technology_name == "Membrane Filtration" ~ "General/Category",
      treatment_technology_name == "Micro- and Ultra-Membrane Filtration" ~ "Physical",
      treatment_technology_name == "Moving Bed Bioreactor" ~ "Biological",
      treatment_technology_name == "Nanofiltration" ~ "Physical",
      treatment_technology_name == "Oil/Water Separation" ~ "Physical",
      treatment_technology_name == "Other Filtration" ~ "Physical",
      treatment_technology_name == "Ozonation" ~ "Chemical",
      treatment_technology_name == "Physical Treatment" ~ "General/Category",
      treatment_technology_name == "Powdered Activated Carbon" ~ "Adsorptive",
      treatment_technology_name == "Reverse Osmosis" ~ "Physical",
      treatment_technology_name == "Sorption" ~ "General/Category",
      treatment_technology_name == "Stripping" ~ "Physical",
      treatment_technology_name == "Surface Impoundment" ~ "Physical",
      treatment_technology_name == "Unspecified Biological Treatment" ~ "Biological",
      treatment_technology_name == "Ultrasound" ~ "Physical",
      treatment_technology_name == "UV" ~ "Physical/Chemical",
      treatment_technology_name == "Wet Air Oxidation" ~ "Thermal",
      treatment_technology_name == "Zero Valent Iron" ~ "Chemical",
      TRUE ~ NA_character_ # Default case for unmatched names
    ),
    Treatment_Stage = case_when(
      treatment_technology_name == "Adsorptive Media" ~ "Tertiary",
      treatment_technology_name == "Advanced Oxidation Processes, Not Classified Elsewhere (NEC)" ~ "Advanced",
      treatment_technology_name == "Aeration" ~ "Ancillary/Support",
      treatment_technology_name == "Aerobic Biological Treatment" ~ "Secondary",
      treatment_technology_name == "Aerobic Fixed Film Biological Treatment" ~ "Secondary",
      treatment_technology_name == "Aerobic Suspended Growth" ~ "Secondary",
      treatment_technology_name == "Alkaline Chlorination" ~ "Specialized",
      treatment_technology_name == "Anaerobic Biological Treatment" ~ "Secondary",
      treatment_technology_name == "Anaerobic Fixed Film Biological Treatment" ~ "Secondary",
      treatment_technology_name == "Anaerobic Membrane Bioreactor" ~ "Secondary",
      treatment_technology_name == "Anaerobic Suspended Growth" ~ "Secondary",
      treatment_technology_name == "Bag and Cartridge Filtration" ~ "Tertiary",
      treatment_technology_name == "Ballasted Clarification" ~ "Primary",
      treatment_technology_name == "Bioaugmentation" ~ "Ancillary/Support",
      treatment_technology_name == "Biofilm Airlift Suspension Reactor" ~ "Secondary",
      treatment_technology_name == "Biological Activated Carbon Filters" ~ "Tertiary",
      treatment_technology_name == "Biological Nutrient Removal" ~ "Tertiary",
      treatment_technology_name == "Biological Treatment" ~ "General/Category",
      treatment_technology_name == "Capacitive Deionization" ~ "Advanced",
      treatment_technology_name == "Centrifugal Separators" ~ "Preliminary",
      treatment_technology_name == "Chemical Disinfection" ~ "Disinfection",
      treatment_technology_name == "Chemical Nitrogen Removal" ~ "Tertiary",
      treatment_technology_name == "Chemical Oxidation" ~ "Advanced",
      treatment_technology_name == "Chemical Phosphorous Removal" ~ "Tertiary",
      treatment_technology_name == "Chemical Precipitation" ~ "Tertiary",
      treatment_technology_name == "Chemical Treatment" ~ "General/Category",
      treatment_technology_name == "Clarification" ~ "Primary/Secondary Support",
      treatment_technology_name == "Cloth Filtration" ~ "Tertiary",
      treatment_technology_name == "Constructed Wetlands" ~ "Secondary/Tertiary",
      treatment_technology_name == "Controlled Hydrodynamic Cavitation" ~ "Specialized",
      treatment_technology_name == "Crystallization" ~ "Sludge Handling/Specialized",
      treatment_technology_name == "Dechlorination" ~ "Disinfection",
      treatment_technology_name == "Degasification" ~ "Specialized",
      treatment_technology_name == "Denitrification Filters" ~ "Tertiary",
      treatment_technology_name == "Dissolved Air Flotation" ~ "Primary/Sludge Handling",
      treatment_technology_name == "Dissolved Gas Flotation" ~ "Primary/Sludge Handling",
      treatment_technology_name == "Distillation" ~ "Advanced",
      treatment_technology_name == "Electrocoagulation" ~ "Tertiary",
      treatment_technology_name == "Electrodialysis" ~ "Advanced",
      treatment_technology_name == "Enhanced Biological Phosphorus Removal" ~ "Tertiary",
      treatment_technology_name == "Evaporation" ~ "Sludge Handling/Specialized",
      treatment_technology_name == "Flow Equalization" ~ "Preliminary",
      treatment_technology_name == "Forward Osmosis" ~ "Advanced",
      treatment_technology_name == "Gasification" ~ "Sludge Handling",
      treatment_technology_name == "Granular Activated Carbon Adsorption" ~ "Tertiary",
      treatment_technology_name == "Granular Sludge Sequencing Batch Reactor" ~ "Secondary",
      treatment_technology_name == "Granular-Media Filtration" ~ "Tertiary",
      treatment_technology_name == "Hydrolysis, Alkaline or Acid" ~ "Ancillary/Support",
      treatment_technology_name == "Integrated Fixed Film Activated Sludge" ~ "Secondary",
      treatment_technology_name == "Ion Exchange" ~ "Tertiary/Advanced",
      treatment_technology_name == "Liquid Extraction" ~ "Specialized",
      treatment_technology_name == "Mechanical Pre-Treatment" ~ "Preliminary",
      treatment_technology_name == "Media Filtration" ~ "General/Category",
      treatment_technology_name == "Membrane Bioreactor" ~ "Secondary",
      treatment_technology_name == "Membrane Distillation" ~ "Advanced",
      treatment_technology_name == "Membrane Filtration" ~ "General/Category",
      treatment_technology_name == "Micro- and Ultra-Membrane Filtration" ~ "Tertiary",
      treatment_technology_name == "Moving Bed Bioreactor" ~ "Secondary",
      treatment_technology_name == "Nanofiltration" ~ "Advanced",
      treatment_technology_name == "Oil/Water Separation" ~ "Preliminary/Primary",
      treatment_technology_name == "Other Filtration" ~ "Tertiary/Specialized",
      treatment_technology_name == "Ozonation" ~ "Disinfection/Advanced",
      treatment_technology_name == "Physical Treatment" ~ "General/Category",
      treatment_technology_name == "Powdered Activated Carbon" ~ "Tertiary/Ancillary/Support",
      treatment_technology_name == "Reverse Osmosis" ~ "Advanced",
      treatment_technology_name == "Sorption" ~ "General/Category",
      treatment_technology_name == "Stripping" ~ "Specialized/Tertiary",
      treatment_technology_name == "Surface Impoundment" ~ "Tertiary/Sludge Handling",
      treatment_technology_name == "Unspecified Biological Treatment" ~ "Secondary",
      treatment_technology_name == "Ultrasound" ~ "Specialized",
      treatment_technology_name == "UV" ~ "Disinfection/Advanced",
      treatment_technology_name == "Wet Air Oxidation" ~ "Sludge Handling/Specialized",
      treatment_technology_name == "Zero Valent Iron" ~ "Tertiary/Advanced",
      TRUE ~ NA_character_ # Default case for unmatched names
    )
  )

# Display the first few rows with the new columns (optional)
# head(iwtt_df)

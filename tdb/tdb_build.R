# Packages ----------------------------------------------------------------

{
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
 # library(ComptoxR)
  library(jsonlite)
 # library(colorspace)
  library(DT)
}

# Raw ---------------------------------------------------------------------

list_import <- list.files(here('tdb', 'tdb'), full.names = F) %>%
  as.list() %>% 
  discard_at(., 1) #discards app_options.json

raw_json <- vector(mode = 'list', length = length(list_import))

raw_json <- map(list_import, ~{
  loc <- paste0(here('tdb', 'tdb'),'/',.x)
  #.x <- paste(readLines(loc, warn = F),collapse = "")
  .x <- readLines(loc, warn = F)
  .x <- stringi::stri_enc_toutf8(.x, validate = T)
  .x <- sub(pattern = ",\"items\":", replacement = "{\"items\":", .x, useBytes = T)
  .x <- sub(pattern = "]}]}", replacement = "]}", .x, useBytes = T)
  # .x <- sub(pattern = "\x92", replacement = "", .x, useBytes = T)
  
}, .progress = T)

names(raw_json) <- str_remove_all(list_import, '.json')

raw_json <- 
  map(raw_json, as.list)

raw_json <- 
  map(raw_json, ~replace(., 1, "{\"items\":"))

raw_json <- 
  map(raw_json, 
  ~{unlist(.x) %>% paste(., collapse = "")})


# Build -------------------------------------------------------------------

js <- map(raw_json, fromJSON) %>% 
  map(., pluck(1)) %>% 
  keep(names(.) %in% c(
  #  'CONT_PROC_AUDIT_TRAIL',
  #  'CONT_PROC_SUMM_REF_XREF',
    'CONTAMINANT',
  #  'CONTAMINANT_AUDIT_TRAIL',
  #  'CONTAMINANT_DESC',
  #  'CONTAMINANT_IMAGE',
  #  'CONTAMINANT_PARAMETER',
  #  'CONTAMINANT_PROC_SUMM_IMAGE',
  #  'CONTAMINANT_PROCESS_DESC',
    'CONTAMINANT_PROCESS_SUMMARY', #compound-treatment-result rel't
  #  'CONTAMINANT_PROPERTY',
  #  'CONTAMINANT_QUICK_LINK',
  #  'CONTAMINANT_REF_XREF', #may not be needed
  #  'CONTAMINANT_SYNONYM',
    'CONTAMINANT_TYPE',
  
    'CP_DETAIL_ADSORP',
    'CP_DETAIL_AIR',
    'CP_DETAIL_BIO_TREATMENT',
    'CP_DETAIL_BIOLOGICAL_FILTER',
    'CP_DETAIL_CHEMICAL',
    'CP_DETAIL_CHLORAMINE',
    'CP_DETAIL_CHLORINE',
    'CP_DETAIL_CHLORINE_DIOXIDE',
    'CP_DETAIL_CONV_TREATMENT',
    'CP_DETAIL_DE_FILTER',
    'CP_DETAIL_DIRECT_FILTER',
    'CP_DETAIL_GAC',
    'CP_DETAIL_GAC_ISOTHERM',
    'CP_DETAIL_GENERAL',
    'CP_DETAIL_H2O2',
    'CP_DETAIL_IEX',
    'CP_DETAIL_MEMBRANE_SEP',
    'CP_DETAIL_MF_UF',
    'CP_DETAIL_OZONE',
    'CP_DETAIL_OZONE_H2O2',
    'CP_DETAIL_PAC',
    'CP_DETAIL_PERMANGANATE',
    'CP_DETAIL_PRESSURE_FILTER',
    'CP_DETAIL_SLOW_SAND',
    'CP_DETAIL_SOFTENING',
    'CP_DETAIL_UV',
    'CP_DETAIL_UV_H2O2',
    'CP_DETAIL_UV_IRRAD_O3',
  
  #  'FUTURE_CONTAMINANT',
  #  'QUICK_LINK',
  #  
    'REFERENCE',
    'REFERENCE_TAG',
    'REFERENCE_TAG_TYPE',
  
    'TAG',
    'TREATMENT_PROCESS',
    'TREATMENT_PROCESS_COLUMN'
  #  'TREATMENT_PROCESS_DESC',
  #  'TREATMENT_PROCESS_IMAGE'
  )) %>% 
  map(.,  function(df) {
    if ("tmsp_last_updt" %in% names(df)) {
      df[, !names(df) %in% "tmsp_last_updt", drop = FALSE]
    } else {
      df
    }
  })

## References --------------------------------------------------------------

ref <- js %>% 
  keep(names(.) %in% c(
    'REFERENCE',
    'REFERENCE_TAG',
    'REFERENCE_TAG_TYPE'
  ))

ref$REFERENCE_TAG <- ref$REFERENCE_TAG %>% 
  left_join(., ref$REFERENCE_TAG_TYPE, join_by(tag_type_code))

ref <- ref$REFERENCE %>% 
  left_join(., ref$REFERENCE_TAG, join_by(reference_tag_id == tag_id))

## Compound ----------------------------------------------------------------

comp <- js %>% 
  keep(str_detect(names(js), 'CONTAMINANT'))
  
comp$CONTAMINANT <- comp$CONTAMINANT %>%   
  left_join(., comp$CONTAMINANT_TYPE, join_by(contaminant_type_code))

comp <- comp %>% 
  discard_at(., 'CONTAMINANT_TYPE')

comp <- comp$CONTAMINANT %>% 
  left_join(., comp$CONTAMINANT_PROCESS_SUMMARY, join_by(contaminant_id)) %>% 
  select(-treatment_process_summary)

# comp and ref both share reference_ids, but are not entirely 1:1
# probably some other field that is also pulling from there. 
# Advise against left_join until full relationship is documented.

## Tag ---------------------------------------------------------------------

tag <- js %>% 
  keep(names(.) %in% c(
    'TAG'
  )) %>% 
  pluck(1) %>% 
  split(.$tag_group)

## Treatments ---------------------------------------------------------------
### Treatment details -------------------------------------------------------

{  
  treat <- js %>%
    keep(str_detect(names(js), 'TREATMENT_'))
  
  treat <- treat$TREATMENT_PROCESS_COLUMN %>% 
    left_join(., treat$TREATMENT_PROCESS, join_by(treatment_process_id)) %>% 
    select(
      #  'column_id'
      'treatment_process_id',
      'column_name',
      'column_display_name',
      #  'display_order'
      #  'display_for_input_flag'
      #  'data_type'
      'tag_group',
      #  'conc_treatment_column_name'
      'treatment_process_name',
      'treatment_process_table_name'
      #  'included_treatments' 
    )
} 

# treat_cols <- treat %>% 
#   distinct(column_name, column_display_name)

# max possible columsn to see
treat_spread <- treat %>%
  pivot_wider(.,
              id_cols = c(treatment_process_id, treatment_process_name, treatment_process_table_name),
              names_from = column_name,
              values_from = tag_group)


treat_uq <- treat %>% 
  distinct(., treatment_process_id, treatment_process_name, treatment_process_table_name)


### Processes ---------------------------------------------------------------

{
  proc <- js %>%
    keep(str_detect(names(js), 'CP_')) %>% 
    compact() %>% #removes pressure filter
    discard_at('CP_DETAIL_GAC_ISOTHERM')
   
 
# Fixes the units----
  proc_col_list <- proc %>% 
    map(., colnames) %>% 
    map(., ~str_count(.x, 'units')) %>% 
    map(., ~sum(.x)) %>% 
    discard(., ~.x != 2) %>% 
    names(.)
  
  proc_chem <- proc %>% 
    discard(., names(.) %in% proc_col_list) %>% 
    map(., ~rename(.x, 'contaminant_units' = 'chem_contaminant_units'))
  
  proc_micro <- proc %>% 
    keep(., names(.) %in% proc_col_list) %>% 
    map(., ~pivot_longer(.x, 
                         c(chem_contaminant_units,micr_contaminant_units), 
                         names_to = NULL,
                         values_to = 'contaminant_units', 
                         values_drop_na = T))
  
  proc <- c(proc_chem, proc_micro)
  rm(proc_chem, proc_micro)
  
# keeps the processes that have multiple removal types ----
  proc_col_list <- proc %>% 
    map(., colnames) %>% 
    map(., ~str_count(.x, 'removal_')) %>% 
    map(., ~sum(.x)) %>% 
    discard(., ~.x == 1) %>% 
    names(.)
  
  proc_good <- proc %>% 
    discard(., names(.) %in% proc_col_list)
    
  proc_fix <- proc %>% 
    keep(., names(.) %in% proc_col_list) %>% 
    map(., ~{
      pivot_longer(.x, 
                          c(removal_both, removal_coag, removal_filt),
                          names_to = 'removal_subtype',
                          values_to = 'removal', 
                          values_drop_na = T
                         )
        
      
      })
    
    proc <- c(proc_good, proc_fix)
    rm(proc_good, proc_fix)  

# Fixes inf/eff types----
    proc_col_list <- proc %>% 
      map(., colnames) %>% 
      map(., ~str_count(.x, 'removal_')) %>% 
      map(., ~sum(.x)) %>% 
      discard(., ~.x == 1) %>% 
      names(.)
  
}  

#### Cutdown -----------------------------------------------------------------

  proc_dat <- proc %>% 
    map(., ~select(.x, c(
      detail_id,
      contaminant_process_id,
      reference_id,
      removal,
      removal_type,
      contaminant_units,
      scale,
      water,
      modeled_data
      #contaminant_inf,
      #contaminant_eff
      
    ))) %>% 
  list_rbind() %>% 
  mutate(removal_num = str_remove_all(removal, pattern = '<|>|#|\\*') %>% as.numeric) %>% 
  left_join(., comp, join_by(contaminant_process_id)) %>% 
  left_join(., treat_uq, join_by(treatment_process_id)) %>% 
  split(is.na(.$removal_num)) #splits for the ranged values
  
proc_fix <- proc_dat$`TRUE` %>% 
  filter(str_detect(removal, 'to')) %>% #removes ~37 records that are too complicated to fix
  mutate(removal_dupe = str_remove_all(removal, pattern = '<|>|#|\\*')) %>%
  mutate(removal_dupe = str_remove_all(removal_dupe, pattern = '[[:blank:]]')) %>% 
  separate(., removal_dupe, into = c('low', 'high'), sep = 'to') %>% 
  rowwise() %>% 
  mutate(
    high = as.numeric(high), 
    low = as.numeric(low),
    removal_num = mean(c(low, high))
         ) %>% 
  select(-c(high, low)) %>% 
  ungroup()

proc_dat <- list_rbind(list(proc_dat$`FALSE`, proc_fix))
rm(proc_fix)

# binning -----------------------------------------------------------------

proc_binned <- proc_dat %>% 
  left_join(., select(ref, reference_id, tag_value, tag_desc), join_by(reference_id)) %>% 
  mutate(
    
    removal_type = case_when(
      removal_type == 'percent' ~ 'Percent',
      removal_type == "Percent   " ~ 'Percent',
      is.na(removal_type) == TRUE ~ 'Percent',
      .default = removal_type),
    
    cat = case_when(
      removal_type == 'Percent' & removal_num < 0 ~ 'Conc',
      removal_type == 'Percent' & removal_num >= 0 & removal_num < 10 ~ 'XL',
      removal_type == 'Percent' & removal_num >= 10 & removal_num < 20 ~ 'L',
      removal_type == 'Percent' & removal_num >= 20 & removal_num < 50 ~ 'M',
      removal_type == 'Percent' & removal_num >= 50 & removal_num < 90 ~ 'H',
      removal_type == 'Percent' & removal_num >= 90 & removal_num < 99 ~ 'VH',
      removal_type == 'Percent' & removal_num >= 99 ~ 'XH',
      
      removal_type == 'Log' & removal_num < 1 ~ 'XL',
      removal_type == 'Log' & removal_num >= 1 & removal_num < 2 ~ 'L',
      removal_type == 'Log' & removal_num >= 2 & removal_num < 3 ~ 'M',
      removal_type == 'Log' & removal_num >= 3 & removal_num < 4 ~ 'H',
      removal_type == 'Log' & removal_num >= 4 & removal_num < 5 ~ 'VH',
      removal_type == 'Log' & removal_num >= 5 ~ 'XH',
      removal_type == 'Log' & removal_num > 20 ~ NA),
    
    scale_int = case_when(
      scale == 'B'~ 1,
      scale == 'P'~ 5,
      scale == 'F'~ 10,
      .default = 1
    )
  ) %>% 
  group_by(
    contaminant_name,
    scale_int,
  ) %>% 
  mutate(cat_int = case_when(
    cat == 'Conc' ~ -1,
    cat == 'XL' ~ 0,
    cat == 'L' ~ 1,
    cat == 'M' ~ 2,
    cat == 'H' ~ 3,
    cat == 'VH' ~ 4,
    cat == 'XH' ~ 5,
    .default = NA
  )) %>% 
  filter(!is.na(cat)) %>% 
  group_by(contaminant_name, treatment_process_name, contaminant_type_code) %>% 
  reframe(score = weighted.mean(cat_int, scale_int)) %>%
  mutate(score = floor(score),
         score = case_when(
           score == -1 ~ 'CONC',
           score == 0 ~ 'XL',
           score == 1 ~ 'L',
           score == 2 ~ 'M',
           score == 3 ~ 'H',
           score == 4 ~ 'VH',
           score == 5 ~ 'XH',
           is.na(score) == TRUE ~ 'ND'
          )
  ) %>% 
  ungroup() %>% 
  pivot_wider(., 
              names_from = treatment_process_name,
              values_from = score,
              values_fill = 'ND')


rio::export(proc_binned, file = 'tdb_binned.xlsx')

proc_export <- proc_binned %>%
  DT::datatable(., 
  extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )) %>% 
  formatStyle(names(proc_binned), textAlign = "center") %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("XH", "#11C638")) %>% 
  formatStyle(names(proc_binned), backgroundColor = styleEqual("VH", "#77D17F")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("H", "#B0DAB3")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("M", "lightyellow")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("L", "#EDC9B0")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("VL", "#F1B077")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("XL", "#EF9708")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("CONC","black")) %>%
  formatStyle(names(proc_binned), color = styleEqual("CONC","white")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("I", "slategrey")) %>%
  formatStyle(names(proc_binned), backgroundColor = styleEqual("ND", "grey"))


# testing on 1,4-d --------------------------------------------------------

#cutdown for a single compound

# comp_binned <- proc_binned %>%
#  filter(contaminant_name == '1,4-dioxane') %>%
#   pivot_longer(cols = !c(contaminant_name,
#                          contaminant_type_code),
#                names_to = 'treatment') %>%
#   arrange(value = forcats::fct_relevel(value, c('XH', 'VH', 'H', 'M', 'L','XL','CONC', 'ND')))

# comp_treat <- comp %>%
#   left_join(., treat_uq, join_by(treatment_process_id)) %>%
#   select(., c(
#     contaminant_id:contaminant_type_code,
#     dtxsid,
#     contaminant_process_id:treatment_process_id,
#     treatment_process_name:treatment_process_table_name
#   )) %>%
#   filter(!is.na(treatment_process_name)) %>%
#   split(., ~str_detect(contaminant_type_code, pattern = 'C|R')) %>%
#   map(., ~pivot_wider(.,
#                       id_cols = c(contaminant_id, contaminant_name),
#                       names_from = treatment_process_table_name,
#                       values_from = contaminant_process_id
#                       ))

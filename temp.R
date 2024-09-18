{
  cx <- v8()
  
  cx$source("https://cfpub.epa.gov/wqsits/wqcsearch/data/criteria_json_5a.js")
  
  vars <- cx$eval(
    "
  Object.keys(this).filter(function(key) {
    return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
  })
"
  ) %>%
    str_split(., ",") %>%
    pluck(., 1)
  
  parent_dat <- vars %>%
    map(., ~ {
      cx$get(.x)
    }) %>%
    set_names(., vars)
  
  state_vars <- parent_dat$entities %>%
    map(., ~ as_tibble(.) %>%
          t(.) %>%
          as_tibble()) %>%
    list_rbind() %>%
    set_names(., c("area", "region", "abv", "cat", "file", "coverage")) %>%
    mutate(
      across(everything(), ~ na_if(., "")),
      json = paste0(
        "https://cfpub.epa.gov/wqsits/wqcsearch/data/stateJson_",
        abv,
        ".js"
      ),
      idx = 1:n()
    )
  
  state_extra <- state_vars %>%
    select(area, region, cat, file, coverage, idx, abv)
  
  state_vars %<>%
    select(abv, json)
  
  rm(vars)
}

{
  state_dat <- state_vars %>%
    pmap(., function(abv, json) {
      message(abv, "\n")
      ctx <- v8()
      ctx$source(json)
      
      st_vars <- ctx$eval(
        "
  Object.keys(this).filter(function(key) {
    return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
  })
"
      ) %>%
        str_split(., ",") %>%
        pluck(., 1)
      
      dat <- st_vars %>%
        map(., ~ {
          ctx$get(.x)
        }) %>%
        set_names(., st_vars) %>%
        modify_at(., "criteriaData_sub", ~ pluck(., 1)) %>%
        compact(.)
      
      if (length(dat) == 0) {
        dat <- NULL
      } else {
        dat$desc_use_class_sub %<>%
          flatten(.) %>%
          enframe(., name = "key", value = "local") %>%
          unnest(., "local")
        
        new_names <- c(
          "analyte" = "analyte",
          "result" = "V1",
          "unit" = "V2",
          "protection" = "V3",
          "use_class" = "V4",
          "source" = "V5",
          "page_source" = "V6"
        )
        
        dat$criteriaData_sub %<>%
          # pluck(., 1) %>%
          map_if(., is.list, ~ {
            map(., ~ {
              t(.x) %>%
                as_tibble(.)
            }) %>%
              list_rbind(.)
          }) %>%
          map_if(., is.character, ~ {
            as_tibble(.x) %>%
              mutate(across(everything(), as.character))
          }) %>%
          list_rbind(., names_to = "analyte") %>%
          rename(!!!new_names)
      }
      rm(ctx)
      return(dat)
    }, .progress = T) %>%
    set_names(., state_vars$abv) %>%
    compact(.)
  
  
  sources <- state_dat %>%
    map(., ~ {
      pluck(.x, "sourcedoc_sub") %>%
        flatten(.) %>%
        enframe(., name = "key", value = "link")
    }) %>%
    list_rbind(names_to = "area") %>%
    unnest("link")
  
  use_class <- state_dat %>%
    map(., ~ pluck(., "desc_use_class_sub")) %>%
    list_rbind(names_to = "area")
  
  crit_dat <- state_dat %>%
    map(., ~ pluck(., "criteriaData_sub")) %>%
    list_rbind(names_to = "area")
  
  rm(state_dat)
}
library(stringr)

# Sample string
text <- "The price is $45.99 and the quantity is 23 units."

# Extract numbers using str_extract_all
numbers <- str_extract_all(text, "\\d+(?:\\.\\d+)?")

# Convert to numeric
numeric_result <- as.numeric(unlist(numbers))

print(numeric_result)
#negative
numbers <- str_extract_all(text, "-?\\d+(?:\\.\\d+)?")
#sci
numbers <- str_extract_all(text, "-?\\d+(?:\\.\\d+)?(?:[eE][-+]?\\d+)?")
#thousands sep
numbers <- str_extract_all(text, "\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?")

numeric_result <- as.numeric(gsub(",", "", unlist(numbers)))


#   -----------------------------------------------------------------------

nwqs <- import(here('final', 'nwqs.RDS')) %>%
  select(preferredName) %>%
  distinct() %>%
  mutate(raw_source = str_to_upper(preferredName))

r1 <- raw_pol_srs %>%
  mutate(raw_target = str_to_upper(analyte))

h1 <- similar_hash(
  target = r1,
  target_by = 'raw_target',
  dict = nwqs,
  dict_by = 'raw_source'
)

hash_1 <-
  keep(h1, names(h1) %in% c("cosine", "jw")) %>%
  list_rbind(names_to = "method") %>%
  filter(
    method == 'cosine' &
      dist <= quantile(.[.$method == 'cosine', ][['dist']], 0.015)
  )


job::job({
  library(rgbif)

  sp_dat <- sp$latin %>%
    map(
      .,
      ~ {
        name_backbone(name = .x) %>%
          as_tibble()
      },
      .progress = TRUE
    ) %>%
    set_names(sp$latin) %>%
    list_rbind(names_to = 'raw_search') %>%
    filter(!is.na(usageKey)) %>%
    inner_join(sp, ., join_by(latin == raw_search))

  hab_dat_raw <- map(
    sp_dat$usageKey,
    ~ name_usage(key = .x, data = 'speciesProfiles'),
    .progress = TRUE
  ) %>%
    set_names(sp_dat$usageKey)

  hab_dat <- hab_dat_raw %>%
    map(
      .,
      ~ {
        pluck(., 'data')
      }
    ) %>%
    compact() %>%
    list_rbind(names_to = 'usageKey') %>%
    select(
      usageKey,
      marine,
      freshwater
      #choosing to not use this as I don't need it?
      #, terrestrial
      #choosing to not use this since it is too freeform
      #,habitat
    ) %>%
    pivot_longer(
      .,
      cols = c(marine, freshwater),
      values_to = 'hab1',
      values_drop_na = TRUE
    ) %>%
    distinct() %>%
    arrange(hab1) %>%
    distinct(usageKey, name, .keep_all = TRUE) %>%
    pivot_wider(names_from = name, values_from = hab1, values_fill = FALSE) %>%
    mutate(usageKey = as.integer(usageKey)) %>%
    inner_join(
      sp_dat %>%
        select(
          latin,
          common,
          usageKey,
          canonicalName
        ),
      .,
      join_by(usageKey)
    ) %>%
    distinct() %>%
    mutate(
      hab_chk = case_when(
        freshwater == TRUE & marine == TRUE ~ TRUE,
        .default = FALSE
      )
    )
})


qq <- c('DTXSID7020267','DTXSID5023954')
q1 <- ct_related(qq)

q1 %>% 
	filter(query %in% qq & dtxsid	%in% qq)


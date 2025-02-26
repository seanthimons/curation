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


hash <-
  map(
    c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"),
    ~ {stringdist_join(params_hash, chars,
                   by = c('raw_search' = 'name'),
                   mode = 'right',
                   method = .x,
                   #max_dist = 9,
                   distance_col = 'dist') %>%
      group_by(analyte) %>%
      filter(!is.na(dist)) %>%
      slice_min(order_by = dist, n = 5)
      },
    .progress = T) %>%
  set_names(., c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw"))

#TODO Repeat this as each method for comparison...?
{
hash_1 <-
  keep(hash, names(hash) %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram")) %>%
  list_rbind(names_to = 'method') %>%
  filter(!is.na(srs_id)) %>%
  distinct(analyte, name, cas_number, srs_id, .keep_all = T) %>%
  select(-c(raw_search, method, dist)) %>%
  mutate(srs_id = as.character(srs_id))

hash_2 <-
  keep(hash, names(hash) %in% c("cosine", "jaccard", "jw")) %>%
  list_rbind(names_to = 'method') %>%
  group_by(analyte) %>%
  slice_min(dist, n = 1) %>%
  filter(!(analyte %in% hash_1$analyte)) %>%
  distinct(analyte, name, cas_number, srs_id, .keep_all = T) %>%
  filter(dist <= 0.1) %>%
  select(-c(raw_search, method, dist)) %>%
  ungroup() %>%
  mutate(srs_id = as.character(srs_id))

#NOTE Unlikely to produce any good results


{
  hash_3 <-
    keep(hash, names(hash) %in% c("cosine", "jaccard", "jw")) %>%
    list_rbind(names_to = 'method') %>%
    filter(!c(analyte %in% c(hash_1$analyte, hash_2$analyte))) %>%
    group_by(analyte) %>%
    slice_min(dist, n = 1) %>%
    distinct(analyte, raw_search)

  hash_4 <- map(hash_3$raw_search, ~srs_search(query = .x, method = 'begins'), .progress = T) %>%
    set_names(., hash_3$analyte) %>%
    list_rbind(names_to = 'raw_search') %>%
    select(raw_search, itn)

  hash_4 <- map(hash_4$itn, ~srs_details(query = .x), .progress = T) %>%
    set_names(., hash_4$raw_search) %>%
    list_rbind(names_to = 'raw_search') %>%
    select(raw_search, systematicName, internalTrackingNumber) %>%
    rename(
      name = systematicName,
      srs_id = internalTrackingNumber
    ) %>%
    mutate(name = str_to_title(name))

  hash_3 <- hash_3 %>%
    left_join(., hash_4, join_by(raw_search)) %>%
    select(-raw_search) %>%
    ungroup() %>%
    mutate(srs_id = as.character(srs_id))

  rm(hash_4)
  }

  params <- list_rbind(list(hash_1, hash_2, hash_3)) %>%
    select(-cas_number)

  rm(hash_1, hash_2, hash_3, hash, chars)
  }
}

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
  dict_by = 'raw_source')

hash_1 <-
      keep(h1, names(h1) %in% c("cosine", "jw")) %>%
      list_rbind(names_to = "method") %>% 
      filter(
        method == 'cosine' & dist <= quantile(.[.$method == 'cosine', ][['dist']], 0.015))

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

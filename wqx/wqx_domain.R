dat <- readr::read_csv(file = here('wqx', 'raw', 'Organization.csv'))

raw_text <- readr::read_file(here('wqx', 'raw', 'Analytical Method.csv'))

# This regex finds a quote, followed by any characters that are not a quote,
# and replaces any newline characters within that matched string.
# The `perl = TRUE` is necessary for the `\K` (keep) and lookaheads.
cleaned_text <- gsub('"[^"]*\\K\\n', "", raw_text, perl = TRUE)
dat <- readr::read_csv(cleaned_text, show_col_types = FALSE, progress = FALSE)

problems(dat)

wqx_table('Characteristic') %>% glimpse()
wqx_table('Characteristic Alias') %>% glimpse()

wqx_table('Characteristic') %>%
  filter(str_detect(Name, 'Oil and Grease')) %>%
  select(Name, `SRS ID`, `Comparable Name`)

wqx_table('Characteristic Alias') %>%
  filter('Characteristic Name' == 'Oil and grease')

dbDisconnect(con, shutdown = TRUE)

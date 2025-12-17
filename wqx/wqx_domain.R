
dat <- readr::read_csv(file = here('wqx', 'raw', 'Organization.csv'))
problems(dat)

wqx_table('Characteristic') %>% glimpse()
wqx_table('Characteristic Alias') %>% glimpse()

wqx_table('Characteristic') %>%
	filter(str_detect(Name, 'Oil and Grease')) %>%
	select(Name, `SRS ID`, `Comparable Name`)

wqx_table('Characteristic Alias') %>%
	filter('Characteristic Name' == 'Oil and grease')

dbDisconnect(con, shutdown = TRUE)

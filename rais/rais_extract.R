# Packages -------------------------------------------------------------

{
  install_booster_pack <- function(package, load = TRUE) {
    # Loop through each package
    for (pkg in package) {
      # Check if the package is installed
      if (!requireNamespace(pkg, quietly = TRUE)) {
        # If not installed, install the package
        install.packages(pkg)
      }
      # Load the package
      if (load) {
        library(pkg, character.only = TRUE)
      }
    }
  }

  if (file.exists('packages.txt')) {
    packages <- read.table('packages.txt')

    install_booster_pack(package = packages$Package, load = FALSE)

    rm(packages)
  } else {
    # Packages ----

    booster_pack <- c(
      ## IO ----
      'fs',
      'here',
      'janitor',
      'rio',
      'tidyverse',
      #	'data.table',
      'mirai',
      # 'targets',
      # 'crew',

      ## DB ----
      #  'arrow',
      'nanoparquet',
      #  'duckdb',
      #  'duckplyr',
      #  'dbplyr',

      ## EDA ----
      'skimr',

      ## Web ----
      'rvest',
      'polite',
      #	'plumber',
      # 'plumber2', #Still experimental
      'httr2',

      ## Plot ----
      # 'paletteer',
      # 'ragg',
      # 'camcorder',
      # 'esquisse',
      # 'geofacet',
      # 'patchwork',
      # 'marquee',
      # 'ggiraph',
      # 'geomtextpath',
      # 'ggpattern',
      # 'ggbump',
      # 'gghighlight',
      # 'ggdist',
      # 'ggforce',
      # 'gghalves',
      # 'ggtext',
      # 'ggrepel', # Suggested for non-overlapping labels
      # 'gganimate', # Suggested for animations
      # 'ggsignif',
      # 'ggTimeSeries',
      # 'tidyheatmaps',

      ## Modeling ----
      # 'tidymodels',

      ## Shiny ----
      # 'shiny',
      # 'bslib',
      # 'DT',
      # 'plotly',

      ## Reporting ----
      # 'quarto',
      # 'gt',

      ## Spatial ----
      # 'sf',
      # 'geoarrow',
      # 'duckdbfs',
      # 'duckspatial',
      # 'ducksf',
      # 'tidycensus', # Needs API
      # 'mapgl',
      # 'dataRetrieval', # Needs API
      # 'StreamCatTools',

      ## Misc ----
      # 'devtools',
      # 'usethis',
      # 'pak',
      'remotes'
    )

    # ! Change load flag to load packages
    install_booster_pack(package = booster_pack, load = TRUE)
    rm(install_booster_pack, booster_pack)
  }

  # Custom Functions ----

  `%ni%` <- Negate(`%in%`)

  # skim_count <- skim_with(
  # 	numeric = sfl(
  # 		n = length,
  # 		min = ~ min(.x, na.rm = T),
  # 		median = ~ median(.x, na.rm = T),
  # 		max = ~ max(.x, na.rm = T)
  # 	)
  # )

  # Camcorder ----

  # if(!dir.exists(here::here('output'))) {
  #   dir.create(here::here('output'))
  # }

  # gg_record(
  # 	here::here('output'),
  # 	device = "png",
  # 	width = 10,
  # 	height = 7,
  # 	units = "in",
  # 	dpi = 320
  # )

  # Theme ----

  # theme_custom <- function() {
  # 	theme_minimal() +
  # 		theme(
  # 			plot.background = element_rect(colour = "white"),
  # 			panel.grid.major = element_blank(),
  # 			panel.grid.minor = element_blank(),
  # 			strip.background = element_rect(colour = "white"),
  # 			axis.text.x = element_text(angle = 90L)
  # 		)
  # }
	

	setwd(here('rais'))
}

# Checkpoint -------------------------------------------------------------

# Determine if a rebuild is necessary.
# A rebuild is needed if any file is missing, or if any existing file is older than 90 days.
{
	files_to_check <- c(
		'rais_eco_arars.RDS',
		'rais_benchmarks_chemicals.RDS',
		'rais_benchmarks_rads.RDS',
		'rais_chemtox.RDS',
		'rais_radstox.RDS',
		'gen_bgval.RDS'
	)

	files_exist_check <- file.exists(files_to_check)

	rebuild_is_needed <- if (!all(files_exist_check)) {
		cli::cli_alert_info(
			"One or more data files are missing. Rebuilding dataset."
		)
		tibble(files_to_check, files_exist_check) %>% print()
		TRUE
	} else {
		# Using mtime (modification time) is more robust for checking data freshness.
		file_ages_days <- difftime(
			Sys.time(),
			file.info(files_to_check)$mtime,
			units = "days"
		)
		if (any(file_ages_days > 90)) {
			cli::cli_alert_warning(
				"One or more data files are older than 90 days. Rebuilding dataset."
			)
			tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
			TRUE
		} else {
			cli::cli_alert_success(
				"All data files are present and up-to-date. Skipping rebuild."
			)
			tibble(files_to_check, files_exist_check, file_ages_days) %>% print()
			rm(files_to_check, files_exist_check, file_ages_days)
			FALSE
		}
	}
}


# Rebuild ----------------------------------------------------------------

# If a rebuild is needed, run the data scraping and processing sections.
if (rebuild_is_needed) {

	# Established Regulatory Limits for Surface Water and Groundwater ---------
	## ARARs; Applicable or Relevant and Appropriate Requirements ----

	{
		#list options

		webpage <- read_html("https://rais.ornl.gov/tools/arar_search.php")

		# #gets the url
		# html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>%
		#   html_attrs()
		#
		# #gets the displayed value
		# html_elements(webpage, xpath = '//*[@id="state_options-0"]/option') %>%
		#   html_text()

		choices <- tibble(
			state = html_elements(
				webpage,
				xpath = '//*[@id="state_options-0"]/option'
			) %>%
				html_text(),
			url = html_elements(
				webpage,
				xpath = '//*[@id="state_options-0"]/option'
			) %>%
				html_attrs() %>%
				unlist() %>%
				unname()
		) %>%
			mutate(
				abv = str_remove_all(url, '&action=next'),
				abv = str_remove_all(abv, '.*='),
				caps = str_to_upper(abv)
			) %>%
			.[-1, ] #removes empty choice by position

		rm(webpage)

		{
			cli::cli_alert_info('Begining ARAS search...')
			cli::cat_line()
		}

		arars <- pmap(choices, function(state, url, abv, caps) {
			#  })
			# ({
			#   choices %>% map(., 14) %>% list2env(., .GlobalEnv)
			#
			cli::cli_alert(state)
			cli::cat_line()

			webpage <- read_html(paste0("https://rais.ornl.gov", url))

			fields <- tibble(
				name = html_elements(
					webpage,
					xpath = '//*[@id="dualselect"]/div/fieldset'
				) %>%
					html_attrs() %>%
					unlist() %>%
					unname(),
				value = html_elements(
					webpage,
					xpath = '//*[@id="dualselect"]/div/fieldset/legend'
				) %>%
					html_text()
			) %>%
				mutate(
					value = str_remove_all(value, pattern = "Select ") %>%
						str_remove_all(., pattern = '/|,|\\(|\\)') %>%
						str_replace_all(., pattern = " ", replacement = "_")
				) %>%
				head(., -1)

			field_list <- map(fields$value, function(value) {
				#value <- fields$value[2]
				opt <- paste0('//*[@id="arars', value, '-0"]/option')

				sam_pay <- list("Yes")
				sam_pay <- set_names(sam_pay, paste0("sam_", value))

				df <- list(
					name = html_elements(webpage, xpath = opt) %>%
						html_text(),
					val = html_elements(webpage, xpath = opt) %>%
						html_attrs() %>%
						unlist() %>%
						unname(),
					p_list = NA
				)

				dim <- length(df$val)

				df$p_list <- rep(paste0("arars", value, "[]"), length(df$val))

				df$p_list <- setNames(df$val, df$p_list) %>% as.list()

				df$p_list <- c(df$p_list, sam_pay)

				df$val <- NULL

				return(df)
			}) %>%
				set_names(., fields$value)

			group_list <- map(field_list, ~ pluck(., 'name') %>% as_tibble) %>%
				list_rbind(names_to = 'group') %>%
				mutate(group = str_replace_all(group, pattern = '_', replacement = ' '))

			table_col_names <- map(field_list, ~ pluck(., 'name')) %>%
				unlist() %>%
				unname() %>%
				append(., c('Analyte', 'CAS Number'), after = 0L)

			{
				chems <- tibble(
					name = html_elements(
						webpage,
						xpath = '//*[@id="analysis-0-from"]/option'
					) %>%
						html_text(),
					value = html_elements(
						webpage,
						xpath = '//*[@id="analysis-0-from"]/option'
					) %>%
						html_attrs() %>%
						unlist() %>%
						unname()
				)
				chems <- chems$value %>%
					map(
						.,
						~ list(
							"analysis[]" = .x
						)
					) %>%
					flatten()

				p1 <- list(
					"_qf__dualselect" = "",
					"action" = "Retrieve",
					"table_name" = "",
					"state" = abv
				)

				p2 <- map(field_list, ~ pluck(., "p_list")) %>% flatten()

				p3 <- list(
					"sam_Chem" = "Yes",
					"testSubmit" = "Retrieve"
				)

				payload <- c(p1, p2, chems, p3)

				rm(p1, p2, chems, p3, fields)
			}

			req <- POST(
				url = "https://rais.ornl.gov/tools/arar_search.php",
				body = payload
				#,progress()
			)

			response <-
				content(req, as = "parsed")

			table_headers <- html_elements(
				response,
				xpath = '//*[@id="arars"]/thead/tr/th'
			) %>%
				html_text2() %>%
				str_split(., pattern = "\t") %>%
				str_split(., pattern = "\n") %>%
				map(., ~ str_remove_all(., ",|[[:digit:]]")) %>%
				map(., ~ str_remove_all(., "NA|NANA"))

			units_list <- "ug/l|ug/L|pCi/L|mg/l|mg/L|mg/kg"

			table_ft <- html_elements(
				response,
				xpath = '//*[@id="arars"]/thead/tr/th'
			) %>%
				html_text2() %>%
				str_split(., pattern = "\t") %>%
				str_split(., pattern = "\n") %>%
				map(., ~ str_remove_all(., "NA|NANA")) %>%
				map(., ~ str_remove_all(., units_list))

			tb_ft <- tibble(
				col = map(table_ft, ~ pluck(., 1)) %>% list_simplify(),
				ft = map(table_ft, ~ pluck(., 2, .default = NA)) %>% list_simplify()
			) %>%
				separate_longer_delim(., ft, delim = ",") %>%
				mutate(
					#TODO follow up on where coercing to NA is happening
					ft = as.numeric(ft),
					ft = as.character(ft)
				) %>%
				filter(!is.na(ft)) %>%
				filter(ft != "")

			rm(table_ft)

			table_units <- tibble(
				col = map(table_headers, ~ pluck(., 1)) %>%
					list_simplify(),
				units = map(table_headers, ~ pluck(., 2, .default = NA)) %>%
					list_simplify() %>%
					str_extract_all(., pattern = units_list)
			)

			table_units$col <- table_col_names

			rm(table_headers)

			dat_table <- html_elements(response, xpath = '//*[@id="arars"]') %>%
				html_table(., trim = T, na.strings = "") %>%
				pluck(., 1) %>%
				set_names(., table_units$col) %>%
				as_tibble(., .name_repair = "unique") %>%
				mutate(across(everything(), as.character)) %>%
				pivot_longer(
					.,
					cols = !matches("Analyte|CAS Number"),
					names_to = "source",
					values_to = "result",
					values_drop_na = T
				) %>%
				mutate(
					orig_result = result,
					ft = case_when(
						source != "Federal Water Quality Criteria Source" ~
							str_extract(result, pattern = "(?<=\\s)\\S+"),
						.default = NA
					),
					result = case_when(
						source != "Federal Water Quality Criteria Source" ~
							str_extract(result, pattern = "^\\S+")
					)
				) %>%
				separate_longer_delim(., ft, delim = ",")

			ft_page <- paste0(
				"https://rais.ornl.gov/tools/includes/guide_",
				caps,
				"foot.html"
			)

			ft_text <- read_html(ft_page) %>%
				html_elements(
					.,
					xpath = '//*[@id="page-wrapper"]/section[2]/div/div/p'
				) %>%
				html_text() %>%
				str_replace_all(., pattern = "\\r\\n", " ") %>%
				as_tibble() %>%
				mutate(
					ft = str_extract(value, pattern = "\\d{1,3}\\.") %>%
						str_remove_all(., pattern = "\\."),
					value = str_extract(value, pattern = "(?<=\\d{1,3}\\.).*") %>%
						str_squish()
				)

			header_footnotes <- left_join(tb_ft, ft_text, join_by(ft)) %>%
				select(-ft) %>%
				distinct() %>%
				group_by(col) %>%
				mutate(ft_num = paste0("ft", row_number())) %>%
				ungroup() %>%
				pivot_wider(
					.,
					id_cols = c(col),
					values_from = "value",
					names_from = "ft_num"
				) %>%
				unite(., "header_ft", contains("ft"), sep = " ", remove = T, na.rm = T)

			dat <- left_join(dat_table, header_footnotes, join_by(source == col)) %>%
				left_join(., table_units, join_by(source == col)) %>%
				#TODO follow up on where many-to-many relationships are happening; row 83 x-to-y, row 25 y-to-x
				left_join(., ft_text, join_by(ft)) %>%
				left_join(., group_list, join_by(source == value)) %>%
				filter(!is.na(result)) %>%
				select(-ft) %>%
				clean_names() %>%
				unite(
					.,
					"ft",
					c(header_ft, value),
					na.rm = T,
					remove = T,
					sep = " "
				) %>%
				rowwise() %>%
				mutate(units = paste(units)) %>%
				ungroup()

			return(dat)
		}) %>%
			set_names(., choices$state) %>%
			list_rbind(., names_to = 'state') %>%
			rename(
				preferredName = analyte,
				cit = source,
				unit_name = units,
				value = result,
				meta = ft
			)

		# ! TODO Stopped here to re-eval direction.
		arars_cur <- arars %>%
			mutate(
				value = str_remove_all(value, pattern = ',')
			)

		write_rds(arars, 'rais_eco_arars.RDS')
		rm(list = ls())
	}

	# Ecological Benchmark Tool for Chemicals ---------------------------------

	{
		# Read the HTML content of the website
		webpage <- read_html(
			"https://rais.ornl.gov/tools/eco_search.php?select=chem"
		)

		chems <- list(
			# val = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
			#   html_text(),
			dat = html_elements(
				webpage,
				xpath = '//*[@id="analysis-0-from"]/option'
			) %>%
				html_attrs() %>%
				unlist() %>%
				unname()
		) %>%
			flatten()

		sources <- list(
			val = html_elements(webpage, xpath = '//*[@id="sources-0"]/option') %>%
				html_text()
		) %>%
			flatten()

		media <- list(
			val = html_elements(webpage, xpath = '//*[@id="media-0"]/option') %>%
				html_text()
		) %>%
			flatten()

		rm(webpage)

		{
			p1 <- list(
				'_qf__dualselect' = "",
				'select' = 'chem',
				'tname' = 'ECO_BENCH_LONG',
				'action' = 'Retrieve'
			)

			media_payload <-
				map(
					media,
					~ list(
						'media[]' = .x
					)
				) %>%
				flatten() %>%
				append(., c('sam' = 'Yes'))

			sources_payload <-
				map(
					sources,
					~ list(
						'sources[]' = .x
					)
				) %>%
				flatten() %>%
				append(., c('sabs' = 'Yes'))

			payload <- c(p1, sources_payload, media_payload)
		}

		chems_payload <- map(
			chems,
			~ list(
				'analysis[]' = .x
			)
		) %>%
			flatten() %>%
			split(
				.,
				rep(1:ceiling(length(.) / 200), each = 200, length.out = length(.))
			) %>%
			map(., ~ append(., c('Submit' = 'Yes')))

		{
			cli::cli_alert_info('Ecological Benchmark Tool for Chemicals')
			cli::cat_line()
		}

		rais_tbl <- map(
			chems_payload,
			~ {
				payload_map <- append(payload, .x) %>% flatten()

				req <- POST(
					url = 'https://rais.ornl.gov/tools/eco_search.php',
					body = payload_map,
					progress() # progress bar
				)

				response <-
					content(req, as = 'parsed')

				response_ft <<- response

				tables <- map(
					media,
					~ {
						.x <- str_remove_all(.x, pattern = ' ')
						#message(.x)

						opt = paste0('//*[@id="', .x, '"]')

						html_elements(response, xpath = opt) %>%
							html_table() %>%
							pluck(., 1, .default = tibble()) %>%
							mutate(across(everything(), as.character))
					},
					.progress = T
				) %>%
					set_names(., media) %>%
					compact() %>%
					list_rbind(., names_to = 'media')

				return(tables)
			}
		) %>%
			list_rbind() %>%
			clean_names()

		rais_ft <- tibble(
			val = html_elements(
				response_ft,
				xpath = '//*[@id="page-wrapper"]/section[2]/div/ol/li'
			) %>%
				html_text()
		) %>%
			mutate(num = 1:n() %>% as.character)

		rm(response_ft)

		eco_bench <- rais_tbl %>%
			mutate(
				footnotes = na_if(footnotes, ""), # Changed column name scheme; now footnotes but has empty strings.
				idx = 1:n() # Creates a unique index for each row.
			) %>%
			separate_longer_delim(., cols = footnotes, delim = ',') %>%
			left_join(., rais_ft, join_by(footnotes == num)) %>%
			rename(., ft = val) %>%
			select(
				-c(
					footnotes
					#	, ft_num
				)
			) %>%
			group_by(idx) %>%
			mutate(ft_num = paste0("ft", row_number())) %>%
			ungroup() %>%
			pivot_wider(., values_from = "ft", names_from = "ft_num") %>%
			unite(., "ft", contains("ft"), sep = " ", remove = T, na.rm = T) %>%
			select(-idx) %>%
			rename(
				preferredName = analyte,
				cit = benchmark,
				local = organism,
				unit_name = units,
				criterion_value = value,
				meta = ft
			)

		write_rds(eco_bench, 'rais_benchmarks_chemicals.RDS')

		rm(list = ls()[str_detect(ls(), pattern = 'eco_bench', negate = T)])
	}

	# Ecological Benchmark Tool for Radionuclides -----------------------------

	{
		# Read the HTML content of the website
		webpage <- read_html(
			"https://rais.ornl.gov/tools/eco_search.php?select=rad"
		)

		chems <- list(
			# val = html_elements(webpage, xpath = '//*[@id="analysis-0-from"]/option') %>%
			#   html_text(),
			dat = html_elements(
				webpage,
				xpath = '//*[@id="analysis-0-from"]/option'
			) %>%
				html_attrs() %>%
				unlist() %>%
				unname()
		) %>%
			flatten()

		sources <- list(
			val = html_elements(webpage, xpath = '//*[@id="sources-0"]/option') %>%
				html_text()
		) %>%
			flatten()

		media <- list(
			val = html_elements(webpage, xpath = '//*[@id="media-0"]/option') %>%
				html_text()
		) %>%
			flatten()

		rm(webpage)

		{
			p1 <- list(
				'_qf__dualselect' = "",
				'select' = 'rad',
				'tname' = 'ECO_BENCH_LONG_RAD',
				'action' = 'Retrieve'
			)

			media_payload <-
				map(
					media,
					~ list(
						'media[]' = .x
					)
				) %>%
				flatten() %>%
				append(., c('sam' = 'Yes'))

			sources_payload <-
				map(
					sources,
					~ list(
						'sources[]' = .x
					)
				) %>%
				flatten() %>%
				append(., c('sabs' = 'Yes'))

			payload <- c(p1, sources_payload, media_payload)
		}

		chems_payload <- map(
			chems,
			~ list(
				'analysis[]' = .x
			)
		) %>%
			flatten() %>%
			split(
				.,
				rep(1:ceiling(length(.) / 200), each = 200, length.out = length(.))
			) %>%
			map(., ~ append(., c('Submit' = 'Yes')))

		{
			cli::cli_alert_info('Ecological Benchmark Tool for Radionuclides')
			cli::cat_line()
		}

		rais_tbl <- map(
			chems_payload,
			~ {
				payload_map <- append(payload, .x) %>% flatten()

				req <- POST(
					url = 'https://rais.ornl.gov/tools/eco_search.php',
					body = payload_map,
					progress() # progress bar
				)

				response <-
					content(req, as = 'parsed')

				response_ft <<- response

				tables <- map(
					media,
					~ {
						.x <- str_remove_all(.x, pattern = ' ')
						#message(.x)

						opt = paste0('//*[@id="', .x, '"]')

						html_elements(response, xpath = opt) %>%
							html_table() %>%
							pluck(., 1, .default = tibble()) %>%
							mutate(across(everything(), as.character))
					},
					.progress = T
				) %>%
					set_names(., media) %>%
					compact() %>%
					list_rbind(., names_to = 'media')

				return(tables)
			}
		) %>%
			list_rbind() %>%
			clean_names()

		rais_ft <- tibble(
			val = html_elements(
				response_ft,
				xpath = '//*[@id="page-wrapper"]/section[2]/div/ol/li'
			) %>%
				html_text()
		) %>%
			mutate(num = 1:n() %>% as.character)

		rm(response_ft)

		rads_bench <- rais_tbl %>%
			mutate(
				footnotes = na_if(footnotes, ""), # Changed column name scheme; now footnotes but has empty strings.
				idx = 1:n() # Creates a unique index for each row.
			) %>%
			separate_longer_delim(., cols = footnotes, delim = ',') %>%
			left_join(., rais_ft, join_by(footnotes == num)) %>%
			rename(., ft = val) %>%
			select(
				-c(
					footnotes
					#	, ft_num
				)
			) %>%
			group_by(idx) %>%
			mutate(ft_num = paste0("ft", row_number())) %>%
			ungroup() %>%
			pivot_wider(., values_from = "ft", names_from = "ft_num") %>%
			unite(., "ft", contains("ft"), sep = " ", remove = T, na.rm = T) %>%
			select(-idx) %>%
			rename(
				preferredName = radionuclide,
				cit = benchmark,
				local = organism,
				unit_name = units,
				criterion_value = value,
				meta = ft
			)

		write_rds(rads_bench, 'rais_benchmarks_rads.RDS')

		rm(list = ls())
	}

	# Toxicity Values and Physical Parameters ---------------------------------
	{
		## Chemical Specific Parameters --------------------------------------------
		{
			webpage <- read_html(
				"https://rais.ornl.gov/cgi-bin/tools/TOX_search?select=chemtox"
			)

			chems <- list(
				dat = html_elements(
					webpage,
					xpath = '//*[@id="analysis_all"]/option'
				) %>%
					html_attrs() %>%
					unlist() %>%
					unname()
			) %>%
				flatten()

			endpoints <- list(
				val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
					html_text(., trim = T)
			) %>%
				flatten()

			short_code <- map(
				endpoints,
				~ {
					str_extract(.x, "\\(([^)]*)\\)") %>%
						str_remove_all("[()]") %>%
						str_trim()
				}
			) %>%
				unlist()

			c_names <- map(
				endpoints,
				~ {
					str_split_fixed(.x, pattern = "\\(", n = 2) %>%
						.[, 1] %>%
						str_trim()
				}
			) %>%
				unlist()

			units <- map(
				endpoints,
				~ {
					str_extract(.x, "\\([^()]*\\)(?=[^()]*$)") %>%
						str_remove_all("[()]") %>%
						str_trim()
				}
			) %>%
				unlist()

			options <- list(
				val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
					html_attrs() %>%
					unlist() %>%
					unname()
			) %>%
				flatten()

			unit_table <- tibble(name = c_names, units = units)

			rm(webpage, endpoints)

			{
				p1 <- list(
					"_submitted_form" = 1,
					"action" = "next",
					'select' = 'chemtox',
					'selectall' = 'no',
					'table' = 'raistox'
				)

				ops <- map(
					options,
					~ list(
						'fields' = .x
					)
				) %>%
					flatten()

				p2 <- list(
					'alloptions' = 'all',
					'_submit' = 'Retrieve'
				)

				chems_payload <- map(
					chems,
					~ list(
						'analysis' = .x
					)
				) %>%
					flatten() %>%
					split(
						.,
						rep(1:ceiling(length(.) / 200), each = 200, length.out = length(.))
					)
			}

			{
				cli::cli_alert_info(
					'Toxicity Values and Physical Parameters for Chemicals'
				)
				cli::cat_line()
			}

			chemtox <- imap(
				chems_payload,
				~ {
					cli_alert_info(.y)

					payload_map <- c(p1, .x, ops, p2) %>% flatten()

					req <- POST(
						url = 'https://rais.ornl.gov/cgi-bin/tools/TOX_search',
						body = payload_map,
						progress() # progress bar
					)

					response <-
						content(req, as = 'parsed')

					table <- response %>%
						html_table() %>%
						pluck(., 2) %>%
						mutate(idx = 1:n(), .before = 'Chemical')

					dat <- table %>%
						select(1:3, seq(4, ncol(table), by = 2)) %>%
						set_names(., c('idx', 'compound', 'casrn', c_names)) %>%
						pivot_longer(., cols = !c(1:3), values_drop_na = T)

					refs <- table %>%
						select(1:3, seq(5, ncol(table), by = 2)) %>%
						set_names(., c('idx', 'compound', 'casrn', c_names)) %>%
						mutate(
							across(!c(idx:casrn), as.character),
							across(!c(idx:casrn), ~ na_if(., ""))
						) %>%
						pivot_longer(
							.,
							cols = !c(1:3),
							values_to = 'ref',
							values_drop_na = T
						)

					df <- left_join(dat, refs, join_by(idx, compound, casrn, name)) %>%
						left_join(unit_table, join_by('name')) %>%
						select(-idx)

					return(df)
				}
			) %>%
				list_rbind()

			write_rds(chemtox, 'rais_chemtox.RDS')

			rm(list = ls())
		}

		## Radionuclide Specific Parameters ----------------------------------------
		{
			rais_radtox_params <- c(
				'ARAR',
				'Inhalation Exposure Factor',
				'Maximum Contamination Limit',
				'Specific Activity'
			)

			webpage <- read_html(
				"https://rais.ornl.gov/cgi-bin/tools/TOX_search?select=radspef"
			)

			chems <- list(
				dat = html_elements(
					webpage,
					xpath = '//*[@id="analysis_all"]/option'
				) %>%
					html_attrs() %>%
					unlist() %>%
					unname()
			) %>%
				flatten()

			endpoints <- list(
				val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
					html_text(., trim = T)
			) %>%
				flatten()

			short_code <- map(
				endpoints,
				~ {
					str_extract(.x, "\\(([^)]*)\\)") %>%
						str_remove_all("[()]") %>%
						str_trim()
				}
			) %>%
				unlist()

			c_names <- map(
				endpoints,
				~ {
					str_split_fixed(.x, pattern = "\\(", n = 2) %>%
						.[, 1] %>%
						str_trim()
				}
			) %>%
				unlist()

			units <- map(
				endpoints,
				~ {
					str_extract(.x, "\\([^()]*\\)(?=[^()]*$)") %>%
						str_remove_all("[()]") %>%
						str_trim()
				}
			) %>%
				unlist()

			options <- list(
				val = html_elements(webpage, xpath = '//*[@id="fields"]/option') %>%
					html_attrs() %>%
					unlist() %>%
					unname()
			) %>%
				flatten()

			unit_table <- tibble(name = c_names, units = units) %>%
				filter(
					# ! Filtering out unwanted options, may go back later to add them back in
					name %in%
						rais_radtox_params
				)

			rm(webpage, endpoints)

			{
				p1 <- list(
					"_submitted_form" = 1,
					"action" = "next",
					'select' = 'radspef',
					'selectall' = 'no',
					'table' = 'radparams'
				)

				ops <- map(
					options,
					~ list(
						'fields' = .x
					)
				) %>%
					flatten()

				p2 <- list(
					'alloptions' = 'all',
					'_submit' = 'Retrieve'
				)

				chems_payload <- map(
					chems,
					~ list(
						'analysis' = .x
					)
				) %>%
					flatten() %>%
					split(
						.,
						rep(1:ceiling(length(.) / 200), each = 200, length.out = length(.))
					)
			}

			{
				cli::cli_alert_info(
					'Toxicity Values and Physical Parameters for Radionuclides'
				)
				cli::cat_line()
			}

			radtox <- imap(
				chems_payload,
				~ {
					cli_alert_info(.y)

					payload_map <- c(p1, .x, ops, p2) %>% flatten()

					req <- POST(
						url = 'https://rais.ornl.gov/cgi-bin/tools/TOX_search',
						body = payload_map,
						progress() # progress bar
					)

					response <-
						content(req, as = 'parsed')

					table <- response %>%
						html_table() %>%
						pluck(., 2) %>%
						mutate(idx = 1:n(), .before = 'Radionuclide')

					dat <- table %>%
						set_names(., c('idx', 'compound', c_names)) %>%
						select(-28) %>%
						mutate(across(!c(idx), as.character)) %>%
						pivot_longer(., cols = !c(1:2), values_drop_na = T)

					df <- inner_join(dat, unit_table, join_by('name')) %>%
						select(-idx)

					return(df)
				}
			) %>%
				list_rbind()

			write_rds(radtox, 'rais_radstox.RDS')

			rm(list = ls())
		}
	}

	# Generic background values-----------------------------------------------
	{

		# ! NOTE Did not parse the mean values because they didn't make sense. 

		webpage <- read_html("https://rais.ornl.gov/tools/bg_search.php")

		chems <- list(
			dat = html_elements(
				webpage,
				xpath = '//*[@id="analysis-0-from"]/option'
			) %>%
				html_attrs() %>%
				unlist() %>%
				unname()
		) %>%
			flatten() %>%
			map(
				.,
				~ list(
					'analysis[]' = .x
				)
			) %>%
			flatten()

		p1 <- list(
			"_qf__dualselect" = "",
			"action" = "Retrieve"
		)

		p2 <- list(
			"Submit" = "Retrieve"
		)

		payload <- c(p1, chems, p2)

		req <- POST(
			url = "https://rais.ornl.gov/tools/bg_search.php",
			body = payload,
			progress()
		)

		response <-
			content(req, as = "parsed")

		raw <- response %>%
			html_table() %>%
			pluck(., 1)

		gen_bgval <- raw %>%
			mutate(
				orig_result = paste0('Range: ', Range, ' / Mean: ', Mean),
				#unit_name = 'ppm',
				Range = str_remove_all(Range, pattern = '\\<')
			) %>%
			rename(
				'compound' = 'Analyte',
				'media' = 'Soil-type',
				'cit' = 'Reference',
				'range' = 'Range'
			) %>%
			split(str_detect(.$range, '\\%'))

		#No percents, ppm
		gen_bgval$`FALSE` <-
			gen_bgval$`FALSE` %>%
			mutate(idx = 1:n()) %>%
			separate_longer_delim(cols = range, delim = '-') %>%
			group_by(idx) %>%
			mutate(
				unit = 'ppm',
				value = str_remove_all(range, pattern = '\\*') %>% as.numeric(),
				# mean = case_when(
				#   is.na(mean) ~ mean(value),
				#   .default = mean),
				n_r = case_when(
					min(value) == value ~ 'Lower range',
					max(value) == value ~ 'Upper range'
				)
			) %>%
			ungroup() %>%
			select(-range, -Mean)

		gen_bgval$`TRUE` <-
			gen_bgval$`TRUE` %>%
			mutate(idx = 1:n()) %>%
			separate_longer_delim(cols = range, delim = '-') %>%
			group_by(idx) %>%
			mutate(
				unit = 'percent (%)',
				value = str_remove_all(range, pattern = '\\%') %>% as.numeric(),
				# mean = case_when(
				#   is.na(mean) ~ mean(value),
				#   .default = mean),
				n_r = case_when(
					min(value) == value ~ 'Lower range',
					max(value) == value ~ 'Upper range'
				)
			) %>%
			ungroup() %>%
			select(-range, -Mean)

		gen_bgval <- gen_bgval %>%
			list_rbind()

		write_rds(gen_bgval, 'gen_bgval.RDS')
		rm(list = ls())
	}
	rm(file_ages_days, files_exist_check, files_to_check)
	rm(rebuild_is_needed)
} else {
	(rm(rebuild_is_needed))
}

{
	library(here)
	library(cli)
	library(janitor)
	library(rvest)
	library(httr)
	library(tidyverse)
	library(rio)
	#library(ritis)
	#library(textreuse)
	#library(ctxR)
	library(ComptoxR)

	setwd(here('nemi'))

	load('.RData')
}


# Functions ---------------------------------------------------------------

safe_read_html <- possibly(read_html)

# Download ----------------------------------------------------------------

page <- read_html('https://www.nemi.gov/methods/browse_methods/?')

h3_elements <- page %>%
	html_elements(., 'h3') %>%
	html_text(., trim = T)

h4_elements <- page %>%
	html_elements(., 'h4') %>%
	html_text(., trim = T)

l_max <- length(h3_elements)

tax_tree <- map(
	seq(1:l_max),
	~ {
		df <- page %>%
			html_elements(
				.,
				xpath = paste0('//*[@id="browse-methods-content-div"]/div[', .x, ']')
			) %>%
			html_elements(., 'h4') %>%
			html_text(., trim = T) %>%
			str_remove_all(., pattern = "-.*$") %>%
			str_squish() %>%
			as.list()
	}
) %>%
	set_names(
		.,
		page %>%
			html_elements(., 'h3') %>%
			html_text(., trim = T) %>%
			str_remove_all(., pattern = "-.*$") %>%
			str_squish() %>%
			str_to_title()
	)

raw_tbls <-
	tax_tree %>%
	unname() %>%
	imap(
		.,
		~ {
			idx <- .y
			len <- length(.x)

			h3 <- names(tax_tree[idx])

			map(
				seq(1:len),
				~ {
					cli_alert(paste0(h3, ': ', tax_tree[[idx]][[.x]]))
					cli_text("")

					df <-
						page %>%
						html_elements(
							.,
							xpath = paste0(
								'//*[@id="browse-methods-content-div"]/div[',
								idx,
								']/div[3]/div[',
								.x,
								']/div/table'
							)
						) %>%
						html_table() %>%
						pluck(., 1) %>%
						magrittr::set_colnames(
							.,
							c(
								'method_id',
								'method_descriptive_name',
								'method_source',
								'x4',
								'contact',
								'x6',
								'url'
							)
						) %>%
						#as_tibble(., .name_repair = 'universal') %>%
						clean_names() %>%
						filter(
							.,
							str_detect(method_id, pattern = 'URL|Contact', negate = T)
						) %>%
						select(-c(x4, x6, url)) %>%
						mutate(
							method_source = str_remove_all(method_source, pattern = "\n.*$")
						)
				}
			) %>%
				set_names(., tax_tree[[idx]]) %>%
				list_rbind(., names_to = 'sub_cat')
		}
	) %>%
	set_names(., names(tax_tree)) %>%
	list_rbind(., names_to = 'super_cat')

links <- page %>%
	html_elements(., "a") %>%
	map(
		.,
		~ tibble(
			url = html_attr(.x, "href"),
			text = html_text(.x, trim = TRUE)
		)
	) %>%
	list_rbind() %>%
	filter(., str_detect(url, pattern = 'method_summary', negate = F))

super_tbl <- left_join(raw_tbls, links, join_by(method_id == text)) %>%
	#NOTE Removes weird pages
	#COMBAK Perhaps later...
	filter(super_cat != 'Statistical')

url_fs <- super_tbl %>%
	select(method_id, url) #%>%
#.[101:103, ]
#  slice_sample(n = 10)

fact_sheets <- url_fs$url %>%
	imap(
		.,
		~ {
			url <- paste0('https://www.nemi.gov', .x)
			max_int <- length(super_tbl$url)

			cli_alert(paste0(.y, ' / ', max_int, ': ', url))
			cli_text("")

			#HACK to work around dead pages
			page <- safe_read_html(url)

			if (is.null(page)) {
				cli_alert_danger(paste0(url, ' failed!'))
				cli_text("")
				return(NULL)
			} else {
				df <- list()

				df$fs <- page %>%
					html_elements(., xpath = '//*[@id="method-tab"]/table[1]') %>%
					html_table() %>%
					pluck(., 1) %>%
					magrittr::set_colnames(., c('name', 'value')) %>%
					select(name, value)

				df$analytes <- page %>%
					html_elements(., xpath = '//*[@id="analytes-tab"]/table[1]') %>%
					html_table() %>%
					pluck(., 1)

				df[["analytes"]][["Analyte"]] <- page %>%
					html_elements(., xpath = '//*[@id="analytes-tab"]/table[1]') %>%
					html_elements(., 'b') %>%
					html_text()

				analyte_rows <- page %>%
					html_elements(., "#analytes-tab .data-table tbody tr")

				df$analytes_synonyms <- map(
					analyte_rows,
					~ {
						# Get the main analyte name, which is inside a <b> tag
						main_analyte <- .x %>%
							html_element("b") %>%
							html_text()

						# Get the corresponding synonyms for that analyte
						synonyms <- .x %>%
							html_elements(".hidden-details-div li") %>%
							html_text()

						# Return the synonyms, with the main_analyte as the name
						list(synonyms) %>% set_names(main_analyte)
					}
				) %>%
					list_flatten() # Flatten the list of lists into a single named list

				return(df)
			}
		},
		.progress = T
	) %>%
	set_names(., url_fs$method_id)

errors <- fact_sheets %>%
	keep(., is.null)

write_rds(errors, 'nemi_method_errors.RDS')

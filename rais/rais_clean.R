# Packages -------------------------------------------------------------
{
	library(here)
	library(cli)
	library(janitor)
	library(tidyverse)
	library(rio)
	library(ComptoxR)

	setwd(here('rais'))
}

#Read in -------------------------------------------------------------------------

raw_rais <- list.files(here('rais'), pattern = '*.RDS') %>%
	map(., ~ read_rds(here('rais', .x))) %>%
	set_names(list.files(here('rais'), pattern = '*.RDS'))

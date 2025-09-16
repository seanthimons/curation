dbListTables(eco_con) %>% sort()

tbl(eco_con, 'exposure_type_codes')
tbl(eco_con, 'app_exposure_types')

tbl(eco_con, 'species')
tbl(eco_con, 'app_lifestages') %>% print(n = Inf)
tbl(eco_con, 'lifestage_codes')


dbListTables(eco_con) %>%
  sort() %>%
  .[str_detect(., pattern = 'app_')]

tbl(eco_con, 'tests') %>%
  glimpse()
pull('test_type') %>%
  unique()

tbl(eco_con, 'results') %>%
  pull(endpoint) %>%
  unique() %>%
  .[str_detect(., pattern = 'NO')]
glimpse()

tbl(eco_con, 'endpoint_codes') %>%
  collect() %>%
  print(n = Inf)

eco_risk_tbl %>%
  filter(
    eco_group == 'Mammals',
    life_stage == 'Adult' | life_stage == 'Other/Unknown',
    endpoint_group == 'NOEL | NOEC'
  ) %>%

  group_by(
    eco_group,
    life_stage,
    #org_lifestage,
    endpoint_group
  ) %>%
  reframe(
    n = n(),
    dur_mode = Mode(new_dur),
    dur_min = min(new_dur),
    dur_mean = mean(new_dur),
    dur_max = max(new_dur)
  ) %>%
  #arrange(eco_group, life_stage) %>%
  #knitr::kable(.) %>%
  print(n = Inf)

eco_risk_tbl %>%
  filter(
    # eco_group == 'Fish',
    # common_name == 'Zebra Danio',
    # new_unit == 'mg/L'
  ) %>%
  group_by(
    eco_group,
    common_name,
    #endpoint_group,
    #new_unit
  )
reframe(
  #n = n(),
  conc = mean(as.numeric(new_value))
) %>%
  pivot_wider(names_from = endpoint_group, values_from = conc) %>%
  print(n = Inf)

#multigeneration----

tbl(eco_con, 'tests') %>%
  select(
    'test_id',
    'test_cas',
    'species_number',
    'exposure_type',
    'test_type',
    'organism_lifestage'
  ) %>%
  filter(
    organism_lifestage != 'F11',
    str_detect(organism_lifestage, 'F0|F1')
  ) %>%
  inner_join(
    tbl(eco_con, 'chemicals'),
    join_by(test_cas == cas_number)
  ) %>%
  inner_join(
    tbl(eco_con, "species") %>%
      select(
        'species_number',
        'common_name',
        'latin_name',
        'ecotox_group'
      ),
    join_by('species_number')
  ) %>%
  inner_join(
    tbl(eco_con, 'results') %>%
      select(
        'result_id',
        'test_id',
        'obs_duration_mean',
        'obs_duration_unit',
        'endpoint',
        'effect',
        'conc1_mean',
        'conc1_unit'
      ),
    join_by('test_id')
  ) %>%
  collect() %>%
  group_by(
    chemical_name,
    #common_name,
    organism_lifestage
  ) %>%
  summarize(
    n = n()
  ) %>%
  arrange(organism_lifestage) %>%
  pivot_wider(names_from = organism_lifestage, values_from = n)



		# duration ----------------------------------------------------------------

		#filter(eco_group == 'Mammals' | eco_group == 'Birds' | eco_group == 'Fish') %>%
		# mutate(
		# 	test_type = case_when(
		# 		# Mammals -----------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Mammals') &
		# 			(effect == 'MOR') &
		# 			(exposure_group == 'ORAL' | is.na(exposure_group)) &
		# 			#(life_stage == 'Adult') &
		# 			(new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50') ~
		# 			'acute',

		# 		(eco_group == 'Mammals') &
		# 			(effect == 'MOR') &
		# 			(exposure_group == 'ORAL' | is.na(exposure_group)) &
		# 			#(life_stage == 'Adult') &
		# 			(new_unit == 'mg/kg bdwt') &
		# 			(endpoint == 'LD50') ~
		# 			'acute',
		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Mammals') &
		# 			(effect == 'MOR') &
		# 			(exposure_group == 'ORAL' | is.na(exposure_group)) &
		# 			#(life_stage == 'Adult') &
		# 			(endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
		# 			(new_unit == 'mg/kg/d') ~
		# 			'chronic',

		# 		# Birds -------------------------------------------------------------------
		# 		#NOTE Needs better breakdown for reptiles and amphibians...
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Birds' |
		# 			eco_group == 'Amphibians' |
		# 			eco_group == 'Reptiles') &
		# 			(effect == 'MOR') &
		# 			(exposure_group == 'ORAL' | is.na(exposure_group)) &
		# 			#(life_stage == 'Adult') &
		# 			(new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Birds' |
		# 			eco_group == 'Amphibians' |
		# 			eco_group == 'Reptiles') &
		# 			(effect == 'MOR') &
		# 			(exposure_group == 'ORAL' | is.na(exposure_group)) &
		# 			#(life_stage == 'Adult') &
		# 			(endpoint == 'NOEL' | endpoint == 'NR-ZERO') &
		# 			(new_unit == 'mg/kg/d' | new_unit == 'mg/kg bdwt/d') ~
		# 			'chronic',

		# 		# Fish --------------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Fish') &
		# 			(effect == 'MOR') &
		# 			#(life_stage == 'Adult') &
		# 			(new_dur == 96) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Fish') &
		# 			(effect == 'MOR') &
		# 			#(life_stage == 'Adult') &
		# 			(new_dur >= 144) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'LD50' | endpoint == 'EC50' | endpoint == 'LC50') ~
		# 			'chronic',

		# 		(eco_group == 'Fish') &
		# 			(effect == 'MOR') &
		# 			#(life_stage == 'Adult') &
		# 			(new_dur == 504) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
		# 			'chronic',

		# 		# Bees --------------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		# Could be refined later, but OPP doesn't isn't entirely clear
		# 		(eco_group == 'Bees') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 24 | new_dur == 28 | new_dur == 72) &
		# 			(new_unit == 'ug/bee') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Bees') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 240) &
		# 			(new_unit == 'ug/bee') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50') ~
		# 			'chronic',

		# 		# Insects -----------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Insects/Spiders') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 24 | new_dur == 48 | new_dur == 72) &
		# 			(new_unit == 'mg/L' | new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------

		# 		(eco_group == 'Insects/Spiders') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 504 | new_dur == 672) &
		# 			(new_unit == 'mg/L' | new_unit == 'mg/kg') &
		# 			(endpoint == 'NOEL' | endpoint == 'NOEC' | endpoint == 'NR-ZERO') ~
		# 			'chronic',

		# 		# Invertebrates -----------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 24 | new_dur == 48 | new_dur == 72 | new_dur == 96) &
		# 			(new_unit == 'mg/L' | new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Invertebrates' | eco_group == 'Molluscs') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 504 | new_dur == 672) &
		# 			(new_unit == 'mg/L' | new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'chronic',

		# 		# Worms -------------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Worms') &
		# 			(effect == 'MOR') &
		# 			(new_dur == 336) &
		# 			(new_unit == 'mg/kg') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Worms') &
		# 			(effect == 'MOR') &
		# 			(new_dur <= 336) &
		# 			(new_unit == 'mg/kg') &
		# 			(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
		# 			'chronic',

		# 		# Crustaceans -----------------------------------------------------------------------
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Crustaceans') &
		# 			(effect == 'MOR') &
		# 			(new_dur <= 96) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Crustaceans') &
		# 			(effect == 'MOR') &
		# 			(new_dur >= 672) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
		# 			'chronic',

		# 		# Algae -----------------------------------------------------------------------
		# 		#NOTE Needs better, hard to find data for fungi and mosses etc
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Algae' |
		# 			eco_group == 'Fungi' |
		# 			eco_group == 'Moss, Hornworts') &
		# 			(new_dur <= 24 * 7) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Algae' |
		# 			eco_group == 'Fungi' |
		# 			eco_group == 'Moss, Hornworts') &
		# 			(new_dur == 96) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
		# 			'chronic',

		# 		# Flowers, Trees, Shrubs, Ferns-----------------------------------------------------------------------
		# 		#Note probably could be something like developemental etc...
		# 		## Acute -------------------------------------------------------------------
		# 		(eco_group == 'Flowers, Trees, Shrubs, Ferns') &
		# 			(new_dur <= 7 * 24) &
		# 			(new_unit == 'mg/L') &
		# 			(endpoint == 'LD50' | endpoint == 'LC50' | endpoint == 'EC50') ~
		# 			'acute',

		# 		## Chronic -----------------------------------------------------------------
		# 		(eco_group == 'Flowers, Trees, Shrubs, Ferns') &
		# 			(effect != 'MOR') &
		# 			(endpoint == 'NOEC' | endpoint == 'NOEL' | endpoint == 'NR-ZERO') ~
		# 			'chronic',
		# 	)
		# ) %>%
		# filter(!is.na(test_type))



left_join(
	.,
	standardtox_dict,
	join_by(num == unit)
) %>%
	rename(num_type = type) %>%
	left_join(
		.,
		standardtox_dict,
		join_by(denom == unit)
	) %>%
	rename(denom_type = type) %>%
	mutate(
		derived_unit = case_when(
			# -- Dosage per unit/organism (highest priority) --
			!is.na(cur_units) &
				str_detect(cur_units, "/org|/fish|/egg|/bee|/cell|/disk|/cntr") ~
				"dosage (amount/unit)",

			# -- Specific Formulations / Non-science --
			!is.na(cur_units) & str_to_lower(cur_units) == "granules" ~ "noscience",

			# -- Application Rates (amount per area) --
			num_type == "mass" & denom_type == "area" ~
				"application rate (mass/area)",
			num_type == "volume" & denom_type == "area" ~
				"application rate (volume/area)",
			num_type == "mol" & denom_type == "area" ~ "molar application rate",

			# -- Molar Concentration (moles per volume) --
			num_type %in% c("mol", "mol/volume") & denom_type == "volume" ~
				"molar concentration",
			!is.na(cur_units) & cur_units %in% c("mM", "M", "nM", "pM", "uM", "N") ~
				"molar concentration",
			!is.na(cur_units) & str_detect(cur_units, fixed("mol/")) ~
				"molar concentration",

			# -- Concentration (mass/volume) --
			is.na(cur_units) & str_detect(raw, "% w/v") ~
				"concentration (mass/volume)",
			num_type == "mass" & denom_type == "volume" ~
				"concentration (mass/volume)",

			# -- Concentration (volume/mass) -- NEW CATEGORY
			num_type == "volume" & denom_type == "mass" ~
				"concentration (volume/mass)",

			# -- Fractions (like-units divided by like-units) --
			is.na(cur_units) & str_detect(raw, "% v/v") ~ "volume fraction",
			num_type == "mass" & denom_type == "mass" ~ "mass fraction",
			num_type == "volume" & denom_type == "volume" ~ "volume fraction",
			num_type == "fraction" & (is.na(denom_type) | denom_type == "noscience") ~
				"fraction (dimensionless)",

			# -- Molality (moles per mass) --
			num_type %in% c("mol", "mol/volume") & denom_type == "mass" ~ "molality",

			# -- Linear Density (mass per length) --
			num_type == "mass" & denom_type == "length" ~ "linear density",

			# -- Radioactivity Units --
			num_type == "radioactivity" & denom_type == "volume" ~
				"radioactivity concentration",
			num_type == "radioactivity" & denom_type == "mass" ~ "specific activity",
			num_type == "radioactivity" & denom_type == "mol" ~ "molar activity",

			# -- Flow/Count Rates (amount per time) --
			(num_type == "mass" | num_type == "volume") & denom_type == "time" ~
				"flow rate",
			!is.na(cur_units) & str_detect(cur_units, "counts/min") ~ "count rate",

			# -- Base Units (not derived from a ratio) --
			is.na(denom_type) &
				num_type %in% c("mass", "volume", "length", "mol", "radioactivity") ~
				num_type,

			# -- Final catch-all for anything else --
			TRUE ~ NA_character_
		)
	)

mutate(
	# The regex extracts characters from the start of the string
	# until a forward slash or space is encountered (i.e., the numerator).
	num = str_extract(raw, "^[^/\\s]+"),
	# The regex uses a positive lookbehind to extract characters
	# that follow a forward slash (i.e., the denominator).
	denom = str_extract(raw, "(?<=/)[^/\\s]+"),

	# The regex matches the first word and captures everything that
	# follows it.
	suffix = stringr::str_match(raw, "^\\S+\\s+(.*)")[, 2],
	cur_units = case_when(
		# The regex checks for the presence of a percent sign.
		str_detect(raw, pattern = '%') ~ NA,
		part_counts >= 1 ~ paste0(num, "/", denom),
		.default = num
	)
)

units %>%
	filter(str_detect(orig, 'mg/L 10 mi') | str_detect(raw, 'mg/L 10 mi'))

units %>%
	distinct(num) %>%
	print(n = Inf)

units %>%
	filter(part_counts == 1) %>%
	count(num) %>%
	arrange(desc(n))

# Testing ----------------------------------------------------------------

library(units)

valid_units <- units::valid_udunits()

install_unit("gal100", def = "100 gal")

a <- set_units(1, 'month')
units(a) <- units::make_units(days)
a <- set_units(a, 'g/L')
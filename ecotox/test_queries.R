library(ComptoxR)

all_lists <- ct_lists_all()

query_chems <- ct_list(
  list_name = c(
    'EPAHFR',
    'EPAHFRTABLE2',
    'FRACFOCUS',
    'CALWATERBDS',
    'PRODWATER'
  )
) %>%
  ct_details(query = .) %>%
  mutate(casrn = str_remove_all(casrn, "-"))

q1 <- post_results(casrn = query_chems$casrn)

q2 <- q1 %>%
  left_join(query_chems, ., join_by(casrn == test_cas)) %>%
  group_by(
    preferredName,
    casrn,
    endpoint,
    #test_type,
    eco_group,
    #harmonized_life_stage
  ) %>%
  reframe(
    result = min(new_value),
    unit = new_unit
  ) %>%
  distinct() %>%
  filter(!is.na(result) & endpoint == 'EC50')


df <- tibble::tribble(
	~time_val,
	~time_unit,
	10,
	"days",
	2,
	"weeks",
	48,
	"hours",
	3600,
	"seconds"
)


# Dictionaries -----------------------------------------------------------

# unit_dict <- rio::import(here('ecotox', 'MeasureUnit.csv')) %>%
# 	clean_names() %>%
# 	select(
# 		code,
# 		target_unit,
# 		# conversion_factor,
# 		# conversion_coefficient,
# 		description
# 	) %>%
# 	mutate(
# 		domain = str_extract(description, "^[^,]+")
# 	) %>%
# 	select(-description)

unit_result <-
  tibble::tribble(
					 ~unit, ~multiplier,					~unit_conv,					 ~type,
						"ag",			 1e-18,								 "g",					"mass",
						"fg",			 1e-15,								 "g",					"mass",
						"pg",			 1e-12,								 "g",					"mass",
						"ng",			 1e-09,								 "g",					"mass",
						"ug",			 1e-06,								 "g",					"mass",
						"mg",			 0.001,								 "g",					"mass",
						 "g",					 1,								 "g",					"mass",
						"kg",				1000,								 "g",					"mass",
						"kg N",        1,						     "kg N",			"mass",
						 "t",			 1e+06,								 "g",					"mass",
					 "ton",			 1e+06,								 "g",					"mass",
					"tons",			 1e+06,								 "g",					"mass",
			 "quintal",			 1e+05,								 "g",					"mass",
						 "q",			 1e+05,								 "g",					"mass",
						"pl",			 1e-12,								 "l",				"volume",
						"nl",			 1e-09,								 "l",				"volume",
						"ul",			 1e-06,								 "l",				"volume",
						"ml",			 0.001,								 "l",				"volume",
						"dl",				 0.1,								 "l",				"volume",
						 "l",					 1,								 "l",				"volume",
					 "lit",					 1,								 "l",				"volume",
						"hl",				 100,								 "l",				"volume",
						"pL",			 1e-12,								 "l",				"volume",
						"nL",			 1e-09,								 "l",				"volume",
						"uL",			 1e-06,								 "l",				"volume",
						"mL",			 0.001,								 "l",				"volume",
						"dL",				 0.1,								 "l",				"volume",
						 "L",					 1,								 "l",				"volume",
						"hL",				 100,								 "l",				"volume",
						"bu",		36.36872,								 "l",				"volume",
				"bushel",		36.36872,								 "l",				"volume",
					 "ppq",			 1e-06,							 "ppb",			"fraction",
					 "ppt",			 0.001,							 "ppb",			"fraction",
					 "ppb",					 1,							 "ppb",			"fraction",
					 "ppm",				1000,							 "ppb",			"fraction",
			"ppm-hour",				1000,						 "ppb/h",			"fraction",
	"ppm for 36hr", 27.77777778,						 "ppb/h",			"fraction",
					"ppmv",				1000,							 "ppb",			"fraction",
					"ppmw",				1000,							 "ppb",			"fraction",
					"0/00",			 1e+06,							 "ppb",			"fraction",
					 "ptm",					 1,							 "ppb",			"fraction",
						"no",					NA,									NA,				"amount",
					"amol",			 1e-18,							 "mol",					 "mol",
					"fmol",			 1e-15,							 "mol",					 "mol",
					"pmol",			 1e-12,							 "mol",					 "mol",
					"nmol",			 1e-09,							 "mol",					 "mol",
					"umol",			 1e-06,							 "mol",					 "mol",
				"umoles",			 1e-06,							 "mol",					 "mol",
				 "mumol",			 1e-09,							 "mol",					 "mol",
					"mmol",			 0.001,							 "mol",					 "mol",
					"cmol",				0.01,							 "mol",					 "mol",
					 "mol",					 1,							 "mol",					 "mol",
					"kmol",				1000,							 "mol",					 "mol",
						"pM",			 1e-12,						 "mol/l",		"mol/volume",
						"nM",			 1e-09,						 "mol/l",		"mol/volume",
						"uM",			 1e-06,						 "mol/l",		"mol/volume",
						"mM",			 0.001,						 "mol/l",		"mol/volume",
						 "M",					 1,						 "mol/l",		"mol/volume",
				 "molal",			 0.001,						 "mol/g",			"mol/mass",
					"mOsm",			 0.001,						 "Osm/l",		"osmolarity",
						"in",			0.0254,								 "m",				"length",
						"yd",			0.9144,								 "m",				"length",
						"ft",			0.3048,								 "m",				"length",
		 "linear ft",			0.3048,								 "m",				"length",
					 "rod",			5.0292,								 "m",				"length",
						"um",			 1e-06,								 "m",				"length",
						"mm",			 0.001,								 "m",				"length",
						"cm",				0.01,								 "m",				"length",
						"dm",				 0.1,								 "m",				"length",
						 "m",					 1,								 "m",				"length",
						"km",				1000,								 "m",				"length",
					 "neq",			 1e-09,								"eq",		 "noscience",
					 "meq",			 0.001,								"eq",		 "noscience",
					 "ueq",			 1e-06,								"eq",		 "noscience",
					 "mm2",			 1e-06,								"m2",					"area",
					 "cm2",			 1e-04,								"m2",					"area",
					 "dm2",				0.01,								"m2",					"area",
					 "hm2",     10000,                "m2",					"area",
						"m2",					 1,								"m2",					"area",
					 "yd2",		0.836127,								"m2",					"area",
						"ha",			 10000,								"m2",					"area",
			 "hectare",			 10000,								"m2",					"area",
					"acre",		4046.873,								"m2",					"area",
				 "acres",		4046.873,								"m2",					"area",
						"ac",		4046.873,								"m2",					"area",
					"rod2",		 25.2929,								"m2",					"area",
					 "mi2",	2589988.11,								"m2",					"area",
					 "km2",			 1e+06,								"m2",					"area",
					 "ft2",			0.0929,								"m2",					"area",
					 "sqft",		0.0929,								"m2",					"area",
				"k sqft",				92.9,								"m2",					"area",
				"feddan",				4200,								"m2",					"area",
		"dn(Cyprus)",				1338,								"m2",					"area",
			 "dn(Std)",				1000,								"m2",					"area",
						 "%",			 1e+07,							 "ppb",			"fraction",
					 "â€°",			 1e+06,							 "ppb",			"fraction",
						 "d",					24,								 "h",					"time",
					 "day",					24,								 "h",					"time",
						 "h",					 1,								 "h",					"time",
						"hr",					 1,								 "h",					"time",
					"hour",					 1,								 "h",					"time",
						"mi",	 0.0166667,								 "h",					"time",
					 "min",	 0.0166667,								 "h",					"time",
						"wk",				 168,								 "h",					"time",
						"mo",				 730,								 "h",					"time",
						"yr",				8760,								 "h",					"time",
					 "ft3",	0.02831658,								"m3",				"volume",
					 "mm3",			 1e-09,								"m3",				"volume",
					 "cm3",			 1e-06,								"m3",				"volume",
					 "dm3",			 0.001,								"m3",				"volume",
						"m3",					 1,								"m3",				"volume",
				 "fl_oz",	0.02957353,								 "l",				"volume",
						"pt", 0.473176473,							 "l",				"volume",
						"oz", 28.34952313,							 "g",					"mass",
					 "gal", 3.785411784,						 	 "l",				"volume",
						"ga", 3.785411784,							 "l",				"volume",
						"qt", 0.946352946,							 "l",				"volume",
						"lb",		 453.592,								 "g",					"mass",
					 "lbs",		 453.592,								 "g",					"mass",
					 "PSU",					 1,							 "PSU",		 "noscience",
						"--",					NA,									NA,				"nodata",
						"NR",					NA,									NA,				"nodata",
						"eu",					 1,								"eu",		 "noscience",
						"EU",					 1,								"eu",		 "noscience",
					 "MBq",			 1e+06,								"Bq", "radioactivity",
					 "kBq",				1000,								"Bq", "radioactivity",
						"Bq",					 1,								"Bq", "radioactivity",
					 "mBq",			 0.001,								"Bq", "radioactivity",
					 "uBq",			 1e-06,								"Bq", "radioactivity",
						"Ci",		 3.7e+10,								"Bq", "radioactivity",
					 "mCI",		 3.7e+07,								"Bq", "radioactivity",
					 "mCi",		 3.7e+07,								"Bq", "radioactivity",
				"mCi mg",					NA,									NA,		 "noscience",
					 "uCI",			 37000,								"Bq", "radioactivity",
					 "uCi",			 37000,								"Bq", "radioactivity",
					 "nCI",					37,								"Bq", "radioactivity",
					 "nCi",					37,								"Bq", "radioactivity",
					 "pCI",			 0.037,								"Bq", "radioactivity",
					 "pCi",			 0.037,								"Bq", "radioactivity",
					 "ICU",					 1,							 "ICU",		 "noscience",
					 "USP",					 1,							 "USP",		 "noscience",
						"iu",					 1,						 "iunit",		 "noscience",
						"IU",					 1,						 "iunit",		 "noscience",
					 "mIU",					NA,									NA,							NA,
				"fibers",					 1,						"fibers",		 "noscience",
						"kJ",				1000,								 "J",				"energy",
						"mS",			 0.001,								 "S",	 "electricity",
						"dS",				 0.1,								 "S",	 "electricity",
					 "org",					 1,					"organism",		 "noscience",
				"organi",					 1,					"organism",		 "noscience",
						 "v",					NA,									NA,				"volume",
				 "% v/v",			 1e+07,							 "ppb",			"fraction",
					 "cwt",			 45360,								 "g",					"mass",
						 "w",					NA,									NA,		 "noscience",
				 "% w/v",			 1e+07,							 "ppb",			"fraction",
				"in dia",					NA,									NA,		 "noscience",
					 "egg",					 1,							 "egg",		 "noscience",
			 "pellets",					 1,					 "pellets",		 "noscience",
					 "bee",					 1,							 "bee",		 "noscience",
					"fish",					 1,							"fish",		 "noscience",
					 "dpm",	0.01666667,								"Bq", "radioactivity",
						"sd",					 1,							"seed",		 "noscience",
					"seed",					 1,							"seed",    "noscience",
				 "seeds",					 1,							"seed",		 "noscience",
					"cntr",					 1,				 "container",		 "noscience",
					"plot",					 1,							"plot",		 "noscience",
					 "cpm",					 1,							 "cpm",		 "noscience",
				 "mound",					 1,						 "mound",		 "noscience",
		"mouse unit",					 1,				"mouse unit",		 "noscience",
					"disk",					 1,							"disk",		 "noscience",
						"cc",					 1,						"cocoon",		 "noscience",
					"cell",					 1,							"cell",		 "noscience",
					"dose",					 1,							"dose",		 "noscience",
						"em",					 1,						"embryo",		 "noscience",
			"granules",					 1,					 "granule",		 "noscience",
						"lf",					 1,							"leaf",		 "noscience",
					"tank",					 1,							"tank",		 "noscience",
					"tbsp",					 1,				"tablespoon",		 "noscience",
					"Tbsp",					 1,				"tablespoon",		 "noscience",
					 "tsp",					 1,					"teaspoon",		 "noscience",
						 "u",					 1,							"unit",		 "noscience",
						 "U",					 1,							"unit",		 "noscience",
					"unit",					 1,							"unit",		 "noscience",
				 "units",					 1,							"unit",		 "noscience",
			 "U of fl",					 1, "unit fluorescence",	 "noscience",
						"ML",					 1,							"male",		 "noscience",
						 "N",					 1,						"Normal",		 "noscience",
						"RA",					 1,						 "ratio",		 "noscience",
			"ug-atoms",					 1,					"ug-atoms",		 "noscience",
			 "u-atoms",					 1,					 "u-atoms",		 "noscience",
					 "PIg",					 1,							 "PIg",		 "noscience",
					 "g d",					 1,									NA,		 "noscience",
				 "ng eq",					 1,									NA,		 "noscience",
				 "6 in pots",			 1,				 "6_in pot",			"noscience"
	)


unit_symbols <-
  tibble::tribble(
~symbol,	~name,
"CEC","soil.cation.exchange",
"DT","digestivetract",
"100% O2", "100%O2", 
"H2O","water",
"TI","tissue",
"ae","acidequivalents",
"agar","agar",
"ai","activeingredient",
"bdwt","bodyweight",
"blood","blood",
"bt","bait",
"bw","bodyweight",
"bwt","bodyweight",
"caliper","caliper",
"circ","circular",
'canopy', 'canopy',
"dbh","diameterbreastheight",
"dia","diameter",
"diet","diet",
"disk","disk",
"dry wght", "dryweight",
"dw","dry weight",
"dry","dry",
"dry_diet", "drydiet",
#"egg","egg",
"eu","experimentalunit",
"fd","food",
#"fish","fish",
"food","food",
"humus","humus",
"ht" , 'plant height',
"ld","lipid",
"lipid","lipid",
"litter","litter",
"linear", 'linear',
"mat","material",
"media","media",
"om","organicmatter",
#"org","organism",
"pellet","pellet",
"plt","pellet",
'protein', 'protein',
#"sd","seed",
#"seed","seed",
"soil","soil",
"solv", 'solvent',
"solvent","solvent",
'soln', 'solution',
"tubers","tubers",
"tkdi", 'trunk diameter at 1.5 m above ground',
"wet wght", "wetweight",
"wet_bdwt", "wetbodyweight",
"wet","wet",
"wt","wet",
'wght', 'weight'
) %>%
  bind_rows(mutate(., symbol = toupper(symbol)))


duration_dict <- tbl(eco_con, 'duration_unit_codes') %>%
  mutate(
    base_unit = case_when(
      str_detect(tolower(description), "minute") ~ "minutes",
      str_detect(tolower(description), "second") ~ "seconds",
      str_detect(tolower(description), "hour") ~ "hours",
      str_detect(tolower(description), "day") ~ "days",
      str_detect(tolower(description), "week") ~ "weeks",
      str_detect(tolower(description), "month") ~ "months",
      str_detect(tolower(description), "year") ~ "years",
      .default = NA
    ),
    # Converts to hours
    conversion = case_when(
      str_detect(tolower(description), "minute") ~ 1 / 60,
      str_detect(tolower(description), "second") ~ 1 / 3600,
      str_detect(tolower(description), "hour") ~ 1,
      str_detect(tolower(description), "day") ~ 24,
      str_detect(tolower(description), "week") ~ 24 * 7,
      # Average month
      str_detect(tolower(description), "month") ~ 24 * 30.43685,
      .default = 1
    ),
    cur_duration = case_when(
      !is.na(base_unit) ~ 'h',
      .default = code
    )
  ) %>%
  collect()


# Parsing ----------------------------------------------------------------

## Unit conversion --------------------------------------------------------

units_intermediate <- tbl(eco_con, 'results') %>%
  select(orig = conc1_unit, test_id) %>%
  inner_join(
    .,
    tbl(eco_con, 'tests') %>%
      select(
        test_id,
        test_cas,
        species_number,
        reference_number,
        organism_habitat
      ),
    join_by('test_id')
  ) %>%
  inner_join(
    .,
    tbl(eco_con, 'references') %>%
      select(reference_number, publication_year),
    join_by('reference_number')
  ) %>%
  group_by(orig, organism_habitat) %>%
  summarize(
    n = n(),
    cas_n = n_distinct(test_cas),
    species_n = n_distinct(species_number),
    ref_n = n_distinct(reference_number),
    date_n = n_distinct(publication_year),
    ref_date = max(
      sql("TRY_CAST(REPLACE(publication_year, 'xx', '15') AS NUMERIC)"),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  arrange(
    desc(n),
    desc(cas_n),
    desc(species_n),
    desc(ref_n),
    #	desc(ref_date)
  ) %>%
  # ! NOTE: Removes infrequent units ----
  #filter(n <= 2 & ref_n <= 2) %>%
  #filter(!is.na(orig) & n > 2 & ref_n > 2) %>%
  collect() %>%
  select(
    #	-n,
    -cas_n,
    species_n,
    ref_n,
    -date_n,
    -ref_date,
  ) %>%
  mutate(
    idx = 1:n(),
    raw = str_replace_all(
      orig,
      {
        # Create a regex pattern for whole-word matching of symbols.
        # Symbols are sorted by length (desc) to prioritize longer matches
        # (e.g., 'mg/L' over 'g').
        # Lookarounds '(?<!\w)' and '(?!\w)' ensure that symbols are not
        # part of other words.
        # 'str_escape' is used to handle special characters in symbols.
        unit_symbols %>%
          arrange(-stringr::str_length(symbol)) %>%
          pull(symbol) %>%
          stringr::str_escape() %>%
          paste0("(?<!\\w)", ., "(?!\\w)") %>%
          stringr::str_flatten(., "|")
      },
      replacement = ""
    ) %>%
      # One-off injections
      str_replace_all(
        .,
        c(
          "1k" = "1000"
          # 'mgdrydiet' = 'mg dry_diet',
          # 'gwetbdwt' = 'g wet_bdwt'
        )
      ) %>%
      str_squish() %>%
      # NEW: Add a space between numbers and letters where it is missing.
      # e.g., "25kg" -> "25 kg"
      str_replace_all(
        .,
        pattern = "(?<=\\d)(?=[a-zA-Z])",
        replacement = " "
      ) %>%
      str_replace_all(
        .,
        c(
          '6 in pots' = '6inpots',
          'u-atoms' = 'u_atoms',
          'ug-atoms' = 'ug_atoms',
          "/ " = "/",
          "-" = "/",
          "0/00" = "ppt",
          '\\bppmw\\b' = 'ppm',
          '\\bppmv\\b' = 'ppm',
          '\\bppm w/w\\b' = 'ppm',
          '\\bml\\b' = 'mL',
          '\\bul\\b' = 'uL',
          '\\bof\\b' = "",
          '\\bmi\\b' = 'min',
          ' for ' = "/",
          '% ' = '%_',
          'fl oz' = 'fl_oz',
          "ppt v/v" = 'mL/L',
          'ppm w/v' = 'mg/L'
        )
      ) %>%
      # The regex replaces a space with an underscore if it is preceded by a number
      # and followed by either a letter or another number.
      # e.g., "100 g" -> "100_g"
      # e.g., "100 2" -> "100_2"
      str_replace_all(
        .,
        pattern = "(\\b\\d*\\.?\\d+) (?=[[:alpha:]]|\\d)",
        replacement = "\\1_"
      ) %>%
      str_replace_all(
        .,
        c(
          " in " = "",
          " in" = "",
          ' ' = "/",
          "//" = "/",
          "%_v/v" = "%_v_v",
          "%_w/v" = "%_w_v",
          "%_w/w" = "%_w_w",
          "%_g/g" = "%_w_w",
          '6inpots' = '6 in pots'
        )
      ) %>%
      str_squish() %>%
      str_remove(., "/$"),

    has_number = str_detect(raw, pattern = '/\\d+'),
    suffix = str_extract_all(
      orig,
      {
        # Create a regex pattern for whole-word matching of symbols.
        # This is the same robust pattern generation used for the `raw` column
        # to ensure only full symbols are matched.
        unit_symbols %>%
          arrange(-stringr::str_length(symbol)) %>%
          pull(symbol) %>%
          stringr::str_escape() %>%
          paste0("(?<!\\w)", ., "(?!\\w)") %>%
          stringr::str_flatten(., "|")
      }
    ),
    suffix = map_chr(suffix, ~ paste(.x, collapse = " ")),
    u = raw
  ) %>%
  separate_wider_delim(
    u,
    delim = "/",
    names_sep = "_",
    too_few = 'align_start'
  ) %>%
  mutate(
    across(dplyr::starts_with('u'), ~ na_if(.x, "")),
    part_counts = rowSums(!is.na(select(., dplyr::starts_with('u'))))
  ) %>%
  relocate(part_counts, .after = has_number) %>%
  pivot_longer(
    .,
    cols = dplyr::starts_with('u'),
    names_to = 'name'
  ) %>%
  mutate(
    value = case_when(
      value == "%_" ~ "%",
      value == "%_v_v" ~ "% v/v",
      value == "%_w_v" ~ "% w/v",
      value == "%_w_w" ~ "% w/v",
      value == 'u_atoms' ~ 'u-atoms',
      value == 'ug_atoms' ~ 'ug-atoms',
      .default = value
    ),
    num_mod = str_extract(value, pattern = "\\b\\d*\\.?\\d+_") %>%
      str_remove_all(., pattern = "_") %>%
      as.numeric(),
    value = str_remove_all(value, pattern = "\\b\\d*\\.?\\d+_"),
    value = str_squish(value)
  ) %>%
  # ! Join against dictionary, update here----
  left_join(
    .,
    unit_result,
    join_by(value == unit)
  ) #%>%
pivot_wider(
  .,
  names_from = name,
  values_from = value:type
) %>%
  # Replace NA with 1 in num_mod and multiplier columns
  mutate(across(matches("^(num_mod|mult)"), ~ if_else(is.na(.x), 1, .x))) %>%
  # Dynamically calculate the conversion factor
  rowwise() %>%
  mutate(
    conversion = {
      numer <- c_across(starts_with("multiplier_u_"))[1] *
        c_across(starts_with("num_mod_u_"))[1]
      denoms <- c_across(starts_with("multiplier_u_"))[-1] *
        c_across(starts_with("num_mod_u_"))[-1]
      if (length(denoms) > 0) numer / purrr::reduce(denoms, `*`) else numer
    }
  ) %>%
  ungroup() %>%
  # Dynamically create cur_unit and cur_unit_type
  unite("cur_unit", starts_with("unit_conv_u_"), sep = "/", na.rm = TRUE) %>%
  unite("cur_unit_type", starts_with("type_u_"), sep = "/", na.rm = TRUE) %>%
  mutate(
    unit_domain = case_when(
      # --- Rule 1: Invalid or Uncategorized Units (Highest Priority) ---
      # Catch anything with "noscience" or empty strings first.
      str_detect(cur_unit_type, "noscience") | cur_unit_type == "" ~
        "Invalid / Uncategorized",

      # --- Rule 2: Dosing Rates (Amount / Normalization / Time) ---
      # These are the most specific, so they must come before simpler rates or concentrations.
      str_ends(cur_unit_type, "/time") & str_count(cur_unit_type, "/") == 2 ~
        "Dosing Rate",

      # --- Rule 3: Application Rates (Amount / Area) ---
      cur_unit_type %in% c("mass/area", "volume/area", "mol/area") ~
        "Application Rate",

      # --- Rule 4: Concentrations (Amount / Volume or Amount / Mass) ---
      # Liquid-based concentrations
      cur_unit_type %in% c("mass/volume", "mol/volume", "fraction/volume") ~
        "Concentration (Liquid)",
      # Matrix-based concentrations (e.g., in soil, tissue)
      cur_unit_type %in% c("mass/mass", "mol/mass", "volume/mass") ~
        "Concentration (Matrix)",

      # --- Rule 5: Simple Rates (Amount / Time) ---
      cur_unit_type %in% c("mass/time", "volume/time", "fraction/time") ~
        "Rate",

      # --- Rule 6: Dimensionless Ratios ---
      cur_unit_type %in% c("fraction", "volume/volume") ~ "Ratio / Fraction",

      # --- Rule 7: Radioactivity ---
      # Catches all variations like "radioactivity/volume", "radioactivity/mass", etc.
      str_starts(cur_unit_type, "radioactivity") ~ "Radioactivity",

      # --- Rule 8: Linear Density (Amount / Length) ---
      cur_unit_type %in% c("mass/length", "volume/length") ~ "Linear Density",

      # --- Rule 9: Simple Fundamental Quantities ---
      cur_unit_type == "mass" ~ "Mass",
      cur_unit_type == "volume" ~ "Volume",
      cur_unit_type == "mol" ~ "Amount (molar)",
      cur_unit_type == "length" ~ "Length",
      cur_unit_type == "time" ~ "Time",

      # --- Rule 10: Catch-all for Other Valid but Complex Types ---
      # This will group any remaining complex but valid units.
      TRUE ~ "Other Complex Unit"
    )
  )

unit_conversion <- units_intermediate %>%
  select(
    orig,
    cur_unit,
    cur_unit_type,
    conversion,
    unit_domain
  )

## Duration conversion ----------------------------------------------------

tbl(eco_con, 'results') %>% glimpse()

tbl(eco_con, '') %>% glimpse()

tbl(eco_con, '') %>% glimpse()

duration_units <- tbl(eco_con, 'results') %>%
  select(obs_duration_unit) %>%
  count(obs_duration_unit) %>%
  arrange(desc(n)) %>%
  collect()


## Endpoint conversion ----------------------------------------------------

# app_endpoint_terms_and_definitions
# endpoint_codes

worms <- post_results(
  casrn = '106-93-4',
  eco_group = 'Worms'
) %>%
  left_join(
    .,
    unit_conversion,
    join_by(conc1_unit == orig)
  )

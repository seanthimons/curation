# Tables --------------------------------------------------------------------------------
{
  tbl(eco_con, '')
}
# Testing -------------------------------------------------------------------------------
library(ComptoxR)

query <- ct_list(c('PRODWATER', 'FRACFOCUS', 'EPAHFR', 'EPAHFRTABLE2', 'CALWATERBDS'))
  ct_details(query = .)

query_cas <- ct_search(
  query = 'Spirodiclofen',
  search_method = 'equal',
  request_method = 'GET'
) %>%
  pull(casrn) %>%
  str_remove_all(., "-")

query_cas <- query %>%
  pull(casrn) %>%
  str_remove_all(., "-")

p1 <- post_results(
	casrn = query_cas
)

p1 <- post_results(
  #casrn = '50-00-0',
  eco_group = "Flowers, Trees, Shrubs, Ferns"
  ,endpoint = c('BCF', 'BAF')
)

p1 <- post_results(
	casrn = ComptoxR::testing_chemicals$
)

eco_risk_tbl %>% 
	group_by(eco_group) %>% 
	reframe(
		ex = unique(exposure_group)
	) %>% print(n = Inf)

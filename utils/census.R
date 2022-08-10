library(covidcast)

cc <- covidcast::county_census %>%
  dplyr::filter(COUNTY == 0) %>%
  mutate(state_id = tolower(covidcast::fips_to_abbr(FIPS))) %>%
  rename(pop = POPESTIMATE2019) %>%
  dplyr::select(state_id, pop)

write.csv(cc, "data/us-census-population.csv", row.names = F)
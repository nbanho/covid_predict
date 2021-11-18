library(tidyverse)

dat <- read_csv("data/jhu_all_countries.csv")

dat <- dat %>%
  group_by(id) %>%
  dplyr::filter(confirmed >= 10) %>%
  mutate_at(vars(vaccines, tests, confirmed, recovered, deaths, hosp, vent, icu), function(x) ifelse(is.na(x), 0, x)) %>%
  arrange(date) %>%
  mutate(new_confirmed_raw = dplyr::lead(confirmed) - confirmed) %>%
  slice(-n()) %>%
  ungroup() %>%
  rename(confirmed_raw = confirmed) %>%
  mutate(new_confirmed = ifelse(new_confirmed_raw < 0, NA, new_confirmed_raw)) %>% # Impute negative values
  group_by(id) %>%
  arrange(date) %>%
  mutate(new_confirmed = as.integer(floor(zoo::na.approx(new_confirmed, maxgap = 3)))) %>%
  mutate(confirmed = cumsum(new_confirmed)) %>%
  ungroup() %>%
  dplyr::select(-confirmed_raw, -new_confirmed_raw)


write_csv(dat, "data/prep_jhu_all_countries.csv")
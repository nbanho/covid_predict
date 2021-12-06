# libraries
library(tidyverse)

# load data
dat <- read_csv("data/jhu_all_countries.csv")

# subset data
dat <- dat %>%
  dplyr::select(id, date, population, confirmed)

# start after n_cases cumulative confirmed cases
n_cases = 10
dat <- dat %>%
  group_by(id) %>%
  dplyr::filter(confirmed >= n_cases)

# set missing values to zero
dat <- dat %>%
  mutate(confirmed = ifelse(is.na(confirmed), 0, confirmed))

# add missing dates
dat <- dat %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  tidyr::fill(confirmed, population, .direction = "down")

# compute new confirmed
dat <- dat %>%
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
library(tidyverse)
library(COVID19)

dat <- covid19(level = 1, start = "2020-01-01", end = "2021-05-01") %>%
  dplyr::select(id, date, vaccines, tests, confirmed, recovered, deaths, hosp, vent, icu, population)

write_csv(dat, "data/jhu_all_countries.csv")

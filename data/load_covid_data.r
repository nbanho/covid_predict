library(tidyverse)

# country-level data
library(COVID19)
dat <- covid19(level = 1, start = "2020-01-01", end = "2021-05-01") %>%
  dplyr::select(id, date, vaccines, tests, confirmed, recovered, deaths, hosp, vent, icu, population)

write_csv(dat, "data/jhu_all_countries.csv")

# library(tidycovid19)
# cases <- download_jhu_csse_covid19_data()
# cases <- cases %>%
#   dplyr::filter(iso3c %in% c("AUT", "BEL", "BGR", "CHE", "HRV", "CZE", "DNK", "EST", 
#                           "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
#                           "LTU", "NLD", "POL", "PRT", "ROU", "SVK",
#                           "SVN", "ESP", "SWE", "GBR")) %>%
#   group_by(iso3c) %>%
#   arrange(date) %>%
#   mutate(new_cases = confirmed - dplyr::lag(confirmed)) %>%
#   ungroup() %>%
#   na.omit()
# sum(cases$new_cases<0)


# US data
library(covidcast)
date <- seq.Date(as.Date("2020-06-01"), as.Date("2021-06-01"), by = "day")
tra_data <- list()
tes_data <- list()
n_train <- 60
n_ahead <- 1:21
latent_d <- 1

k <- 0
k_bad <- 0
for (d in 1:length(date)) {
  
  print(sprintf("Day %i of %i", d, length(date)))
  
  # train data
  # signal usually is one day latent
  # thus end_day usually is date[d] - 1
  train_signal <- aggregate_signals(
    covidcast_signals(data_source = rep("jhu-csse", 2),
                      signal = c("confirmed_incidence_num", "confirmed_incidence_prop"),
                      geo_type = "state",
                      start_day = date[d] - n_train,
                      end_day = date[d],
                      as_of = date[d]),
    format = "wide"
  ) %>%
    set_names(c("state_id", "train_date", "cases", "inc")) %>%
    mutate(forecast_date = date[d], cases = c(cases), inc = c(inc)) %>%
    dplyr::select(state_id, forecast_date, train_date, cases, inc) 
  
  # if latency > 1 day, then skip this observation
  if (max(train_signal$train_date) < (date[d]-latent_d)) {
    k_bad <- k_bad + 1
    print(sprintf("End date more than %i latent, skipping observation for forecasting date %s.", 
                    latent_d, as.character(date[d])))
  } else {
    k <- k + 1
    tra_data[[k]] <- train_signal
    
    # test data
    # signal usually is one day latent
    # thus start date is date[d] and note date[d] + 1
    # otherwise there would be a 1-day gap between end and start date using as_of
    # however, the start date should not considered for evaluating forecasting performance
    # as this value is today, becomes known the next, and thus cannot be influenced
    test_signal <- aggregate_signals(
      covidcast_signals(data_source = rep("jhu-csse", 2),
                        signal = c("confirmed_incidence_num", "confirmed_incidence_prop"),
                        geo_type = "state",
                        start_day = date[d] + min(n_ahead) - latent_d,
                        end_day = date[d] + max(n_ahead) - latent_d),
      format = "wide"
    ) %>%
      set_names(c("state_id", "test_date", "cases", "inc")) %>%
      mutate(forecast_date = date[d], cases = c(cases), inc = c(inc)) %>%
      dplyr::select(state_id, forecast_date, test_date, cases, inc) 
    
    tes_data[[k]] <- test_signal
  }
}

# histogram of negative values
negs <- c(sapply(tra_data, function(x) x$inc[x$inc < 0]), sapply(tes_data, function(x) x$inc[x$inc < 0]))
hist(negs %>% unlist())


# impute negatives and NAs
tra_data <- lapply(tra_data, function(df) 
  df %>% 
    group_by(state_id) %>% 
    arrange(train_date) %>% 
    mutate_at(vars(cases, inc), ~ ifelse(. < 0, NA, .)) %>%
    fill(cases, inc, .direction = "downup"))
tes_data <- lapply(tes_data, function(df) 
  df %>% 
    group_by(state_id) %>% 
    arrange(test_date) %>% 
    mutate_at(vars(cases, inc), ~ ifelse(. < 0, NA, .)) %>%
    fill(cases, inc, .direction = "downup"))

# one file per state
states <- unique(tra_data[[1]]$state_id)
write.table(states, file = "data/us-state/states.txt", row.names = F, col.names = F)
for (j in states) {
  tra_data_j <- lapply(tra_data, function(df) df %>% dplyr::filter(state_id == j) %>% arrange(train_date))
  tes_data_j <- lapply(tes_data, function(df) df %>% dplyr::filter(state_id == j) %>% arrange(test_date))
  saveRDS(tra_data_j, file = paste0("data/us-state/", j,"-train.rds"))
  saveRDS(tes_data_j, file = paste0("data/us-state/", j, "-test.rds"))
}

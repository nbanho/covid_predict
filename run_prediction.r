run_prediction <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  id_idx <- as.numeric(args[1:2])
  models <- args[3:length(args)]
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # utils
  source("utils/predict.r")
  source("utils/train.r")
  source("utils/transform.r")
  
  # models
  source("models/cori.r")
  source("models/arima.r")
  source("models/prophet.r")
  
  # data (EU-27) - MLT - LUX - CYP + Uk + CHE
  df <- read_csv("data/prep_jhu_all_countries.csv") %>%
    dplyr::filter(id %in% c("AUT", "BEL", "BGR", "CHE", "HRV", "CZE", "DNK", "EST", 
                            "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                            "LTU", "NLD", "POL", "PRT", "ROU", "SVK",
                            "SVN", "ESP", "SWE", "GBR")[id_idx[1]:id_idx[2]]) %>%
    mutate(incidence = new_confirmed / population * 1e5)
  countries <- unique(df$id)
  
  for (j in 1:length(countries)) {
    
    # data
    ctry <- countries[j]
    df_ctry <- subset(df, id == ctry)
    
    # test data
    test_df_ctry <- df_ctry %>%
      slice((n_train+1):nrow(df_ctry)) %>%
      as_tibble() %>%
      mutate(n_ahead = list(NA), date_ahead = list(NA), 
             new_confirmed_ahead = list(NA), incidence_ahead = list(NA)) 
    if ("arima" %in% models) { test_df_ctry$arima <- list(NA) }
    if ("cori" %in% models) { test_df_ctry$cori <- list(NA) }
    if ("prophet" %in% models) { test_df_ctry$prophet <- list(NA) }
    
    # for EpiEstim, get all R estimates right away
    if ("cori" %in% models) {
      trained_cori <- train(df_ctry$date, df_ctry$new_confirmed, method = "cori",
                            n1 = cori.n1, n2 = cori.n2, seed = seed12345)
    }
    
    # for prophet: set max cap a little bit above maximum observed incidence among all countries
    if ("prophet" %in% models) {
      ctry_cap <- prophet.max_inc * df_ctry$population[1] / 1e5
    }
    
    # loop over test set
    for (k in 1:nrow(test_df_ctry)) {
      
      # train data
      train_df_ctry <- slice(df_ctry, 1:(n_train+k-1))
      # use only last 3 months to fit model, except cori, which uses even less
      # train_df_ctry_last <- tail(train_df_ctry, 90) 
      
      # test data
      pred_df <- slice(test_df_ctry, k:(k-1+n_preds)) 
      max_n <- nrow(pred_df)
      test_df_ctry$n_ahead[[k]] <- 1:max_n
      test_df_ctry$date_ahead[[k]] <- pred_df$date
      test_df_ctry$new_confirmed_ahead[[k]] <- pred_df$new_confirmed
      test_df_ctry$incidence_ahead[[k]] <- pred_df$incidence
      
      # train and predict
      if ("arima" %in% models) {
        
        trained_arima <- train(train_df_ctry$date, 
                               trans(train_df_ctry$incidence, method = trans_y), 
                               method = "arima", 
                               iter = n_sample)
        predicted_arima <- predict(trained_arima,
                                   seed = seed12345)
        predicted_arima <- matrix(predicted_arima[1:max_n,1:n_draws], ncol = n_draws)
        predicted_arima <- inv_trans(predicted_arima, method = trans_y)
        test_df_ctry$arima[[k]] <- predicted_arima
      } 
      if ("prophet" %in% models) {
        trained_prophet <- train(train_df_ctry$date, 
                                 trans(train_df_ctry$incidence, method = trans_y), 
                                 method = "prophet", 
                                 cap = ctry_cap, 
                                 mcmc.samples = n_sample, cores = n_chains,
                                 seed = seed12345)
        predicted_prophet <- predict(trained_prophet)
        predicted_prophet <- matrix(predicted_prophet[1:max_n,1:n_draws], ncol = n_draws)
        predicted_prophet <- inv_trans(predicted_prophet, method = trans_y)
        test_df_ctry$prophet[[k]] <- predicted_prophet
      }
      if ("cori" %in% models) {
        predicted_cori <- predict(trained_cori, 
                                  i = n_train+k-1,
                                  seed = seed12345,
                                  d = n_draws)
        test_df_ctry$cori[[k]] <- matrix(predicted_cori[1:max_n,1:n_draws], ncol = n_draws)
      }
    }
    
    # save
    test_df_ctry <- test_df_ctry %>%
      dplyr::select(-date, -new_confirmed, -incidence) %>%
      rename(n = n_ahead, date = date_ahead, 
             new_confirmed = new_confirmed_ahead, incidence = incidence_ahead) %>%
      dplyr::select(id, population, n, date, new_confirmed, incidence, all_of(models))
    saveRDS(test_df_ctry, paste0("predictions/", ctry, ".rds"))
  }
  
}


run_prediction()
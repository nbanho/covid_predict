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
  
  # models
  source("models/cori.r")
  source("models/arima.r")
  source("models/prophet.r")
  
  # data (EU-27) - MLT - LUX - CYP + Uk + CHE
  df <- read_csv("data/prep_jhu_all_countries.csv") %>%
    dplyr::filter(id %in% c("AUT", "BEL", "BGR", "CHE", "HRV", "CZE", "DNK", "EST", 
                            "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                            "LTU", "NLD", "POL", "PRT", "ROU", "SVK",
                            "SVN", "ESP", "SWE", "GBR")[id_idx[1]:id_idx[2]])
  countries <- unique(df$id)
  
  for (j in 1:length(countries)) {
    
    # data
    ctry <- countries[j]
    df_ctry <- subset(df, id == ctry)
    
    # test data
    test_df_ctry <- slice(df_ctry, (n_train+1):nrow(df_ctry))
    
    # for EpiEstim, get all R estimates right away
    if ("cori" %in% models) {
      trained_cori <- train(df_ctry$date, df_ctry$new_confirmed, method = "cori",
                            n1 = cori.n1, n2 = cori.n2, seed = seed12345)
    }
    
    # for prophet: set max cap a little bit above maximum observed incidence among all countries
    if ("prophet" %in% models) {
      ctry_cap <- prophet.max_inc * df_ctry$population[1] / 1e5
    }
    
    # array to store predictions
    test_df_ctry <- as_tibble(test_df_ctry)
    if ("arima" %in% models) { test_df_ctry$arima <- list(NA) }
    if ("prophet" %in% models) { test_df_ctry$arima <- list(NA) }
    if ("cori" %in% models) { test_df_ctry$arima <- list(NA) }
    
    # loop over test set
    for (k in 1:nrow(test_df_ctry)) {
      
      # train data
      train_df_ctry <- slice(df_ctry, 1:(n_train+k-1))
      # use only last 4 months of data to fit, cori will even take less
      train_df_ctry120 <- tail(train_df_ctry, 120) 
      
      # test data
      pred_df <- slice(df_ctry, (n_train+k):(n_train+k-1+n_preds)) 
      
      # train and predict
      if ("arima" %in% models) {
        
        trained_arima <- train(train_df_ctry120$date, train_df_ctry120$new_confirmed, method = "arima", 
                               iter = n_sample)
        predicted_arima <- predict(trained_arima,
                                   seed = seed12345)
        test_df_ctry$arima[[k]] <- matrix(predicted_arima[1:nrow(pred_df), ], ncol = n_draws)
      } 
      if ("prophet" %in% models) {
        trained_prophet <- train(train_df_ctry120$date, train_df_ctry120$new_confirmed, method = "prophet", 
                                 cap = ctry_cap, mcmc.samples = n_sample, cores = n_chains,
                                 show_messages = F, verbose = F,
                                 seed = seed12345)
        predicted_prophet <- predict(trained_prophet)
        test_df_ctry$prophet[[k]] <- matrix(predicted_prophet[1:nrow(pred_df), ], ncol = n_draws)
      }
      if ("cori" %in% models) {
        predicted_cori <- predict(trained_cori, i = n_preds+k-1, seed = seed12345)
        test_df_ctry$cori[[k]] <- matrix(predicted_cori[1:nrow(pred_df), ], ncol = n_draws)
      }
    }
    
    # save
    saveRDS(test_df_ctry, paste0("predictions/", ctry, ".rds"))
  }
  
}


run_prediction()
run_prediction <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  id_idx <- args[1:2]
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
  
  # data (EU-27)
  df <- read_csv("data/prep_jhu_all_countries.csv") %>%
    dplyr::filter(id %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
                            "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                            "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
                            "SVN", "ESP", "SWE", "GBR")[id_idx[1]:id_idx[2]])
  countries <- unique(df$id)
  
  for (j in 1:length(countries)) {
    
    # data
    ctry <- countries[j]
    df_ctry <- subset(df, id == ctry)
    
    # test data
    test_df_ctry <- slice(df_ctry, (n_train+1):nrow(df_ctry))
    
    if ("cori" %in% models) {
      # for EpiEstim, get all R estimates right away
      train_ctry <- train(df_ctry$date, df_ctry$new_confirmed, method = "cori")
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
      
      # test data
      pred_df <- slice(df_ctry, (n_train+k):(n_train+k-1+n_preds)) 
      
      # train and predict
      if ("arima" %in% models) {
        train_df_ctry120 <- tail(train_df_ctry, 120) 
        trained_arima <- train(train_df_ctry120$date, train_df_ctry120$new_confirmed, method = "arima")
        predicted_arima <- predict(trained_arima)
        test_df_ctry$arima[[k]] <- predicted_arima[1:nrow(pred_df), ]
      } 
      if ("prophet" %in% models) {
        trained_prophet <- train(train_df_ctry$date, train_df_ctry$new_confirmed, method = "prophet")
        predicted_prophet <- predict(trained_prophet)
        test_df_ctry$prophet[[k]] <- predicted_prophet[1:nrow(pred_df), ]
      }
      if ("cori" %in% models) {
        train_df_ctry_cori <- slice(df_ctry, (k):(n_train+k-1))
        trained_cori <- train(train_df_ctry_cori$date, train_df_ctry_cori$new_confirmed, method = "cori")
        prev_inc <- create_inc(train_df_ctry_cori$new_confirmed, train_df_ctry_cori$date)
        predicted_cori <- predict(trained_cori, inc = prev_inc, i = n_preds+k-1)
        test_df_ctry$cori[[k]] <- predicted_cori[1:nrow(pred_df), ]
      }
    }
    
    # save
    saveRDS(test_df_ctry, paste0("predictions/", ctry, ".rds"))
  }
  
}


run_prediction()
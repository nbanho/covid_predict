run_prediction <- function() {
  
  # settings
  models <- commandArgs(trailingOnly = TRUE)
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
                            "SVN", "ESP", "SWE", "GBR"))
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
    pred_list <- list()
    
    # loop over test set
    for (k in 1:nrow(test_df_ctry)) {
      
      # train data
      train_df_ctry <- slice(df_ctry, 1:(n_train+k-1))
      
      # test data
      pred_df <- df_ctry %>%
        slice((n_train+k):(n_train+k-1+n_preds)) 
      
      # train and predict
      if ("arima" %in% models) {
        trained_arima <- train(train_df_ctry$date, train_df_ctry$new_confirmed, method = "arima")
        predicted_arima <- predict(trained_arima, method = "arima")
        pred_df$pred_new_confirmed_arima <- rowMeans(predicted_arima)[1:nrow(pred_df)]
      } 
      if ("prophet" %in% models) {
        trained_prophet <- train(train_df_ctry$date, train_df_ctry$new_confirmed, method = "prophet")
        predicted_prophet <- predict(trained_prophet, method = "prophet")
        pred_df$pred_new_confirmed_prophet <- rowMeans(predicted_prophet)[1:nrow(pred_df)]
      }
      if ("cori" %in% models) {
        train_df_ctry_cori <- slice(df_ctry, (k):(n_train+k-1))
        trained_cori <- train(train_df_ctry_cori$date, train_df_ctry_cori$new_confirmed, method = "cori")
        prev_inc <- create_inc(train_df_ctry_cori$new_confirmed, train_df_ctry_cori$date)
        predicted_cori <- predict(trained_cori, method = "cori", inc = prev_inc, i = n_preds+k-1)
        pred_df$pred_new_confirmed_cori <- rowMeans(predicted_cori)[1:nrow(pred_df)]
      }
    
      # store predictions
      pred_df$n_ahead <- 1:nrow(pred_df)
      pred_list[[k]] <- pred_df
    }
    
    # combine and save
    pred_df <- do.call(rbind, pred_list)
    write_csv(pred_df, paste0("predictions/", ctry, ".csv"))
  }
  
}


run_prediction()
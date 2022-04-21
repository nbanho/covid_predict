run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  states <- unlist(stringr::str_split(args[1], "8"))
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # models
  source("models/cori.r")
  
  # states
  if (grepl("all", states)) {
    states <- unique(stringi::stri_extract(list.files("data/us-selected-states/"), regex = "\\w{2}")) 
  } 
  
  for (j in 1:length(states)) {
    
    # train and data
    state <- states[j]
    train_df_state <- readRDS(paste0("data/us-selected-states/", state, "-train.rds")) 
    test_df_state <- readRDS(paste0("data/us-selected-states/", state, "-test.rds"))
    
    # loop over forecasting dates
    K <- nrow(train_df_state)
    for (k in 1:K) {
      
      # train and forecast for each model
      if (all(train_df_state$data[[k]]$incidence==0)) {
        # insert 0 predictions
        predicted_cori <- matrix(0, nrow = n_preds, ncol = n_draws)
        test_df_state$data[[k]]$cori7 <- get_samples(predicted_cori, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$cori14 <- get_samples(predicted_cori, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$cori21 <- get_samples(predicted_cori, test_df_state$data[[k]], k)
      } else {
        # train
        trained_cori7 <- train.cori(train_df_state$data[[k]]$cases, tau = 7)
        trained_cori14 <- train.cori(train_df_state$data[[k]]$cases, tau = 14)
        trained_cori21 <- train.cori(train_df_state$data[[k]]$cases, tau = 21)
        
        # predict
        predicted_cori7 <- predict.cori(trained_cori7, seed = seed12345)
        predicted_cori14 <- predict.cori(trained_cori14, seed = seed12345)
        predicted_cori21 <- predict.cori(trained_cori21, seed = seed12345)
        
        # save
        test_df_state$data[[k]]$cori7 <- get_samples(predicted_cori7, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$cori14 <- get_samples(predicted_cori14, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$cori21 <- get_samples(predicted_cori21, test_df_state$data[[k]], k)
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("predictions/experiment_cori/", state, ".rds"))
  }
  
}


run_prediction_us()
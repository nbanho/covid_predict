run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  states <- unlist(stringr::str_split(args[1], "8"))
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # models
  source("models/prophet.r")
  cp_scale <- seq(0.05, 0.45, 0.1)
  
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
      if (all(train_df_state$data[[k]]$incidence<=min_inc)) {
        # insert 0 predictions
        predicted_prophet <- matrix(0, nrow = n_preds, ncol = n_draws)
        for (tau in cp_scale) {
          test_df_state$data[[k]][[paste0("prophet-",tau)]] <- get_samples(predicted_prophet, test_df_state$data[[k]], k)
        }
      } else {
        for (tau in cp_scale) {
          test_df_state$data[[k]][[paste0("prophet-",tau)]] <- get_samples(
            train_and_predict.prophet(cp_scale = tau, data = train_df_state$data[[k]], 
                                      seed = seed12345, n = n_preds, d = n_draws),
            test_df_state$data[[k]], k
          )
        }
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("experiments/prophet/", state, ".rds"))
  }
  
}


run_prediction_us()
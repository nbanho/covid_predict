run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  states <- unlist(stringr::str_split(args[1], "8"))
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # models
  source("models/prophet.r")
  
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
      # transform data
      dat_proph <- train_df_state$data[[k]] %>% 
        mutate(y = log1p(incidence)) %>%
        rename(ds = date) %>%
        dplyr::select(ds, y)
      
      # train and forecast for each model
      if (all(train_df_state$data[[k]]$incidence==0)) {
        # insert 0 predictions
        predicted_proph <- matrix(0, nrow = n_preds, ncol = n_draws)
        test_df_state$data[[k]]$proph0.10 <- get_samples(predicted_proph, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$proph0.25 <- get_samples(predicted_proph, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$proph0.40 <- get_samples(predicted_proph, test_df_state$data[[k]], k)
      } else {
        # train
        trained_proph0.10 <- train.prophet(dat_proph, cp_scale = 0.10, seed = seed12345, mcmc.samples = n_sample, cores = n_chains)
        trained_proph0.25 <- train.prophet(dat_proph, cp_scale = 0.25, seed = seed12345, mcmc.samples = n_sample, cores = n_chains)
        trained_proph0.40 <- train.prophet(dat_proph, cp_scale = 0.40, seed = seed12345, mcmc.samples = n_sample, cores = n_chains)
        
        # predict
        predicted_proph0.10 <- predict.prophet(trained_proph0.10, seed = seed12345)
        predicted_proph0.25 <- predict.prophet(trained_proph0.25, seed = seed12345)
        predicted_proph0.40 <- predict.prophet(trained_proph0.40, seed = seed12345)
        
        # save
        test_df_state$data[[k]]$proph0.10 <- expm1(get_samples(predicted_proph0.10, test_df_state$data[[k]], k))
        test_df_state$data[[k]]$proph0.25 <- expm1(get_samples(predicted_proph0.25, test_df_state$data[[k]], k))
        test_df_state$data[[k]]$proph0.40 <- expm1(get_samples(predicted_proph0.40, test_df_state$data[[k]], k))
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("predictions/experiment_prophet/", state, ".rds"))
  }
  
}


run_prediction_us()
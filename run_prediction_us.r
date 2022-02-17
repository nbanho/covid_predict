run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  state <- unlist(str_split(args[1], "8"))
  models <- unlist(str_split(args[2], "8"))
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # models
  source("models/cori.r")
  #source("models/epidemia.r")
  source("models/arima.r")
  source("models/prophet.r")
  source("models/gp.r")
  
  # states
  if (grepl("all", state)) {
    states <- unique(stringi::stri_extract(list.files("data/us-selected-states/"), regex = "\\w{2}")) 
  } else {
    states <- state
  }
  
  for (j in 1:length(states)) {
    
    # train and data
    state <- states[j]
    train_df_state <- readRDS(paste0("data/us-selected-states/", state, "-train.rds")) 
    test_df_state <- readRDS(paste0("data/us-selected-states/", state, "-test.rds"))
    
    # loop over forecasting dates
    K <- nrow(train_df_state)
    for (k in 1:21) {
      
      # train and forecast for each model
      if ("cori" %in% models) {
        if (all(train_df_state$data[[k]]$incidence==0)) {
          predicted_cori <- matrix(0, nrow = n_preds, ncol = n_draws)
        } else {
          # train
          trained_cori <- train.cori(train_df_state$data[[k]]$cases)
          
          # predict
          predicted_cori <- predict.cori(trained_cori, seed = seed12345)
        }
        test_df_state$data[[k]]$cori <- get_samples(predicted_cori, test_df_state$data[[k]], k)
      }
      
      if ("arima" %in% models) {
        if (all(train_df_state$data[[k]]$incidence==0)) {
          predicted_arima <- matrix(0, nrow = n_preds, ncol = n_draws)
        } else {
          # train
          trained_arima <- train.arima(log1p(train_df_state$data[[k]]$incidence), iter = n_sample)
          
          # predict
          predicted_arima <- predict.arima(trained_arima, seed = seed12345)
          
        }
        predicted_arima <- get_samples(predicted_arima, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$arima <- expm1(predicted_arima)
      } 
      
      if ("prophet" %in% models) {
        if (all(train_df_state$data[[k]]$incidence==0)) {
          predicted_prophet <- matrix(0, nrow = n_preds, ncol = n_draws)
        } else {
          # train
          dat_proph <- train_df_state$data[[k]] %>% 
            mutate(y = log1p(incidence)) %>%
            rename(ds = date) %>%
            dplyr::select(ds, y)
          trained_prophet <- train.prophet(dat_proph, seed = seed12345, mcmc.samples = n_sample, cores = n_chains)
          
          # predict
          predicted_prophet <- predict.prophet(trained_prophet)
        }
        predicted_prophet <- get_samples(predicted_prophet, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$prophet <- expm1(predicted_prophet)
      }
      
      if ("gp" %in% models) {
        if (all(train_df_state$data[[k]]$incidence==0)) {
          predicted_gp <- matrix(0, nrow = n_preds, ncol = n_draws)
        } else {
          # train
          trained_gp <- train.gp(log1p(train_df_state$data[[k]]$incidence), seed = seed12345, chains = n_chains, 
                                 parallel_chains = n_chains, threads_per_chain = n_chains / 2,
                                 iter = n_sample)
          
          # predict
          predicted_gp <- predict.gp(trained_gp)
        }
        predicted_gp <- get_samples(predicted_gp, test_df_state$data[[k]], k)
        test_df_state$data[[k]]$gp <- expm1(predicted_gp)
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("predictions/", state, ".rds"))
  }
  
}


run_prediction_us()
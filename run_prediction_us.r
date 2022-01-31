run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  state <- args[1]
  models <- args[2:length(args)]
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
    for (k in 1:nrow(train_df_state)) {
      
      # train and forecast for each model
      if ("cori" %in% models) {
        # train
        trained_cori <- train.cori(train_df_state$data[[k]]$cases)
        
        # predict
        predicted_cori <- predict.cori(trained_cori, seed = seed12345)
        test_df_state$data[[k]]$cori <- matrix(predicted_cori[1:n_preds,1:n_draws], ncol = n_draws)
      }
      
      if ("arima" %in% models) {
        # train
        trained_arima <- train.arima(log1p(train_df_state$data[[k]]$incidence), iter = n_sample)
        
        # predict
        predicted_arima <- predict.arima(trained_arima, seed = seed12345)
        predicted_arima <- matrix(predicted_arima[1:n_preds,1:n_draws], ncol = n_draws)
        test_df_state$data[[k]]$arima <- expm1(predicted_arima)
      } 
      
      if ("prophet" %in% models) {
        # train
        dat_proph <- train_df_state$data[[k]] %>% 
          mutate(y = log1p(incidence)) %>%
          rename(ds = date) %>%
          dplyr::select(ds, y)
        trained_prophet <- train.prophet(dat_proph, seed = seed12345, mcmc.samples = n_sample, cores = n_chains)
        
        # predict
        predicted_prophet <- predict.prophet(trained_prophet)
        predicted_prophet <- matrix(predicted_prophet[1:n_preds,1:n_draws], ncol = n_draws)
        test_df_state$data[[k]]$prophet <- expm1(predicted_prophet)
      }
      
      if ("gp" %in% models) {
        # train
        trained_gp <- train.gp(log1p(train_df_state$data[[k]]$incidence), seed = seed12345, chains = n_chains, 
                               parallel_chains = n_chains, threads_per_chain = n_chains / 2,
                               iter = n_sample)
        
        # predict
        predicted_gp <- predict.gp(trained_gp)
        predicted_gp <- matrix(predicted_gp[1:n_preds,1:n_draws], ncol = n_draws)
        test_df_state$data[[k]]$gp <- expm1(predicted_gp)
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("predictions/", state, ".rds"))
  }
  
}


run_prediction_us()
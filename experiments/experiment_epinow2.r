run_prediction_us <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  states <- unlist(stringr::str_split(args[1], "8"))
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # state population
  cc <- read_csv("data/us-census-population.csv")
  
  # models
  source("models/epinow2.r")
  est_rt_opts <- c("backcalc", "rw", "gp")
  
  # states
  if (grepl("all", states)) {
    states <- unique(stringi::stri_extract(list.files("data/us-selected-states/"), regex = "\\w{2}")) 
  } 
  
  for (j in 1:length(states)) {
    
    # train and data
    state <- states[j]
    train_df_state <- readRDS(paste0("data/us-selected-states/", state, "-train.rds")) 
    test_df_state <- readRDS(paste0("data/us-selected-states/", state, "-test.rds"))
    
    # pop for adjustments
    pop <- cc$pop[toupper(cc$state_id) == state]
    
    # loop over forecasting dates
    K <- nrow(train_df_state)
    for (k in 1:K) {
      # train and forecast for each model
      if (all(train_df_state$data[[k]]$incidence<=min_inc)) {
        # insert 0 predictions
        predicted_epinow2 <- matrix(0, nrow = n_preds, ncol = n_draws)
        for (ero in est_rt_opts) {
          test_df_state$data[[k]][[paste0("epinow2-",ero)]] <- get_samples(predicted_epinow2, test_df_state$data[[k]], k)
        }
      } else {
        for (ero in est_rt_opts) {
          test_df_state$data[[k]][[paste0("epinow2-",ero)]] <- get_samples(
            train_and_predict.epinow2(est_rt = ero, data = train_df_state$data[[k]], 
                                      seed = seed12345, n = n_preds, d = n_draws,
                                      pop = pop),
            test_df_state$data[[k]], k
          )
        }
      }
    }
    
    # save
    saveRDS(test_df_state, paste0("experiments/epinow2/", state, ".rds"))
  }
  
}


run_prediction_us()
run_prediction <- function() {
  # arguments
  args <- commandArgs(trailingOnly = TRUE)
  states <- unlist(stringr::str_split(args[1], "8"))
  models <- unlist(stringr::str_split(args[2], "8"))
  
  # settings
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # models
  source("models/epiestim.r")
  source("models/epinow2.r")
  #source("models/epidemia.r")
  source("models/arima.r")
  source("models/prophet.r")
  source("models/gp.r")
  
  # state population
  cc <- read_csv("data/us-census-population.csv")
  
  if (grepl("all", models[1])) {
    models <- c("epiestim", "epinow2", "arima", "prophet", "gp")
  }
  
  # states
  if (grepl("all", states[1])) {
    states <- unique(stringi::stri_extract(list.files("data/us-selected-states/"), regex = "\\w{2}")) 
  } 
  
  for (mod in models) {
    for (j in 1:length(states)) {
      # train and data
      state <- states[j]
      train_df_state <- readRDS(paste0("data/us-selected-states/", state, "-train.rds")) 
      test_df_state <- readRDS(paste0("data/us-selected-states/", state, "-test.rds"))
      
      # loop over forecasting dates
      K <- nrow(train_df_state)
      
      for (k in 1:K) {
        if (all(train_df_state$data[[k]]$incidence<=0.1)) {
          predicted <- matrix(0, nrow = n_preds, ncol = n_draws)
        } else {
          # train and predict
          predicted <- train_and_predict(model = mod, data = train_df_state$data[[k]], 
                                         seed = seed12345, n = n_preds, d = n_draws,
                                         pop = cc$pop[toupper(cc$state_id) == state])
        }
        test_df_state$data[[k]]$forecast <- get_samples(predicted, test_df_state$data[[k]], k)
      }
      
      # save
      saveRDS(test_df_state, paste0("predictions/", state, "_", mod, ".rds"))
    }
  } 
}

run_prediction()
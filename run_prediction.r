run_prediction <- function() {
  
  # settings
  args <- commandArgs(trailingOnly = TRUE)
  id_idx <- as.numeric(args[1:2])
  models <- args[3:length(args)]
  source("settings/defaults.r")
  
  # libraries 
  library(tidyverse)
  
  # utils
  source("utils/diagnostics.r")
  source("utils/transform.r")
  
  # models
  source("models/cori.r")
  source("models/epidemia.r")
  source("models/arima.r")
  source("models/prophet.r")
  source("models/gp.r")
  
  # data (EU-27) - MLT - LUX - CYP + Uk + CHE
  df <- read_csv("data/prep_jhu_all_countries.csv") %>%
    dplyr::filter(id %in% c("AUT", "BEL", "BGR", "CHE", "HRV", "CZE", "DNK", "EST", 
                            "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                            "LTU", "NLD", "POL", "PRT", "ROU", "SVK",
                            "SVN", "ESP", "SWE", "GBR")[id_idx[1]:id_idx[2]]) %>%
    mutate(log_inc = trans(new_confirmed, population)) 
  countries <- unique(df$id)
  
  for (j in 1:length(countries)) {
    
    # data
    ctry <- countries[j]
    df_ctry <- subset(df, id == ctry) 
    
    # test data
    test_df_ctry <- df_ctry %>%
      slice((n_train+1):nrow(df_ctry)) %>%
      as_tibble() %>%
      mutate(n_ahead = list(NA), date_ahead = list(NA), new_confirmed_ahead = list(NA)) 
    if ("arima" %in% models) { 
      test_df_ctry$arima <- list(NA)
      test_df_ctry$diagn_arima <- list(NA)
    }
    if ("cori" %in% models) { test_df_ctry$cori <- list(NA) }
    if ("epidemia" %in% models) { test_df_ctry$epidemia <- list(NA) }
    if ("prophet" %in% models) { test_df_ctry$prophet <- list(NA) }
    if ("gp" %in% models) { test_df_ctry$gp <- list(NA) }
    
    # for EpiEstim, get all R estimates right away
    if ("cori" %in% models) {
      trained_cori <- train.cori(df_ctry$new_confirmed,
                                 seed = seed12345)
    }
    
    # for prophet: set max cap a little bit above maximum observed incidence among all countries
    # if ("prophet" %in% models) {
    #   ctry_cap <- sqrt(prophet.max_inc)
    # }
    
    # loop over test set
    for (k in 1:nrow(test_df_ctry)) {
      
      # train data
      train_df_ctry <- slice(df_ctry, 1:(n_train+k-1))
      # use only last 3 months to fit model, except cori, which effectively uses even less
      train_df_ctry_last <- tail(train_df_ctry, 90) 
      
      # test data
      pred_df <- slice(test_df_ctry, k:(k-1+n_preds)) 
      max_n <- nrow(pred_df)
      test_df_ctry$n_ahead[[k]] <- 1:max_n
      test_df_ctry$date_ahead[[k]] <- pred_df$date
      test_df_ctry$new_confirmed_ahead[[k]] <- pred_df$new_confirmed
      
      # train and predict for each model
      if ("arima" %in% models) {
        # train
        trained_arima <- train.arima(train_df_ctry_last$log_inc,
                                     iter = n_sample)
        
        # diagnostics
        no_of_ar <- n_ar(trained_arima)
        no_of_ma <- n_ma(trained_arima)
        max_Rhat <- max.Rhat(trained_arima)
        prop_div_trans <- share_divergent(trained_arima$stanfit)
        test_df_ctry$diagn_arima[[k]] <- list(no_of_ar, no_of_ma, max_Rhat, prop_div_trans)
        
        # predict
        predicted_arima <- predict.arima(trained_arima,
                                         seed = seed12345)
        predicted_arima <- matrix(predicted_arima[1:max_n,1:n_draws], ncol = n_draws)
        predicted_arima <- inv_trans(predicted_arima, test_df_ctry$population[1])
        test_df_ctry$arima[[k]] <- predicted_arima
      } 
      
      if ("prophet" %in% models) {
        # train
        trained_prophet <- train.prophet(train_df_ctry_last %>% 
                                           rename(y = log_inc, ds = date) %>%
                                           dplyr::select(ds, y), 
                                         # cap = ctry_cap, 
                                         seed = seed12345,
                                         mcmc.samples = n_sample, cores = n_chains)
        
        # predict
        predicted_prophet <- predict.prophet(trained_prophet)
        predicted_prophet <- matrix(predicted_prophet[1:max_n,1:n_draws], ncol = n_draws)
        predicted_prophet <- inv_trans(predicted_prophet, test_df_ctry$population[1])
        test_df_ctry$prophet[[k]] <- predicted_prophet
      }
      
      if ("cori" %in% models) {
        # predict
        predicted_cori <- predict.cori(trained_cori, 
                                       i = n_train+k-1,
                                       seed = seed12345)
        test_df_ctry$cori[[k]] <- matrix(predicted_cori[1:max_n,1:n_draws], ncol = n_draws)
      }
      
      if ("epidemia" %in% models) {
        # train
        trained_epidemia <- train.epidemia(train_df_ctry %>% mutate(cases = new_confirmed),
                                           iter = n_sample, cores = n_chains,
                                           seed = seed12345)
        
        # predict
        predicted_epidemia <- predict.epidemia(trained_epidemia)
        predicted_epidemia <- matrix(predicted_epidemia[1:max_n,1:n_draws], ncol = n_draws)
        test_df_ctry$epidemia[[k]] <- predicted_epidemia
      }
      
      if ("gp" %in% models) {
        trained_gp <- train.gp(train_df_ctry_last$log_inc,
                               seed = seed12345,
                               chains = n_chains, parallel_chains = n_chains, threads_per_chain = n_chains / 2,
                               iter = n_sample)
        predicted_gp <- predict.gp(trained_gp)
        predicted_gp <- matrix(predicted_gp[1:max_n,1:n_draws], ncol = n_draws)
        predicted_gp <- inv_trans(predicted_gp, test_df_ctry$population[1])
        test_df_ctry$gp[[k]] <- predicted_gp
      }
    }
    
    # save
    test_df_ctry <- test_df_ctry %>%
      dplyr::select(-date, -new_confirmed) %>%
      rename(n = n_ahead, date = date_ahead, new_confirmed = new_confirmed_ahead) %>%
      dplyr::select(id, population, n, date, new_confirmed, all_of(models), contains("diagn_arima"))
    saveRDS(test_df_ctry, paste0("predictions/", ctry, ".rds"))
  }
  
}


run_prediction()
# default settings
source("settings/defaults.r")

# predict target
predict <- function(
  train_obj, # training object
  method = "cori", # method to predict
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ... # additional models-pecific parameters 
  ) {
  
  if (method == "cori") {
    
    preds <- predict.cori(estimate_R_obj = train_obj, n = n, d = d, ...)
    
  } else if (method == "arima") {
    
    preds <- predict.arima(varstan_obj = train_obj, n = n, d = d, ...)
    
  } else if (method == "prophet") {
    
    preds <- predict.prophet(prophet_obj = train_obj, n = n, d = d, ...)
    
  }
  
  return(preds)
}

# plot observed vs predicted target
plot.predict <- function(
  date, # date
  target, # target
  pred # matrix with posterior predictions
) {
  
  # true
  true <- data.frame(date = as.Date(date), value = target)
  
  # number of predictions
  n <- nrow(pred)
  
  # predictions as data frame
  pred_df <- data.frame(t(pred)) %>%
    set_names(as.character(tail(true, n)$date)) %>%
    gather() %>%
    mutate(key = as.Date(key))
  
  ggplot() +
    geom_line(data = true, mapping = aes(x = date, y = value)) +
    stat_lineribbon(data = pred_df, mapping = aes(x = key, y = value), .width = c(.95, .80, .50), alpha = 1/4, color = "blue") +
    theme_bw()
  
}

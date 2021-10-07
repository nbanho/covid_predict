library(prophet)

train.prophet <- function(
  dsy, # df with date column (ds) and time series (y) 
  ... # additional model parameters
) {
  
  fit <- prophet(dsy, daily.seasonality = T, ...)  
  
  return(fit)
}


predict.prophet <- function(
  prophet_obj, # train object from train.prophet
  n = n_preds, # number of days to project into the future
  ... # additional parameters
) {
  
  future <- make_future_dataframe(prophet_obj, periods = n)
  pred <- predictive_samples(prophet_obj, future)
  pred <- tail(pred$yhat, n)
  
  return(pred)
}

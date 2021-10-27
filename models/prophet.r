library(prophet)

train.prophet <- function(
  dsy, # df with date column (ds) and time series (y) 
  cap,
  floor = 0,
  ... # additional model parameters
) {
  
  # set floor
  dsy$floor <- floor
  dsy$cap <- cap 
  
  # fit
  fit <- prophet(dsy, growth = "logistic", 
                 yearly.seasonality = F, weekly.seasonality = F, daily.seasonality = F,
                 ...)  
  
  return(fit)
}


predict.prophet <- function(
  prophet_obj, # train object from train.prophet
  n = 1, # number of days to project into the future
  ... # additional parameters
) {
  
  future <- make_future_dataframe(prophet_obj, periods = n)
  future$cap <- prophet_obj$history$cap[1]
  future$floor <- prophet_obj$history$floor[1]
  pred <- predictive_samples(prophet_obj, future)
  pred <- tail(pred$yhat, n)
  
  return(pred)
}

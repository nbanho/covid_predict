library(bayesforecast)
library(rstan)

train.arima <- function(
  y, # number of new confirmed cases
  ... # additional parameters to auto.arima 
  # note it is currently not possible to provide a seed to auto.sarima in the call to varstan
) {
  
  fit <- auto.sarima(y, ...)  
    
  return(fit)
}


predict.arima <- function(
  varstan_obj, # train object from train.arima
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ... # additional parameters
) {
  
  pred <- t(as.matrix(posterior_predict(varstan_obj, h = n, draws = d, ...)))
  
  return(pred)
}

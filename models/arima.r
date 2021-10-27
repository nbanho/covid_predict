library(bayesforecast)
library(rstan)

train.arima <- function(
  y, # number of new confirmed cases
  ... # additional model parameters
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

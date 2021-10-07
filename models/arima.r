library(bayesforecast)
library(rstan)

train.arima <- function(
  y, # number of new confirmed cases
  seed0 = seed12345, # seed
  ... # additional model parameters
) {
  
  set.seed(seed0) # don't think this will work as varstan is calling rstan without seed argument
  fit <- auto.sarima(y, iter = 2000, ...)  
    
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

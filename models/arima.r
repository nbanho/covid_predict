library(bayesforecast)
library(rstan)

# Train ARIMA model
train.arima <- function(
  y, # number of new confirmed cases
  ... # additional parameters to auto.arima 
  # note it is currently not possible to provide a seed to auto.sarima in the call to varstan
) {
  
  fit <- auto.sarima(ts = ts(y, frequency = 7), max.p = 1, max.d = 2, max.q = 1, max.P = 0, max.Q = 1, max.D = 1, stepwise = F, seasonal = T, ...)
    
  return(fit)
}

# number of AR
n_ar <- function(arima_model) {
  sum( grepl( "ar", row.names(summary(arima_model)) ) )
}


# number of MA
n_ma <- function(arima_model) {
  sum( grepl( "ma", row.names(summary(arima_model)) ) ) - 1
}


# Predict with ARIMA model
predict.arima <- function(
  varstan_obj, # train object from train.arima
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ... # additional parameters
) {
  
  pred <- t(as.matrix(posterior_predict(varstan_obj, h = n, draws = d, ...)))
  
  return(pred)
}

# libraries
library(bayesforecast)
library(rstan)

#' @title train and predict using Bayesian arima
#' 
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the incidence

train_and_predict.arima <- function(pdq_order, ...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  # time series
  y <- log1p(args$data$incidence)
  
  # set seed 
  set.seed(args$seed)
  
  # fit sarima model
  fit <- stan_sarima(ts = ts(y, frequency = 7), order = pdq_order, seasonal = c(0,1,0), iter = args$d / 2)
  
  # forecast
  pred <- t(as.matrix(posterior_predict(fit, h = args$n, draws = args$d)))
  
  # transform
  fcast <- expm1(pred)
  
  return(fcast)
}

# libraries
library(prophet)

#' @title train and predict using prophet
#' 
#' @param cp_scale value for the changepoint prior scale
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the incidence

train_and_predict.prophet <- function(cp_scale, ...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  # data
  dsy <- data.frame(ds = args$data$date,
                    y = log1p(args$data$incidence))
                    
  
  # change points
  n <- nrow(dsy)
  cpr <- (args$n - 7) / args$n
  nc <- args$n / 14
  
  # fit
  fit <- prophet(dsy, 
                 growth = "linear",
                 changepoint.range = cpr, n.changepoints = nc, changepoint.prior.scale = cp_scale,
                 yearly.seasonality = F, weekly.seasonality = 'auto', daily.seasonality = F, seasonality.mode = "multiplicative",
                 seed = args$seed, mcmc.samples = args$d, cores = 4)  
  
  # predict
  future <- make_future_dataframe(fit, periods = args$n)
  pred <- predictive_samples(fit, future)
  pred <- tail(pred$yhat, args$n)
  
  # transform
  fcast <- expm1(pred)
  
  return(fcast)
}
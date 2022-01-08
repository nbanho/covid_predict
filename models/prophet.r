library(prophet)

train.prophet <- function(
  dsy, # df with date column (ds) and time series (y) 
  cap = NULL, # upper limit
  floor = NULL, # lower limit
  weekly = T, # consider weekly seasonality
  ... # additional model parameters
) {
  
  # set cap and floor for logistic growth trend
  if (!is.null(cap)) { 
    gr <- "logistic"
    dsy$cap <- cap
  } 
  else {
    gr <- "linear" 
  }
  if (!is.null(floor)) { 
    dsy$floor <- floor 
  }
   
  # change points
  n <- nrow(dsy)
  cpr <- (n - 7) / n
  nc <- n / 14
  
  # fit
  if (weekly) {
    fit <- prophet(dsy, 
                   growth = gr,
                   changepoint.range = cpr, n.changepoints = nc, changepoint.prior.scale = 0.25,
                   yearly.seasonality = F, weekly.seasonality = 'auto', daily.seasonality = F, seasonality.mode = "multiplicative",
                   ...)  
  } else {
    fit <- prophet(dsy, 
                   growth = gr,
                   changepoint.range = cpr, n.changepoints = nc, changepoint.prior.scale = 0.25,
                   yearly.seasonality = F, weekly.seasonality = F, daily.seasonality = F,
                   ...)  
  }
  
  
  return(fit)
}


predict.prophet <- function(
  prophet_obj, # train object from train.prophet
  n = n_preds, # number of days to project into the future
  ... # additional parameters
) {
  
  future <- make_future_dataframe(prophet_obj, periods = n)
  if (!is.null(prophet_obj$history$cap[1])) {future$cap <- prophet_obj$history$cap[1]}
  if (!is.null(prophet_obj$history$floor[1])) {future$floor <- prophet_obj$history$floor[1]}
  pred <- predictive_samples(prophet_obj, future)
  pred <- tail(pred$yhat, n)
  
  return(pred)
}

# default settings
source("settings/defaults.r")

# predict number of new confirmed cases
predict_cases <- function(
  train_obj, # training object
  train_dat = NULL, # training data (default = NULL, i.e. can be read from train_obj or not needed)
  n = n_days, # number of days to project into the future
  d = n_draws, # number of posterior draws
  method = "renewable_cori",
  ... # additional models-pecific parameters 
  ) {
  
  # model based on renewable equation following Cori et al 2013 (EpiEstim)
  if (grepl("renewable_cori", method)) {
    preds <- predict.renewable_cori(estimate_R_obj = train_obj, inc = train_dat, n = n, d = d, ...)
  }
  
  return(preds)
}

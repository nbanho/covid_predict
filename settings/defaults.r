# min incidence for estimation
min_inc <- 1

# seed
seed12345 = 12345 # currently not used

# number of days to predict into the future
n_preds <- 14

# number of days before start of prediction
n_train <- n_preds

# number of posterior simulation draws
n_draws <- 2e3

# number of sampling draws per chain (if MCMC)
n_sample <- n_draws / 2

# number of parallel chains
n_chains <- 4

# wrapper to train and predict model
train_and_predict <- function(model, ...) {
  if (model == "epiestim") {
    train_and_predict.epiestim(tau = 7, ...)
  }
  else if (model == "epinow2") {
    train_and_predict.epinow2(est_rt = "gp", ...)
  }
  else if (model == "arima") {
    train_and_predict.arima(pdq_order = c(1,0,1), ...)
  }
  else if (model == "prophet") {
    train_and_predict.prophet(cp_scale = 0.25, ...)
  }
  else if (model == "gp") {
    train_and_predict.gp(rho_short = 7, ...)
  }
}

# get samples
get_samples <- function(DM, TM, k = n_preds, ns = n_draws, np = n_preds) {
  DM <- DM[ ,1:ns]
  DM <- matrix(DM, ncol = ns)
  n <- nrow(TM)
  if (k < np) {
    DM <- (tail(DM,k))
  } else if (n == np) {
    DM <- (DM)
  } else {
    DM <- (head(DM,n))
  }
  DM_list <- lapply(seq_len(nrow(DM)), function(i) DM[i, ])
  return(DM_list)
}




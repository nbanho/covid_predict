# seed
seed12345 = 1235 # currently not used

# number of days to predict into the future
n_preds <- 21

# number of days of past infections to predict
n_train <- 40

# number of posterior simulation draws
n_draws <- 1e3

# number of sampling draws per chain (if MCMC)
n_sample <- 1e3

# maximum expected incidence
max_inc <- 250 # 25% above the highest observed incidence per 1e5 pop. from BEL and ESP
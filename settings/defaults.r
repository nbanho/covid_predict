# seed
seed12345 = 12345 # currently not used

# number of days to predict into the future
n_preds <- 21

# number of days of past infections to predict
n_train <- 40

# number of posterior simulation draws
n_draws <- 2e3

# number of sampling draws per chain (if MCMC)
n_sample <- n_draws / 2

# cori: choose n1 and n2 based on n_draws with a fixed ratio
cori.n1n2_ratio <- 10
cori.n2 <- ceiling(sqrt(n_draws / cori.n1n2_ratio))
cori.n1 <- cori.n2 * cori.n1n2_ratio

# number of parallel chains
n_chains <- 4

# maximum expected incidence
prophet.max_inc <- 250 # 25% above the highest observed incidence per 1e5 pop. from BEL and ESP

# transform model target
trans_y <- "log"
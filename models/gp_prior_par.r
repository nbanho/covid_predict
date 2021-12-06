# tune parameters for prior
M <- matrix(NA, nrow = 71, ncol = 3)
k <- 0
for (i in 20:90) {
  k <- k + 1
  gamma_prior_param <- stan("models/tune_inv_gamma_prior.stan", data = list(l = 7, u = i),
                            iter = 1, warmup = 0, chains = 1, seed = 123213, algorithm = "Fixed_param")
  
  M[k, ] <- c(i, summary(gamma_prior_param)$summary["shape",1], summary(gamma_prior_param)$summary["scale",1])
}

saveRDS(M, file = "models/gp_prior_par.rds")

# from Bosse et al. log mu and log sigma
mu <- 0.079
sigma <- 0.18
rt_prior_mean <- exp(mu+0.5*sigma^2)
rt_prior_sd <- rt_prior_mean * (exp(sigma^2) - 1)
rt_prior_scale <- rt_prior_sd / rt_prior_mean
rt_prior_shape <- rt_prior_mean / rt_prior_scale
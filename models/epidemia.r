library(epidemia)
library(rstanarm)
library(lubridate)
source("utils/delays.r")

train.epidemia <- function(
  data, # data frame with columns id, population, cases, date
  ... # additional model parameters
) {
  
  # Add week and set ascertainment rate o to 1
  data <- data %>%
    mutate(o = 1, 
           week = week(date),
           year = year(date)) %>%
    mutate(year = year - min(year)) %>%
    mutate(week = max(week) * year + week)
  
  # model the rep number as weekly random walk (no covariates)
  rt <- epirt(
    formula = R(id, date) ~ 1 + rw(time = week, prior_scale = 0.1),
    prior_intercept = rstanarm::normal(log(3), 0.3), 
    link = scaled_logit(K = 6)
  )
  
  # model infections with serial interval for generation time distribution
  gen_distr <- vp(xT = 10, from0 = F, FUN = p_g)
  gen_distr <- gen_distr / sum(gen_distr)
  inf <- epiinf(
    gen = gen_distr,
    pop_adjust = T,
    pops = "population",
    latent = TRUE,
    prior_aux = rstanarm::normal(10,2),
    seed_days = 6,
    prior_seeds = rstanarm::exponential(0.03)
  )
  
  # model observed cases as a proportion of infections constant over time
  i2o_distr <- vp(xT = 30, from0 = F, FUN = p_in)
  i2o_distr <- i2o_distr / sum(i2o_distr)
  obs <- epiobs(
    formula = cases ~ 0 + offset(o),
    link = "identity",
    i2o = i2o_distr
  )
  
  # fit the model with various model params
  args <- list(rt = rt, obs = obs, inf = inf, data = data,  ...,
               control = list(adapt_delta = 0.95, max_treedepth = 15))
  fit <- do.call(epim, args)
  
  return(fit)
}


predict.epidemia <- function(
  epidemia_obj, # train object from train.epidemia
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ...
) {
  
  # test data
  test_data <- epidemia_obj$data %>%
    tail(n_preds) %>%
    mutate(date = date + n_preds) %>%
    rename(id = group)
  
  pred <- posterior_predict(epidemia_obj, newdata = test_data, draws = d, ...)
  
  return( t(pred$draws) )
  
}

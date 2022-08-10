# libraries
library(EpiNow2)

# utilities
source("utils/delays.r")

#' @title train and predict using EpiNow2
#' 
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the incidence


train_and_predict.epinow2 <- function(...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  # rename and select columns
  data <- data.frame(date = args$data$date,
                     confirm = args$data$cases)
  
  #return(data)
  
  # fit model
  rt_prior_mean <- exp(0.079+0.5*0.18^2)
  rt_prior_sd <- rt_prior_mean * (exp(0.18^2) - 1)
  estimates <- epinow(reported_cases = data,
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      rt = rt_opts(prior = list(mean = rt_prior_mean, sd = rt_prior_sd), 
                                   use_breakpoints = F, pop = args$pop),
                      gp = gp_opts(basis_prop = .2, ls_min = 3, alpha_sd = .1),
                      stan = stan_opts(samples = args$d, seed = args$seed),
                      horizon = args$n,
                      output = "samples")
  
  # get samples
  samples <- estimates$estimates$samples
  
  # filter
  samples <- samples %>%
    dplyr::filter(variable == "reported_cases") %>%
    dplyr::filter(type == "forecast")
  
  # transform
  fcast <- samples %>%
    dplyr::select(sample, time, value) %>%
    spread(time, value) %>%
    dplyr::select(-sample) %>%
    as.matrix() %>%
    t()
  
  return(fcast)
}
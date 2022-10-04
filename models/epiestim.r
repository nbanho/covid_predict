# libraries
library(EpiEstim)
library(projections)
library(epitrix)
library(incidence)

# utilities
source("utils/delays.r")
source("utils/prior_R.R")

#' @title train and predict using EpiEstim
#' 
#' @param tau the sliding time window for estimating R
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the number of new cases

train_and_predict.epiestim <- function(tau, ...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  # n1: size of the sample of GI distributions
  # n2: size of draws from posterior distribution of R
  # choose n1 and n2 based on n with a fixed ratio
  cori.n1n2_ratio <- 1
  cori.n2 <- ceiling(sqrt(args$d / cori.n1n2_ratio))
  cori.n1 <- cori.n2 * cori.n1n2_ratio
  
  # configuration
  # - use uncertain generation interval
  # - specify time window tau over which to estimate R
  cG <- make_config(
    method = "uncertain_si",
    t_start = seq(2, length(args$data$cases) - tau + 1),
    t_end = seq(2 + tau - 1, length(args$data$cases)),
    mean_si = generation_time$mean,
    std_si = generation_time$sd,
    std_mean_si = generation_time$mean_sd,
    std_std_si = generation_time$sd_sd,
    min_mean_si = generation_time$mean - 3 * generation_time$mean_sd,
    max_mean_si = generation_time$mean + 3 * generation_time$mean_sd,
    min_std_si = generation_time$sd - 3 * generation_time$sd_sd,
    max_std_si = generation_time$sd + 3 * generation_time$sd_sd,
    n1 = cori.n1,
    n2 = cori.n2,
    mean_prior = rt_prior_mean,
    std_prior = rt_prior_sd
  )
  
  # estimate R over time
  eR <- estimate_R(
    incid = args$data$cases,
    method = "uncertain_si",
    config = cG)
  
  # use latest R estimate
  i <- max(eR$R$t_end)
  
  # seed
  set.seed(args$seed)
  
  # extract plausible r values from most recent estimate
  mean_r <- eR$R$`Mean(R)`[which(eR$R$t_end == i)]
  sd_r <- eR$R$`Std(R)`[which(eR$R$t_end == i)]
  shapescale <- epitrix::gamma_mucv2shapescale(mu = mean_r, cv = sd_r/mean_r)
  plausible_r <- rgamma(args$d, shape = shapescale$shape, scale = shapescale$scale)
  
  # create incidence
  inc <- incidence(eR$dates[1:i])
  inc$counts <- as.matrix(eR$I[1:i])
  
  # list of serial interval distributions
  sis <- lapply(seq_len(nrow(eR$si_distr)), function(i) eR$si_distr[i, ])
  
  # simulate projections
  ns <- ceiling(args$d / nrow(eR$si_distr))
  proj <- map(sis, function(S) as.matrix(project(x = inc, R = plausible_r, si = S[-1], n_sim = ns, n_days = args$n, model = "poisson", instantaneous_R = T)))
  fcast <- do.call(cbind, proj)
  #proj <- project(inc, plausible_r, cg$si_distr[-1], n_sim = d, n_days = n, model = "poisson", instantaneous_R = T)
  
  return(as.matrix(fcast))
}

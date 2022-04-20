# libraries
library(EpiEstim)
library(projections)
library(epitrix)
library(incidence)

# utils
source("utils/delays.r")

# use estimated serial interval
# based on meta analysis: https://www.medrxiv.org/content/10.1101/2020.11.17.20231548v1.full-text 
# See Supplementary Material Tabl. 2 Mean and SD for Gamma 
# min/Max set large enough and symmetric
# config_si <- make_config(
#   method = "uncertain_si",
#   mean_si = 5.61,
#   std_mean_si = 0.30,
#   min_mean_si = 5.61 - 3,
#   max_mean_si = 5.61 + 3,
#   std_si = 4.83,
#   std_std_si = 0.37,
#   min_std_si = 4.83 - 3,
#   max_std_si = 4.83 + 3,
#   ...)


# train
train.cori <- function(
  new_infected, # number of new infections
  tau = 7,
  ... # additional arguments to make_config (n1, n2, seed)
  ) {
  
  # configuration
  # - use generation interval from Ferretti et al
  # - specify time window over which to estimate R
  config_g <- make_config(
    method = "non_parametric_si",
    t_start = seq(2, length(new_infected) - tau + 1),
    t_end = seq(2 + tau - 1, length(new_infected)),
    si_distr = vp(xT = 10, from0 = F, FUN = p_g)
  )
  
  # estimate R over time
  est_R <- estimate_R(
    incid = new_infected,
    method = "non_parametric_si",
    config = config_g)
  
  return(list(eR = est_R, cg = config_g))
  
}

# predict
predict.cori <- function(
  estimate_R_obj, # train object from train.renewable_cori
  i = NULL, # R estimate to use for projection (default: NULL --> use latest)
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ... # additional parameters
  ) {
  
  # config file
  cg <- estimate_R_obj$cg
  
  # estimated R
  eR <- estimate_R_obj$eR
  
  # determine i
  if (is.null(i)) { i <- max(eR$R$t_end) }
  
  # seed
  set.seed(list(match.call())$seed)
  
  # extract plausible r values from most recent estimate
  mean_r <- eR$R$`Mean(R)`[which(eR$R$t_end == i)]
  sd_r <- eR$R$`Std(R)`[which(eR$R$t_end == i)]
  shapescale <- epitrix::gamma_mucv2shapescale(mu = mean_r, cv = sd_r/mean_r)
  plausible_r <- rgamma(d, shape = shapescale$shape, scale = shapescale$scale)
  
  # create incidence
  inc <- incidence(eR$dates[1:i])
  inc$counts <- as.matrix(eR$I[1:i])
  
  # list of serial interval distributions
  #sis <- lapply(seq_len(nrow(estimate_R_obj$si_distr)), function(i) estimate_R_obj$si_distr[i, ])
  
  # simulate projections
  #ns <- ceiling(d / nrow(estimate_R_obj$si_distr))
  #proj <- map(sis, function(S) as.matrix(project(x = inc, R = plausible_r, si = S[-1], n_sim = ns, n_days = n)))
  #proj <- do.call(cbind, proj)
  proj <- project(inc, plausible_r, cg$si_distr[-1], n_sim = d, n_days = n, model = "poisson", size = 0.03, instantaneous_R = T)
  
  return(as.matrix(proj))
}

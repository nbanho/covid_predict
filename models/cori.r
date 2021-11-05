# libraries
library(EpiEstim)
library(projections)
library(epitrix)
library(incidence)


# train
train.cori <- function(
  new_confirmed, # number of new confirmed cases
  ... # additional arguments to make_config (n1, n2, seed)
  ) {
  
  # use estimated serial interval
  # based on meta analysis: https://www.medrxiv.org/content/10.1101/2020.11.17.20231548v1.full-text 
  # See Supplementary Material Tabl. 2 Mean and SD for Gamma 
  # min/Max set large enough and symmetric
  config_si <- make_config(
    method = "uncertain_si",
    mean_si = 5.61,
    std_mean_si = 0.30,
    min_mean_si = 5.61 - 3,
    max_mean_si = 5.61 + 3,
    std_si = 4.83,
    std_std_si = 0.37,
    min_std_si = 4.83 - 3,
    max_std_si = 4.83 + 3,
    ...)
  
  # estimate R over time
  estimate_R(
    incid = new_confirmed,
    method = "uncertain_si",
    config = config_si)
  
}

# predict
predict.cori <- function(
  estimate_R_obj, # train object from train.renewable_cori
  i = NULL, # R estimate to use for projection (default: NULL --> use latest)
  n = 21, # number of days to project into the future
  d = 4e3, # number of posterior draws
  ... # additional parameters
  ) {
  
  # determine i
  if (is.null(i)) { i <- max(estimate_R_obj$R$t_end) }
  
  # seed
  set.seed(list(match.call())$seed)
  
  # extract plausible r values from most recent estimate
  mean_r <- estimate_R_obj$R$`Mean(R)`[which(estimate_R_obj$R$t_end == i)]
  sd_r <- estimate_R_obj$R$`Std(R)`[which(estimate_R_obj$R$t_end == i)]
  shapescale <- epitrix::gamma_mucv2shapescale(mu = mean_r, cv = sd_r/mean_r)
  plausible_r <- rgamma(d, shape = shapescale$shape, scale = shapescale$scale)
  
  # create incidence
  inc <- incidence(estimate_R_obj$dates[1:i])
  inc$counts <- as.matrix(estimate_R_obj$I[1:i])
  
  # list of serial interval distributions
  sis <- lapply(seq_len(nrow(estimate_R_obj$si_distr)), function(i) estimate_R_obj$si_distr[i, ])
  
  # simulate projections
  ns <- ceiling(d / nrow(estimate_R_obj$si_distr))
  proj <- map(sis, function(S) as.matrix(project(x = inc, R = plausible_r, si = S[-1], n_sim = ns, n_days = n)))
  proj <- do.call(cbind, proj)
  
  return(as.matrix(proj))
}

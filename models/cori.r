# libraries
library(EpiEstim)
library(projections)
library(epitrix)
library(distcrete)
library(incidence)

# serial interval
# based on Li et al 2020 - Early transmission dynamics in Wuhan, China, of novel coronavirus–infected pneumonia
config_si <- make_config(
  seed = seed12345,
  mean_si = 7.5,
  std_si = sqrt(11.6)
)
si_distr <- distcrete("gamma", interval = 1L,
                shape = 6.5,
                scale = 0.62,
                w = 0.5)

# prepare data
create_inc <- function(
  new_confirmed,# number of new confirmed cases 
  dates # corresponding dates
  ) {
  
  # create incidence object
  inc <- incidence(dates)
  
  # add counts
  inc$counts <- as.matrix(new_confirmed)
  
  return(inc)
}

# train
train.cori <- function(
  new_confirmed, # number of new confirmed cases
  config = config_si, # configuration, i.e. serial interval distribution
  ... # additional model parameters
  ) {
  
  # estimate R over time
  estimate_R(
    incid = new_confirmed,
    method = "parametric_si",
    config = config,
    ...
  )
  
}

# predict
predict.cori <- function(
  estimate_R_obj, # train object from train.renewable_cori
  inc, # incidence object from create_inc with data over training period
  i = 1, # R estimate to use for projection (default: i = 1 --> use latest)
  si = si_distr, # serial interval distribution
  n = 1, # number of days to project into the future
  d = 4e3, # number of posterior draws
  ... # additional parameters
  ) {
  
  # extract plausible r values from most recent estimate
  mean_r <- tail(estimate_R_obj$R$`Mean(R)`, i)
  sd_r <- tail(estimate_R_obj$R$`Std(R)`, i)
  shapescale <- gamma_mucv2shapescale(mu = mean_r, cv = sd_r/mean_r)
  plausible_r <- rgamma(d, shape = shapescale$shape, scale = shapescale$scale)
  
  # make projection
  proj <- project(
    x = inc,
    R = plausible_r,
    si = si,
    n_days = n,
    n_sim = d,
    ...
  )
  
  return(as.matrix(proj))
}

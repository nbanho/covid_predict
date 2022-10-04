library(EpiNow2)
library(tidyverse)
library(fitdistrplus)
library(ConnMatTools)

# incubation period

#' distribution from Lauer et al. (2020) as sourced by EpiNow2
#' 
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max_value = 15)


# generation time

#' distribution from Ganyani et al. (2020) as sourced by EpiNow2
#' 
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 15)

# # - alternatively from from Hart et al. (2021): https://elifesciences.org/articles/70767#appendix-1
# # - see Apendix Table 2
# # - Mean: mean=4.2 (95%-CrI: 3.3-5.3)
# # - SD: mean=4.9 (95%-CrI: 3.0-8.3)


# reporting delay

#' - from Cereda et al. (2020): https://arxiv.org/abs/2003.09320
#' - see Table S2 (between onset of symptoms and confirmation)
#' - Gamma distribution with
#' - Shape: mean=1.88 (SD: 0.055)
#' - Rate: mean=2.13 (SD: 0.078)
#' - approximated via simulation with a log normal distribution using fitdistrplus::fitdist 
#' 

make_reporting_delay <- function(seed = 12345, n = 1e3, s = 1e2, max_delay = 15,
                                 shape_mean = 1.88, shape_sd = 0.055,
                                 rate_mean = 2.16, rate_sd = 0.078) {
  set.seed(seed)
  pars_gamma <- list(shape = rnorm(n, shape_mean, shape_sd), 
                     rate = rnorm(n, rate_mean, rate_sd))
  pars_lnorm_rep_del <- list()
  for (i in 1:s) {
    d <- rgamma(n, shape = pars_gamma$shape[i], scale = pars_gamma$rate[i])
    pars_lnorm_rep_del[[i]] <- fitdist(d, distr = "lnorm")$estimate
  }
  meanlog_rd_mean <- mean(sapply(pars_lnorm_rep_del, function(x) x["meanlog"]))
  meanlog_rd_sd <- sd(sapply(pars_lnorm_rep_del, function(x) x["meanlog"]))
  sdlog_rd_mean <- mean(sapply(pars_lnorm_rep_del, function(x) x["sdlog"]))
  sdlog_rd_sd <- sd(sapply(pars_lnorm_rep_del, function(x) x["sdlog"]))
  
  reporting_delay <- list(
    mean = meanlog_rd_mean, mean_sd = meanlog_rd_sd,
    sd = sdlog_rd_mean, sd_sd = sdlog_rd_sd,
    max = max_delay
  )
  
  return(reporting_delay)
}

reporting_delay <- make_reporting_delay()


# discretize delay distribution

#' @param xT number of days 
#' @param from0 whether distribution should start at zero
#' @param FUN distribution, e.g. plnorm
#' @param ... parameters of FUN
#' 
vp <- function(xT, from0, FUN, ...) {
  x <- seq(0,xT)
  y <- length(x)
  if (from0) {
    y[1] <- FUN(0.5, ...)
    y[2] <- FUN(1.5, ...) - FUN(0.5, ...)
  } else {
    y[1] <- 0
    y[2] <- FUN(1.5, ...)
  }
  for (i in 3:length(x)) {
    y[i] = FUN(x[i] + 0.5, ...) - FUN(x[i] - 0.5, ...)
  }
  return(y)
}


# plot delay distribution

#' @param delay parameters of the delay distribution
#' @param distr distribution (lognormal or gamma)
#' @param S number of samples
#' 

plot_delay_distr <- function(delay, distr = "lognormal", S = 100) {
  if (grepl("lognormal", distr)) {
    params <- lognorm_dist_def(delay$mean, delay$mean_sd, delay$sd, delay$sd_sd, max_value = delay$max, samples = S)
    lines <- lapply(params$params, function(p) vp(delay$max, from0 = T, FUN = plnorm, meanlog = p$mean, sdlog = p$sd))
  } 
  else if (grepl("gamma", distr)) {
    params <- list(mean = rnorm(S, delay$mean, delay$mean_sd), sd = rnorm(S, delay$sd, delay$sd_sd))
    lines <- map2(params$mean, params$sd, function(m, s) {
      gamma_params <- gammaParamsConvert(mean = m, sd = s)
      vp(delay$max, from0 = T, FUN = pgamma, shape = gamma_params$shape, scale = gamma_params$scale)
    })
  }
  data <- do.call(cbind, lines) %>% 
    data.frame() %>%
    mutate(`Number of days` = 0:delay$max) %>%
    reshape2::melt("Number of days") %>%
    rename(Probability = value)
  
  ggplot(data, aes(x = `Number of days`, y = Probability)) +
    stat_lineribbon() +
    scale_fill_brewer() +
    scale_x_continuous(limits = c(0, delay$max))
}


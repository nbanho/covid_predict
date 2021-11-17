# distribution from infection to reporting of new case
# parameters from our study
p_in <- function(x, mu = 2.47, sigma = 0.45, is_log = T) { 
  
  if (is_log) {
    log_mu <- mu
    log_sigma <- sigma
  } else {
    # log mean and sd
    log_mu <- log(mu^2 / sqrt(mu^2 + sigma^2))
    log_sigma <- sqrt( log(1 + sigma^2 / mu^2) ) 
  }
  
  # p
  p <- plnorm(x, log_mu, log_sigma)
  
  return( p )
}

# generation time distribution
# From Ferretti: Weibull(3.2862, 6.1244)
p_g <- function(x, distr = "weibull", p1 = 3.2862, p2 = 6.1244) { 
  if (distr == "weibull") { pweibull(x, p1, p2) }
}



# Vectorize delay function
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




# Simulate data for delay
data_delay <- function(xT, from0, FUN, ...) {
  fun_args <- list(...)
  if (length(fun_args) == 0) {
    fun_args <- formals(FUN)[-1]
    arg_names <- names(formals(FUN)[-1])
    dat <- data.frame(x = 0:xT, value = vp(xT, from0, FUN, ...))
    return(dat)
  } else {
    l <- length(fun_args[[1]])
    p_x <- mapply(function(...) invoke(vp, xT = xT, from0 = from0, FUN = FUN,  ...), ...)
    p_par <- mapply(function(...) paste0("(", paste(sapply(list(...), round, 2), collapse = ", "), ")"), ...)
    dat <- data.frame(cbind(0:xT, p_x))
    colnames(dat) <- c("x", p_par)
    dat_long <- reshape2::melt(dat, "x") %>%
      mutate(par1 = stringi::stri_extract(variable, regex = "\\d.\\d{1,2}"),
             par2 = gsub(", ", "", stringi::stri_extract(variable, regex = ", \\d.\\d{1,2}")))
    return(dat_long)
  }
}

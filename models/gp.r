# libraries
library(cmdstanr)
library(posterior)

#' @title train and predict using Gaussian process
#' 
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the incidence

train_and_predict.gp <- function(rho_short, ...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  # input data
  n_data <- length(args$data$incidence)
  x1 <- 1:n_data
  x2 <- (n_data+1):(n_data+args$n)
  data_list <- list(N1 = n_data, 
                    N2 = args$n,
                    y1 = log1p(args$data$incidence), 
                    x1 = x1,
                    x2 = x2,
                    rho_short = rho_short)
  
  # fit model
  gp_mod <- cmdstan_model("models/gp.stan", cpp_options = list(stan_threads = T))
  fit <- gp_mod$sample(
    data =  data_list,
    iter_sampling = args$d / 2,
    parallel_chains = 4, 
    threads_per_chain = 4 / 2,
    seed = args$seed
  )
  
  # extract predictions
  y_pred <- fit$draws("y2") %>%
    as_draws_df() %>%
    reshape2::melt(c(".draw", ".chain", ".iteration")) %>%
    mutate(variable = as.numeric(stringi::stri_extract(gsub("y2", "", variable), regex = "\\d+"))) %>% 
    dplyr::select(variable, .draw, value) %>%
    reshape2::dcast(variable ~ .draw) %>%
    dplyr::select(-variable) %>%
    as.matrix()
  
  # transform
  fcast <- expm1(y_pred)
  
  return(fcast)
  
}

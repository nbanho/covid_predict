library(cmdstanr)
library(posterior)

prior_par <- readRDS("models/gp_prior_par.rds")

train.gp <- function(
  target, # number of new confirmed cases
  n = n_preds, # number of days to project into the future
  d = n_draws, # number of posterior draws
  ... # additional parameters to cmdstanr::sample
) {
  
  # input data
  n_data <- length(target)
  x1 <- 1:n_data
  x2 <- (n_data+1):(n_data+n)
  data_list <- list(N1 = n_data, 
                    N2 = n,
                    y1 = target, 
                    x1 = x1,
                    x2 = x2)
  
  # tune parameters for prior
  data_list$rho1_shape <- prior_par[prior_par[, 1]==n_data,2]
  data_list$rho1_scale <- prior_par[prior_par[, 1]==n_data,3]
  
  # fit model
  gp_mod <- cmdstan_model("models/gp.stan", cpp_options = list(stan_threads = T))
  gp_mod_fit <- gp_mod$sample(
    data =  data_list,
    iter_sampling = d,
    ...
  )
  
  return(gp_mod_fit)
}


predict.gp <- function(
  cmdstan_model_fit # fitted gp from train.gp
) {
  
  y_pred <- cmdstan_model_fit$draws("y2") %>%
    as_draws_df() %>%
    reshape2::melt(c(".draw", ".chain", ".iteration")) %>%
    mutate(variable = as.numeric(stringi::stri_extract(gsub("y2", "", variable), regex = "\\d+"))) %>%
    dplyr::select(variable, .draw, value) %>%
    reshape2::dcast(variable ~ .draw) %>%
    dplyr::select(-variable) %>%
    as.matrix()
  
  return(y_pred)
    
}

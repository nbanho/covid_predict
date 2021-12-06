# Rhat 
max.Rhat <- function(stan_model) {
  max( summary(stan_model)[ ,"Rhat"] )
}

# Divergent transitions
share_divergent <- function(stanfit) {
  # Get sampler params
  sampler_params <- get_sampler_params(stanfit, inc_warmup = FALSE)
  
  # Divergent transitions
  sum_divergent_by_chain <- sapply(sampler_params, function(x) sum(x[, "divergent__"]))
  n_div <- sum( sum_divergent_by_chain )
  
  # Total samples
  ns <- sum( sapply(sampler_params, nrow) )
  
  return( n_div / ns )
  
}

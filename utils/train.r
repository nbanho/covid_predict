# train model
train <- function(
  dat, # a data frame with columns new_confirmed, population, date
  method = "prophet", # model
  ... # additional model-specific parameters
  ) {
  
  if (method == "cori") {
    
    return( train.cori(dat$target, ...) )
    
  } else if (method == "epidemia") { 
    
    data = dat %>% rename(cases = target) %>% mutate(id = "A")
    return( train.epidemia(data, ...) )
      
  } else if (method == "arima") {
    
    return( train.arima(dat$target, ...) )
    
  } else if (method == "prophet") {
    
    dsy <- dat %>% rename(ds = date, y = target)
    return( train.prophet(dsy, ...) )
    
  }
  
}


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

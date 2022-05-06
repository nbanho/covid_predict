# libraries
# ...

# utilities
source("utils/delays.r")

#' @title train and predict using Epidemia
#' 
#' @param ... args$data: data.frame with columns date, cases, incidence; 
#'            args$seed: seed
#'            args$n: the number of days to forecast ahead
#'            args$d: the number of posterior draws
#'            
#' @return A d x n matrix fcast of the posterior draws for the number of new cases

train_and_predict.epidemia <- function(...) {
  
  # arguments 
  args <- c(as.list(environment()), list(...))
  
  #' ... put 
  #' ... your 
  #' ... code
  #' ... here
  
  return(fcast)
}
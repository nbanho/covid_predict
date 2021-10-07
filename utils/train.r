train <- function(
  date, # date
  target, # target
  method = "prophet", 
  ... # additional model-specific parameters
  ) {
  
  if (method == "cori") {
    
    return( train.cori(target, ...) )
    
  } else if (method == "arima") {
    
    return( train.arima(target, ...) )
    
  } else if (method == "prophet") {
    
    return( train.prophet(data.frame(ds = date, y = target), ...) )
    
  }
  
}
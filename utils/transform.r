# transform variable
trans <- function(x, method = "log") {
  if (is.null(method)) {return(x)}
  else if (method == "log") {return(log(x+1))}
  else if (method == "sqrt") {return(sqrt(x))}
}

# inverse transform of variable
inv_trans <- function(x, method = "log") {
  if (is.null(method)) {return(x)}
  else if (method == "log") {return(exp(x)-1)}
  else if (method == "sqrt") {return((x)^2)}
}
# transform variable
trans <- function(x, pop, transfct = log, add = +1) {
  if (!is.null(transfct)) {
    return( transfct(x / pop * 1e5 + add) )
  }
  else {
    return( x / pop * 1e5 )
  }
}

# inverse transform of variable
inv_trans <- function(y, pop, transfct = exp, add = -1) {
  if (!is.null(transfct)) {
    return( (transfct(y) + add) * pop / 1e5)
  }
  else {
    return( y * pop / 1e5 )
  }
}

# transform variable
trans <- function(x, pop, transfct = log) {
  if (!is.null(transfct)) {
    return( transfct(x / pop * 1e5 ) )
  }
  else {
    return( x / pop * 1e5 )
  }
}

# inverse transform of variable
inv_trans <- function(y, pop, transfct = exp, add = -1) {
  if (!is.null(transfct)) {
    return( transfct(y) * pop / 1e5 + add)
  }
  else {
    return( y * pop / 1e5 )
  }
}

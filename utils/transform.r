# transform variable
trans <- function(x, pop, transfct = sqrt) {
  if (!is.null(transfct)) {
    return( transfct(x / pop * 1e5 ) )
  }
  else {
    return( x / pop * 1e5 )
  }
}

# inverse transform of variable
inv_trans <- function(y, pop, transfct = function(x) x ^ 2) {
  if (!is.null(transfct)) {
    return( transfct(y) * pop / 1e5 )
  }
  else {
    return( y * pop / 1e5 )
  }
}
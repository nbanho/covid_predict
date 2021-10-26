label_facet <- function(original_var, custom_name = "#days ahead: "){
  lev <- levels(as.factor(original_var))
  lab <- paste0(custom_name, ": ", lev)
  names(lab) <- lev
  return(lab)  
}
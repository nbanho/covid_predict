cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

# create long data frame
mat_to_long <- function(M, name) {
  TM <- t(M)
  colnames(TM) <- seq(1:length(colnames(TM)))
  D <- data.frame(TM) %>%
    gather() %>%
    mutate(key = gsub("X", "", key) %>% as.numeric) %>%
    mutate(var = name) %>%
    dplyr::select(var, key, value)
}

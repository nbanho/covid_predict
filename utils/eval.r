library(scoringRules)
library(tidybayes)


# read files
read_files <- function(f) { do.call(rbind, map(f, readRDS)) }

# determine maximum days ahead forecast
get_max_N <- function(D) { D %>% rowwise() %>% mutate(max_N = ifelse(is.null(dim(arima)), 1, nrow(arima))) }

pred_score <- function(
  X, # predicted target
  y = NULL, # observed target
  z = NULL, # values to normalize target (e.g. cumulative cases or pop)
  type = "crps", # type of score (default: continuous ranked probability score)
  ... # additional arguments to compute type of score
) {
  
  if (!is.null(z)) {
    X <- X / z
    if (!is.null(y)) {
      y <- y / z
    }
  }
  
  X <- data.frame(t(X))
  
  
  if (type == "calibration") {
    scoring_fct <- function(y, x) { ecdf(x)(y) }
  }
  else if (type == "sharpness") {
    scoring_fct <- function(y = NULL, x, q) { quantile(x, 1-q) - quantile(x, q) }
  } 
  else if (type == "crps") {
    scoring_fct <- function(y, x) { scoringRules::crps_sample(y = y, dat = x) }
  } 
  else if (type == "logscore") {
    scoring_fct <- function(y, x) { scoringRules::logs_sample(y = y, dat = x)}
  }
  else if (type == "bias") {
    scoring_fct <- function(y, x) { sum(x > y) / length(x) }
  }
  
  score <- mapply(scoring_fct, y, X, ...)
  
  return( list(score) )
  
}


plot_predict <- function(
  dat, # the data frame with columns id, date, variable, value
  smoothing = NULL, # should date be smoothed? (default = NULL, i.e. no)
  interval = TRUE # plot with uncertainty intervals
) {
  
  if (!is.null(smoothing)) {
    group_vars <- "variable"
    if (!interval) { group_vars = c(group_vars, "n") }
    dat <- dat %>%
      group_by_at(c(group_vars, "date")) %>%
      mutate(draw = 1:n()) %>%
      ungroup() %>%
      group_by_at(c(group_vars, "draw")) %>%
      mutate(value = zoo::rollmean(value, k = smoothing, fill = NA, align = "center")) %>%
      ungroup() %>%
      na.omit()
  }
  
  
  dat_pred <- dplyr::filter(dat, variable != "new_confirmed")
  dat_obse <- dat %>%
    dplyr::filter(variable == "new_confirmed") %>%
    dplyr::select(date, value) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup()
  
  if (interval) {
    
    # plot interval
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      stat_lineribbon(data = dat_pred, .width = c(.99, .95, .8, .5), color = "#08519C") +
      geom_line(data = dat_obse, size = 1) +
      scale_fill_brewer() +
      facet_wrap(~ variable, scales = "free_y") +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat_pred, mapping = aes(color = n, group = n)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ variable, scales = "free_y") +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}


plot_score <- function(
  dat, # the data frame with columns value, variable, and group
  CrI = c(.5, .8, .95),
  ... # lab names
) {
  
  pl <- dat %>%
    ggplot(aes(y = variable, x = value)) +
    stat_interval(.width = CrI) +
    stat_pointinterval(.width = CrI, position = position_nudge(y = -0.2)) +
    facet_wrap(~ group) +
    scale_color_brewer() +
    labs(x = "Weighted CRPS", y = "Method", color = "CrI", fill = "CrI") +
    theme_bw() 
  
  return(pl)
  
}


is_peak <- function(
  x, # target
  t = 28, # number of days to the left and right to determine peak
  min_x, # minimum target value
  z = NULL, # normalizer 
  na_value = F # value for resulting Nas
) {
  
  if (!is.null(z)) {
    x <- x / z
  }
  
  n <- length(x)
  isp <- logical(n)
  isp[1:t] <- na_value
  isp[(n-t+1):n] <- na_value
  for (i in (t):(n-t)) {
    left_x <- x[(i-t):(i-1)]
    right_x <- x[(i+1):(i+t)]
    if (all(c(left_x, right_x) < x[i]) & x[i] > min_x) {
      isp[i] <- T
    } else {
      isp[i] <- F
    }
  } 
  
  return(isp)
  
}

library(scoringRules)
library(tidybayes)


# read files
read_files <- function(f) { do.call(rbind, map(f, readRDS)) }

# determine maximum days ahead forecast
get_max_N <- function(D) { D %>% rowwise() %>% mutate(max_N = ifelse(is.null(dim(arima)), 1, nrow(arima))) }

pred_score <- function(
  X, # predicted target
  y = NULL, # observed target
  type = "crps", # type of score (default: continuous ranked probability score)
  ... # additional arguments to compute type of score
) {
  
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
      facet_wrap(~ variable, ncol = 1) +
      labs(y = "Number of new cases", color = "N") +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat_pred, mapping = aes(color = n, group = n)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ variable, ncol = 1) +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw2() +
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
    theme_bw2() 
  
  return(pl)
  
}


is_peak <- function(
  x, # target
  t = 28, # number of days to the left and right to determine peak
  min_x = 30, # minimum target value
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


is_onset <- function(
  x, # target
  onset_x = 10, # target onset incidence
  ... # additional arguments to is_peak
) {
  
  # get peaks
  peaks <- is_peak(x)
  
  # determine onset
  x_seg <- split(x, cumsum(peaks))
  x_seg_idx <- split(1:length(x), cumsum(peaks))
  onsets <- integer(length(x_seg)-1)
  # for each peak find the first time x is below onset_x
  for (i in 1:(length(x_seg)-1)) {
    x_seg_i <- rev(x_seg[[i]])
    x_seg_idx_i <- rev(x_seg_idx[[i]]) 
    n <- length(x_seg_i)
    k <- 1
    while(ifelse(is.na(x_seg_i[k]), F, x_seg_i[k]  > onset_x)) {
      k <- k + 1
    }
    if (k <= n) {
      onsets[i] <- x_seg_idx_i[k]
    } else {
      # if x always > onset_x take the middle between minimum and maximum x
      onsets[i] <- floor( ( x_seg_idx_i[which.min(x_seg_i)] + which(peaks)[i] ) / 2 )
    }
  }
  
  iso <- rep(F, length(x))
  iso[onsets] <- T
  
  return(iso)
  
}

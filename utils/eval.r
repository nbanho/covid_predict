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
  
  if(is.null(y)) {y <- rep(NA, nrow(X))}
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
  else if (type == "critical") {
    scoring_fct <- function(y = NULL, x, q) { sum(x > q) / length(x) }
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
  
  
  dat_pred <- dplyr::filter(dat, variable != "target")
  dat_obse <- dat %>%
    dplyr::filter(variable == "target") %>%
    dplyr::select(date, value) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup()
  
  if (interval) {
    
    # plot interval
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      stat_lineribbon(data = dat_pred, .width = c(.95, .8, .5), color = "#08519C") +
      geom_line(data = dat_obse, size = 1) +
      scale_fill_brewer() +
      facet_wrap(~ variable, ncol = 2) +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat_pred, mapping = aes(color = n)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ variable, ncol = 2) +
      scale_color_viridis_d() +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}


plot_score <- function(
  dat, # the data frame with columns value, variable, and group
  CrI = c(.5, .8, .95)
) {
  
  pl <- dat %>%
    ggplot(aes(y = variable, x = value)) +
    stat_interval(.width = CrI) +
    stat_pointinterval(.width = CrI, position = position_nudge(y = -0.2)) +
    facet_wrap(~ group) +
    scale_color_brewer() +
    labs(color = "CrI", fill = "CrI") +
    theme_bw2() 
  
  return(pl)
  
}


is_peak <- function(
  x, # target
  through = F, # look for through instead of peak
  t = 21, # number of days to the left and right to determine peak
  min_x = 30, # minimum target value
  na_value = F # value for resulting Nas
) {
  
  n <- length(x)
  isp <- logical(n)
  isp[1:t] <- na_value
  isp[(n-t+1):n] <- na_value
  for (i in (t):(n-t)) {
    left_x <- x[(i-t):(i-1)]
    right_x <- x[(i+1):(i+t)]
    if (through) {
      extreme <- all(c(left_x, right_x) > x[i]) & x[i] > min_x
    } else {
      extreme <- all(c(left_x, right_x) < x[i]) & x[i] > min_x
    }
    if (extreme) {
      isp[i] <- T
    } else {
      isp[i] <- F
    }
  } 
  
  return(isp)
  
}

# closest_peak <- function(
#   x, # target
#   p, # peaks of target
#   ... # additional arguments to is_peak(x, ...)
# ) {
#   
#   # n
#   n <- length(x)
#   n_p <- sum(p)
#   p_i <- which(p)
#   
#   # peaks of x
#   p_x <- is_peak(x, ...)
#   p_x_i <- which(p_x)
#   
#   # determine peak no of closest peak of x to peak of target p
#   p_x_no <- rep(NA, n)
#   
#   for (i in 1:n_p) {
#     d_i <- dist(c(p_i[i], p_x_i)) %>% as.matrix() %>% .[1,2:ncol(.)]
#     idx_min_d_i <- which.min(d_i)
#     idx_p_x_no <- p_x_i[idx_min_d_i]
#     p_x_no[idx_p_x_no] <- i 
#   }
#   
#   return(p_x_no)
# }



is_closest_peak <- function(
  x, # forecast
  p, # peaks of target
  t_right = 28, # maximum time to look for peak to the right
  t_left = 14 # maximum time to look for peak to the right
) {
  
  # n
  n <- length(x)
  n_p <- sum(p)
  
  # next peaks
  next_p <- rep(F, n)
  
  # split by peaks
  btw_peaks <- split(x, cumsum(p))
  btw_peaks_idx <- split(1:n, cumsum(p))
  
  # determine closest peak
  for (k in 1:n_p) {
    peak_left_x <- max(btw_peaks[[k]])
    peak_left_idx <- which.max(btw_peaks[[k]])
    peak_right_x <- max(btw_peaks[[k+1]])
    peak_right_idx <- which.max(btw_peaks[[k+1]])
    
    is_left_close <- ((length(btw_peaks[[k]]) - peak_left_idx) < t_left)
    is_right_close <- (peak_right_idx < t_right)
    is_right_larger <- (peak_left_x <= peak_right_x)
    
    if (is_left_close & is_right_close) {
      if (is_right_larger) {
        next_p[btw_peaks_idx[[k+1]][peak_right_idx]] <- T
      } else {
        next_p[btw_peaks_idx[[k]][peak_left_idx]] <- T
      }
    } else if (is_left_close) {
      next_p[btw_peaks_idx[[k]][peak_left_idx]] <- T
    } else if (is_right_close) {
      next_p[btw_peaks_idx[[k+1]][peak_right_idx]] <- T
    }
    # else, no close peak found
    
  }
  
  return(next_p)
}


is_critical <- function(
  x, # target
  critical_x = 50, # critical incidence
  ... # additional arguments to is_peak
) {
  
  # determine peaks 
  peaks <- is_peak(x, ...)
  
  # determine bottoms
  bottoms <- rep(F, length(peaks))
  btw_peaks <- split(x, cumsum(peaks))
  n <- 0
  for (i in 1:length(btw_peaks)) {
    if (i %% 2 == 0) {
      bottoms[n + which.min(btw_peaks[[i]])] <- T
    } else {
      n <- n + length(btw_peaks[[i]])
    }
  }
  
  # combine peaks and bottoms to know what to know what to look for
  extremes <- mapply(any, peaks, bottoms)
  is_up <- ifelse(((1 + cumsum(extremes)) %% 2) == 0, F, T)
  is_up <- sapply(split(is_up, cumsum(extremes)), all)
  splits <- lag(cumsum(extremes)) # lag to move extremes to previous vector
  splits[is.na(splits)] <- 0
  x_list <- split(x, splits)
  
  
  # determine criticals
  criticals <- lapply(x_list, function(xl) rep(NA, length(xl)))
  for (i in 1:length(x_list)) {
    if (is_up[[i]]) { criticals[[i]][which(x_list[[i]] >= critical_x)[1]] <- "up" }
    else {criticals[[i]][which(x_list[[i]] < critical_x)[1]] <- "down"}
  }
  
  # add x (helpful for testing)
  criticals <- unlist(criticals)
  names(criticals) <- x
  
  return(criticals)
}


is_onset <- function(
  x, # target
  onset_x = 10, # target onset incidence
  ... # additional arguments to is_peak
) {
  
  # get peaks
  peaks <- is_peak(x, ...)
  
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

library(scoringRules)
library(tidybayes)


plot_predict <- function(
  dat, # the data frame with columns id, date, true and prediction list columns
  target = "new_confirmed", # target variable
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  method = c("arima", "cori", "prophet"), # methods to plot (default: all)
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  start_date = NULL, # start date (default = NULL, i.e. full time series)
  end_date = NULL, # end date (default = NULL, i.e. full time series)
  point_estimate = NULL # function to compute point estimate
) {
  
  # subset variables 
  dat <- dplyr::select(dat, c("id", "date", target, method))
  
  # filter countries
  if (!is.null(countries)) {
    dat <- dplyr::filter(dat, id %in% countries)
  }
  
  # filter time
  if (!is.null(start_date)) {
    dat <- dplyr::filter(dat, date >= start_date)
  } 
  if (!is.null(end_date)) {
    dat <- dplyr::filter(dat, date <= end_date)
  }
  
  # subset pred and true
  dat_true <- dplyr::select(dat, c("id", "date", target))
  dat_pred <- dplyr::select(dat, c("id", "date", method))
  
  # to long 
  if (is.null(point_estimate)) {
    
    # default N
    if (is.null(N)) { N <- 11 }
  
    # filter predictions for which N is not available
    dat_pred <- dat_pred %>%
      group_by(id) %>%
      arrange(date) %>%
      slice(1:(n()-N+1))
      
    # long format  
    dat_pred <- dat_pred %>%
      mutate_at(vars(method), function(M) map(M, function(m) m[N,1:1000])) %>% # temporary fix
      unnest(cols = method) %>%
      reshape2::melt(c("id", "date"))
    
  } else {
    
    # compute point estimate
    dat_pred <- dat_pred %>% 
      mutate(n_ahead = map(!! sym(method[1]), function(P) if (is.null(dim(P))) { 1 } else { 1:nrow(P) } )) %>%
      mutate_at(vars(method), function(M) map(M, function(m) apply(m, 1, point_estimate))) %>%
      unnest(cols = c("n_ahead", method)) %>%
      reshape2::melt(c("id", "date", "n_ahead"))
    
    # filter n_ahead
    if (!is.null(N)) { dat_pred <- dplyr::filter(dat_pred, n_ahead %in% N) }
    
  }
  
  
  if (is.null(point_estimate)) {
    
    # plot interval
    pl <- ggplot(mapping = aes(x = date)) +
      stat_lineribbon(data = dat_pred, mapping = aes(y = value), .width = c(.99, .95, .8, .5), color = "#08519C") +
      geom_line(data = dat_true, mapping = aes(y = !! sym(target)), size = 1) +
      scale_fill_brewer() +
      facet_wrap(~ id + variable, scales = "free_y") +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot mean
    pl <- ggplot() +
      geom_line(data = dat_pred, mapping = aes(x = date, y = value, color = n_ahead, group = n_ahead)) +
      geom_line(data = dat_true, mapping = aes(x = date, y = !! sym(target)), color = "black") +
      facet_wrap(~ id + variable, scales = "free_y") +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}



pred_score <- function(
  true, # observed target
  pred, # predicted target
  normalizer = NULL, # values to normalize true and pred (e.g. cumulative cases or pop)
  type = "crps" # type of score (default: continuous ranked probability score)
) {
  
  if (!is.null(normalizer)) {
    true <- true / normalizer
    pred <- pred / normalizer
  }
  
  if (type == "crps") {
    score <- scoringRules::crps_sample(true, pred)
  }
  
  return(score)
  
}


plot_score <- function(
  dat, # the data frame with columns id, variable, n_ahead, and score
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  method = NULL, # method to plot (default = NULL, i.e. all)
  weighted = FALSE # whether the scores were weighted
) {
  
  # filter countries
  if (!is.null(countries)) {
    dat <- dplyr::filter(dat, id %in% countries)
  }
  
  # filter method
  if (!is.null(method)) {
    dat <- dplyr::filter(dat, variable %in% method)
  }
  
  # filter n_ahead
  if (!is.null(N)) {
    dat <- dplyr::filter(dat, n_ahead %in% N)
  }
  
  # plot
  if (weighted) {
    
    pl <- ggplot(dat, aes(x = id, y = score, fill = factor(variable))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Average score", fill = "Method", x = "Country") +
      theme_bw() +
      theme(axis.title.x = element_blank(), legend.position = "top")
    
  } else {
    
    pl <- ggplot(dat, aes(x = n_ahead, y = score, fill = factor(variable))) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ id) +
      labs(y = "Average score", fill = "Method", x = "Days predicted ahead") +
      theme_bw() +
      theme(axis.title.x = element_blank(), legend.position = "top")
    
  }
  
  return(pl)
  
}


plot_score_overall <- function(
  dat, # the data frame with columns id, variable, n_ahead, and score
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  method = NULL, # method to plot (default = NULL, i.e. all)
  weighted = FALSE # whether the scores were weighted
) {
  
  # filter method
  if (!is.null(method)) {
    dat <- dplyr::filter(dat, variable %in% method)
  }
  
  # filter n_ahead
  if (!is.null(N)) {
    dat <- dplyr::filter(dat, n_ahead %in% N)
  }
  
  # plot
  if (weighted) {
    
    pl <- ggplot(dat, aes(x = variable, y = score)) +
      geom_boxplot(stat = "boxplot", position = "dodge") +
      labs(y = "Weighted average Score", x = "Method") +
      theme_bw() +
      theme(legend.position = "top")
    
  } else {
    
    pl <- ggplot(dat, aes(x = n_ahead, y = score, color = factor(variable))) +
      geom_boxplot(stat = "boxplot", position = "dodge") +
      labs(y = "Average score", color = "Method", x = "Days predicted ahead") +
      theme_bw() +
      theme(legend.position = "top")
    
  }
  
  return(pl)
  
}


plot_calibration <- function(
  dat, # the data frame with columns id, date, true and prediction list columns
  target = "new_confirmed", # target variable
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  method = c("arima", "cori", "prophet"), # methods to plot (default: all)
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  start_date = NULL, # start date (default = NULL, i.e. full time series)
  end_date = NULL # end date (default = NULL, i.e. full time series)
) {
  
  
  # subset variables 
  dat <- dplyr::select(dat, c("id", "date", target, method))
  
  # filter countries
  if (!is.null(countries)) {
    dat <- dplyr::filter(dat, id %in% countries)
  }
  
  # filter time
  if (!is.null(start_date)) {
    dat <- dplyr::filter(dat, date >= start_date)
  } 
  if (!is.null(end_date)) {
    dat <- dplyr::filter(dat, date <= end_date)
  }
  
  # subset
  dat <- dplyr::select(dat, c("id", "date", target, method))
  
  # function to count
  count_q <- function(x, y) {
    sum(x <= y) / length(x)
  }
  
  # replicate count function
  rep.count_q <- function(Yhat, y) {
    counts <- list()
    for (i in 1:length(y)) {
      if (is.null(dim(Yhat[[i]]))) { Yhat[[i]] <- t(as.matrix(Yhat[[i]])) }
      N <- nrow(Yhat[[i]])
      C <- numeric(N)
      for (n in 1:N) {
        C[n] <- count_q(x = Yhat[[i]][n, ], y = y[[i]])
      }
      counts[[i]] <- C
    }
    return(counts)
  }
  
  # determine N 
  dat <- dat %>% 
    mutate(n_ahead = map(!! sym(method[1]), function(P) if (is.null(dim(P))) { 1 } else { 1:nrow(P) } ))
  
  # compute counts
  for (m in method) {
    dat[[m]] <- rep.count_q(Yhat = dat[[m]], y = dat[[target]])
  }
      
  # to long format
  dat <- dat %>%
    dplyr::select(- !! sym(target)) %>%
    unnest(cols = c("n_ahead", method)) %>%
    reshape2::melt(c("id", "date", "n_ahead")) %>%
    mutate(inv_value = 1 - value) %>%
    rename(model = variable, Below = value, Above = inv_value) %>%
    reshape2::melt(c("id", "date", "n_ahead", "model")) %>%
    group_by(id, n_ahead, model, variable) %>%
    summarize(mean_value = mean(value)) %>%
    ungroup()
  
  if (!is.null(N)) { dat <- dplyr::filter(dat, n_ahead %in% N) }
  
  # plot
  pl <- ggplot(dat, aes(x = mean_value, y = model, fill = factor(variable))) +
    geom_bar(stat = "identity", position = "stack") +
    geom_vline(aes(xintercept = 0.5), linetype = "dotted") +
    facet_wrap(~ id + n_ahead) +
    labs(y = "Model", x = "Average probability to predict below/above true value (%)") +
    scale_fill_brewer() +
    scale_x_continuous(labels = function(x) x * 100) +
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank())
  
  return(pl)
  
}

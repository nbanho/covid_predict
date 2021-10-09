plot_predict <- function(
  dat, # the data frame with columns id, date, true, pred, n_ahead
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  start_date = NULL, # start date (default = NULL, i.e. full time series)
  end_date = NULL # end date (default = NULL, i.e. full time series)
) {
  
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
  
  # subset true
  dat_true <- dplyr::filter(dat, n_ahead == 1)
  
  # filter n_ahead
  if (!is.null(N)) {
    dat <- dplyr::filter(dat, n_ahead %in% N)
  }
  
  # plot
  pl <- ggplot() +
    geom_line(data = dat, mapping = aes(x = date, y = pred, color = n_ahead)) +
    geom_line(data = dat_true, mapping = aes(x = date, y = true), color = "black") +
    facet_wrap(~ id, scales = "free_y") +
    scale_color_viridis_c() +
    labs(y = "Number of new cases", color = "N") +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  return(pl)
  
}



pred_error <- function(
  true, # observed target
  pred, # predicted target
  pop # population
) {
  
  # set negative pred to zero
  pred <- ifelse(pred < 0, 0, pred)
  
  # normalize by pop
  true <- true / pop
  pred <- pred / pop
  
  # compute error
  err <- (true - pred) ^ 2
  
  return(err)
  
}


pred_error.weighted <- function(
  weights, # weights
  N, # number of days predicted ahead
  ... # arguments to function pred_error
) {
  
  # weight prediction error
  weighted_err <- pred_error(...) * weights[N]
  
  return(weighted_err)
  
}


plot_rmse <- function(
  dat, # the data frame with columns id, variable, n_ahead, and rmse
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  method = NULL # method to plot (default = NULL, i.e. all)
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
  pl <- ggplot(dat, aes(x = n_ahead, y = rmse, fill = factor(variable))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ id) +
    labs(y = "RMSE", fill = "Method", x = "Days predicted ahead") +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "top")
  
  return(pl)
  
}


plot_rmse_overall <- function(
  dat, # the data frame with columns id, variable, n_ahead, and rmse
  N = NULL, # number of days predicted ahead (default = NULL, i.e. all)
  method = NULL # method to plot (default = NULL, i.e. all)
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
  pl <- ggplot(dat, aes(x = n_ahead, y = rmse, color = factor(variable))) +
    geom_boxplot(stat = "boxplot", position = "dodge") +
    labs(y = "RMSE", color = "Method", x = "Days predicted ahead") +
    theme_bw() +
    theme(legend.position = "top")
  
  return(pl)
  
}


plot_rmse_weighted <- function(
  dat, # the data frame with columns id, variable, n_ahead, and rmse
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  method = NULL # method to plot (default = NULL, i.e. all)
) {
  
  # filter countries
  if (!is.null(countries)) {
    dat <- dplyr::filter(dat, id %in% countries)
  }
  
  # filter method
  if (!is.null(method)) {
    dat <- dplyr::filter(dat, variable == method)
  }
  
  # plot
  pl <- ggplot(dat, aes(x = id, y = rmse, fill = factor(variable))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = "Weighted RMSE", fill = "Method", x = "Country") +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "top")
  
  return(pl)
  
}



plot_rmse_weighted_overall <- function(
  dat, # the data frame with columns id, variable, n_ahead, and rmse
  method = NULL # method to plot (default = NULL, i.e. all)
) {
  
  # filter method
  if (!is.null(method)) {
    dat <- dplyr::filter(dat, variable %in% method)
  }
  
  # plot
  pl <- ggplot(dat, aes(x = factor(variable), y = rmse)) +
    geom_boxplot(stat = "boxplot", position = "dodge") +
    labs(y = "Weighted RMSE", x = "Method") +
    theme_bw() +
    theme(legend.position = "top")
  
  return(pl)
  
}


hist_pred_err <- function(
  dat, # the data frame with columns id, variable, n_ahead, and squared pred error
  countries = NULL, # vector of countries to plot (default = NULL, i.e. all) 
  N = 11, # number of days predicted ahead (default = NULL, i.e. all)
  method = NULL # method to plot (default = NULL, i.e. all)
) {
  
  # filter countries
  if (!is.null(countries)) {
    dat <- dplyr::filter(dat, id %in% countries)
  }
  
  # filter method
  if (!is.null(method)) {
    dat <- dplyr::filter(dat, variable %in% method)
  } 
  methods <- unique(dat$variable)
  
  # filter n_ahead
  if (!is.null(N)) {
    dat <- dplyr::filter(dat, n_ahead == N)
  }
  
  # plot
  pl <- ggplot(dat, aes(x = sqrt(sqerr), fill = variable))
  
  for (met in methods) {
    pl <- pl +
      geom_histogram(data = subset(dat, variable == met), alpha = .4) 
  }
  
  pl <- pl +
    facet_wrap(~ id, ncol = 4) +
    labs(y = "Count", fill = "Method", x = "Prediction error") +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "top")
  
  return(pl)
  
}

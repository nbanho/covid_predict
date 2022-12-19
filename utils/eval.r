library(scoringRules)
library(pROC)
library(tidybayes)
library(covidcast)
library(lubridate)
library(scoringutils)

# compute forecasting score
compute_forecast_score <- function(df, model, cap_fcast = c(0, 1e5), min_inc = 35, sum_by = "week", ...) {
  #TODO: check and potentially adjust the maximum of cap_fcast
  #' aggregate forecasts und compute log incidence
  df <- compute_incidence(df = df, model = model, cap_fcast = cap_fcast, min_inc = min_inc, sum_by = sum_by) 
  df <- mutate(df, across(c(forecast, incidence), ~ log1p(.x)))
  
  
  #' compute score 
  score_df <- df %>%
    group_by(state_id, state, model, date, n, incidence) %>%
    summarize(score = pred_score(forecast, incidence[1], ...)) %>%
    ungroup()
  
  #' add info
  score_df <- score_df %>%
    dplyr::select(state_id, state, model, date, n, incidence, score)
  
  return(score_df)
}

# compute incidence 
compute_incidence <- function(df, model, cap_fcast = c(0, 1e5), min_inc = -Inf, sum_by = "week") {
  #' aggregate daily forecasts, e.g. by week
  df <- add_n_ahead(df)
  
  #' unnest data
  df <- df %>%
    unnest(cols = c("data")) %>%
    unnest(cols = "forecast") %>%
    group_by(date, n) %>%
    mutate(draw = 1:n()) %>%
    ungroup() 
  
  #' compute incidence
  #' - only for epi models making integer forecasts
  #' - cap forecasts at incidence of 100,000 per 100,000 people (total pop.)
  #' - remove forecasts if observed incidence is below 10
  #' - take log of incidence (percent error)
  #' 
  if (grepl("epi", model)) {
    df$forecast <- compute_incidence.epi(df$forecast, tolower(df$state[1]))
  }
  df <- df %>%
    filter(incidence > min_inc) %>%
    mutate(forecast = ifelse(forecast < cap_fcast[1], cap_fcast[1], forecast),
           forecast = ifelse(forecast > cap_fcast[2], cap_fcast[2], forecast)) 
  
  if (!is.null(sum_by)) {
    
    if (sum_by == "week") {
      n_days <- 7
      cuts <- c(0, seq(n_days, max(df$n), n_days))
      df <- df %>% mutate(date_by = format(date, "%Y-%W"))
    }
    
    forecastDF <- df %>%
      mutate(n = as.integer(cut(n, cuts))) %>%
      group_by(state, date_by, n, draw) %>%
      summarize(days = n(),
                forecast = sum(forecast)) %>%
      ungroup() %>%
      rename(date = date_by)
    
    observedDF <- df  %>%
      group_by(date_by, date) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(state, date_by) %>%
      summarize(incidence = sum(incidence)) %>%
      ungroup() %>%
      rename(date = date_by)
    
  } 
  
  df_by <- left_join(observedDF, forecastDF, by = c("state", "date")) %>%
    # rescale incidence for some weeks with less than seven days (missing reports)
    mutate(across(c(incidence, forecast), ~ (n_days / days) * .x)) %>%
    rename(state_id = state) %>%
    mutate(model = model,
           model = recode(model, !!! model_names),
           model = factor(model, model_names),
           state = recode(tolower(state_id), !!! state_names)) %>%
    dplyr::select(state_id, state, date, n, model, draw, incidence, forecast)
  
  return(df_by)
}

# add n_ahead
add_n_ahead <- function(D) {
  D$data <- map2(D$data, D$forecast_date, function(d,f) {
    d %>% mutate(n = as.numeric(date - f + 1))
  })
  return(D)
}

# compute incidence for epi models
cc <- county_census %>%
  dplyr::filter(COUNTY == 0) %>%
  mutate(state_id = tolower(fips_to_abbr(FIPS))) %>%
  rename(pop = POPESTIMATE2019) %>%
  dplyr::select(state_id, pop)

compute_incidence.epi <- function(x, state_id) { x / cc$pop[cc$state_id==state_id] * 1e5 }


pred_score <- function(
  x, # predicted target
  y = NULL, # observed target
  type = "crps", # type of score (default: continuous ranked probability score)
  ... # additional arguments to compute type of score
) {
  
  if(is.null(y)) {y <- rep(NA, length(x))}
  
  if (type == "calibration") {
    scoring_fct <- function(x, y) { ecdf(x)(y) }
  }
  else if (type == "sharpness") {
    scoring_fct <- function(x, y = NULL, q = NULL) { scoringutils::mad_sample(matrix(x, nrow = 1)) }
      #quantile(x, 1-q, na.rm = T) - quantile(x, q) }
  } 
  else if (type == "crps") {
    scoring_fct <- function(x, y) { scoringRules::crps_sample(y = y, dat = c(na.omit(x))) }
  } 
  else if (type == "logscore") {
    scoring_fct <- function(x, y) { scoringRules::logs_sample(y = y, dat = c(na.omit(x))) }
  }
  else if (type == "bias") {
    scoring_fct <- function(x, y) { sum(x > y, na.rm = T) / length(x[!is.na(x)]) }
  } 
  else if (type == "critical") {
    scoring_fct <- function(x, y = NULL, q) { sum(x > q) / length(x) }
  } 
  else if (type == "coverage") {
    scoring_fct <- function(x, y, q) {ifelse(y <= quantile(x, 1-q, na.rm = T), ifelse(y >= quantile(x, q, na.rm = T), 1, 0), 0)}
  }
  
  score <- scoring_fct(x, y, ...)
  #score <- map2_dbl(x, y, scoring_fct, ...)
  
  return( score )
  
}

is_hotspot <- function(y, q = .25, min_inc = 10) {
  n <- length(y)
  h <- numeric(n-7)
  for (i in 1:(n-7)) {
    if (y[i+7] < min_inc) {
      h[i] <- NA
    } else {
      h[i] <- ifelse(y[i+7] / y[i] - 1 > q, 1, 0)
    }
  }
  return(h)
}

p_hotspot <- function(X, q = .25) {
  n <- nrow(X)
  d <- ncol(X)
  p <- numeric(n-7)
  for (i in 1:(n-7)) {
    p[i] <- sum((X[i+7, ] / X[i, ] - 1) > q) / d
  }
  return(p)
} 


plot_predict <- function(
  dat, # the data frame with columns id, date, variable, value
  smoothing = NULL, # should date be smoothed? (default = NULL, i.e. no)
  interval = TRUE, # plot with uncertainty intervals
  nc = 2, # number of facet columns
  direction = "h" # direction of facets
  
) {
  
  # smoothing
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
  
  # observed data
  dat_obse <- dat %>%
    dplyr::select(date, target) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup() %>%
    rename(value = target)
  
  if (interval) {
    
    # plot interval
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      stat_lineribbon(data = dat, .width = c(.95, .8, .5), color = "#08519C") +
      geom_line(data = dat_obse) +
      scale_fill_brewer() +
      scale_x_date(expand = c(0,0), breaks = "2 months", date_labels = "%b %y") +
      facet_wrap(~ variable, ncol = nc, dir = direction) +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat, mapping = aes(color = n)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ variable, ncol = nc, dir = direction) +
      scale_color_viridis_d() +
      scale_x_date(expand = c(0,0), breaks = "2 months", date_labels = "%b %y") +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}

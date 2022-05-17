library(scoringRules)
library(pROC)
library(tidybayes)
library(covidcast)

# read state id from file name
read_state_id <- function(f) { gsub("(_.*)", "", basename(f)) }

# read model from file name
read_model <- function(f) { gsub(".rds", "", gsub("(.*_)", "", basename(f))) }

# read forecasts
read_forecasts <- function(files) {
  df_F <- do.call(rbind, lapply(files, function(f) {
    # read model name
    m <- read_model(f)
    
    # read state id
    s <- read_state_id(f)
    
    # read file 
    df <- readRDS(f) 
    
    # filter data
    df <- df %>%
      get_n_ahead() %>%
      unnest(cols = c("data")) %>%
      mutate(state_id = tolower(s),
             variable = m) %>%
      unnest(cols = c("forecast")) 
    
    # compute incidence
    if (grepl("epi", m)) {
      df <- df %>%
        rename(m = forecast) %>%
        comp_inc(m) %>%
        rename(forecast = m) %>%
        dplyr::select(-pop)
    }
    
    return(df)
  }))
  
  df_F <- df_F %>%
    mutate(forecast = ifelse(forecast > max_inc, max_inc, forecast)) %>% 
    mutate(variable = recode(variable, !!! model_names)) %>%
    rename(value = forecast, target = incidence)
}

# add n_ahead
add_n_ahead <- function(D) {
  D$data <- map2(D$data, D$forecast_date, function(d,f) {
    d %>% mutate(n = as.numeric(date - f + 1))
  })
  return(D)
}

# compute incidence
cc <- county_census %>%
  dplyr::filter(COUNTY == 0) %>%
  mutate(state_id = tolower(fips_to_abbr(FIPS))) %>%
  rename(pop = POPESTIMATE2019) %>%
  dplyr::select(state_id, pop)

comp_inc <- function(D, ..., pop = cc) {
  D %>%
    left_join(cc, by = "state_id") %>%
    ungroup() %>%
    mutate_at(vars(...), ~ . / pop * 1e5) 
}


# add prediction score
add_pred_score <- function(D, f, models = models, ...) {
  D %>%
    add_n_ahead() %>%
    .$data %>%
    lapply(., function(X) mutate(X, state_id = tolower(gsub(".rds", "", basename(f))))) %>%
    lapply(., function(X) comp_inc(X, matches("cori"))) %>%
    lapply(., function(X) cbind(dplyr::select(X, state_id, date, n, incidence), 
                                sapply(models, function(m) pred_score(X[[m]], X$incidence, ...)))) %>%
    do.call(rbind, .)
}

pred_score <- function(
  X, # predicted target
  y = NULL, # observed target
  trans = NULL,
  type = "crps", # type of score (default: continuous ranked probability score)
  ... # additional arguments to compute type of score
) {
  
  if (!is.null(trans)) {
    X <- trans(X)
    y <- trans(y)
  }
  
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

# summarize prediction score by day or week
summarize_score_by_n <- function(df, fct, week = F) {
  if (week) {
    df <- df %>% mutate(n = cut(n, breaks = n_brks, labels = n_lbs))
  }
  df <- df %>%
    dplyr::select(n, models) %>%
    melt("n") %>%
    group_by(n, variable) %>%
    summarize(value = fct(value)) %>%
    ungroup() %>%
    mutate(variable = recode(variable, !!! model_names))
  return(df)
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
  interval = TRUE # plot with uncertainty intervals
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
      facet_wrap(~ variable, ncol = 2) +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat, mapping = aes(color = n)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ variable, ncol = 2) +
      scale_color_viridis_d() +
      scale_x_date(expand = c(0,0), breaks = "2 months", date_labels = "%b %y") +
      theme_bw2() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}


plot_score <- function(
  dat, # the data frame with columns value, variable, and group
  models, # model names
  model_colors # color for models
) {
  
  if ("group" %in% colnames(dat)) {
    pl <- dat %>%
      dplyr::select(c("state_id", "group", models)) %>%
      gather("variable", "value", models) %>%   
      group_by(state_id, group, variable) %>%
      summarize(mean_score = mean(value)) %>%
      ungroup() %>% 
      mutate(variable = factor(variable, levels = models)) %>%
      ggplot(aes(x = variable, y = mean_score, color = variable)) +
      facet_wrap(~ group)
  } else {
    pl <- dat %>%
      dplyr::select_at(c("state_id", models)) %>%
      gather("variable", "value", models) %>%    
      group_by(state_id, variable) %>%
      summarize(mean_score = mean(value)) %>%
      ungroup() %>% 
      mutate(variable = factor(variable, levels = models)) %>%
      ggplot(aes(x = variable, y = mean_score, color = variable)) 
  }
  
  pl <- pl +
    geom_boxplot() +
    geom_jitter(shape = 4, width = .1, height = 0, alpha = .66) +
    scale_color_manual(values = model_colors) +
    theme_bw2() 

  return(pl)
  
}
library(scoringRules)
library(pROC)
library(tidybayes)
library(covidcast)

# read state id from file name
read_state_id <- function(f) { gsub("(_.*)", "", basename(f)) }

# read model from file name
read_model <- function(f) { gsub(".rds", "", gsub("(.*_)", "", basename(f))) }

# read forecasts
read_n_ahead_forecasts <- function(files, n) {
  df_F <- do.call(rbind, lapply(files, function(f) {
    # read model name
    m <- read_model(f)
    
    # read state id
    s <- read_state_id(f)
    
    # read file 
    df <- readRDS(f) 
    
    # filter data
    df <- df %>%
      get_n_ahead(n) %>%
      unnest(cols = c("data")) %>%
      mutate(state_id = tolower(s),
             variable = m) %>%
      unnest(cols = c("forecast")) 
    
    # compute incidence
    if (grepl("epi", m)) {
      df$forecast <- compute_incidence(df$forecast, df$state_id[1])
    }
    
    return(df)
  }))
  
  df_F <- df_F %>%
    mutate(forecast = ifelse(forecast > max_inc, max_inc, forecast)) %>% 
    mutate(variable = recode(variable, !!! model_names)) %>%
    rename(value = forecast, target = incidence) %>%
    mutate(state = recode(state_id, !!! state_names)) 
}

compute_forecast_score <- function(files, sum_by = NULL, ...) { 
  do.call(rbind, map(files, function(f) {
    print(sprintf("Compute score for file %s", f))
    # read model name
    m <- read_model(f)
  
    # read state id
    s <- read_state_id(f)
  
    # read file 
    df <- readRDS(f) 
  
    # compute incidence
    if (grepl("epi", m)) {
      df$data <- lapply(df$data, function(D) { 
        D$forecast <- lapply(D$forecast, compute_incidence, state_id = tolower(s))
        return(D)
      })
    }
    
    # add n_ahead
    df <- add_n_ahead(df)
  
    # compute score
    if (is.null(sum_by)) {
      df$data <- lapply(df$data, function(D) {
        D$value <- pred_score(D$forecast, D$incidence, ...)
        return(D %>% dplyr::select(-forecast))
      })
    } else {
      df <- df %>%
        dplyr::select(-state,-forecast_date) %>%
        unnest(cols = c("data")) %>%
        mutate(n = as.integer(cut(n, sum_by))) %>%
        group_by(date, n) %>%
        summarize(incidence = sum(incidence),
                  forecast = list(colSums(do.call(rbind, forecast)))) %>%
        ungroup()
      df$value <- pred_score(df$forecast, df$incidence, ...)
      df <- dplyr::select(df, -forecast)
    }
  
    # unnest and add info
    if(is.null(sum_by)) {
      df <- df %>%
        unnest(cols = c("data")) 
    }
   df <- df %>%
      mutate(state_id = tolower(s),
             variable = m) %>%
      mutate(variable = recode(variable, !!! model_names)) %>%
      mutate(variable = factor(variable, model_names)) %>%
      rename(target = incidence) %>%
      mutate(state = recode(state_id, !!! state_names)) 
  
    return(df)
  }))
}

# get n_ahead forecast draws by model
get_n_ahead <- function(D, n = 10) {
  D$data <- map2(D$data, D$forecast_date, function(d,f) dplyr::filter(d, date == f %m+% days(n-1)))
  D %>% dplyr::filter(sapply(D$data, nrow) > 0)
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

compute_incidence <- function(x, state_id) { x / cc$pop[cc$state_id==state_id] * 1e5 }

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
    scoring_fct <- function(x, y) { sum(x > y) / length(x) }
  } 
  else if (type == "critical") {
    scoring_fct <- function(x, y = NULL, q) { sum(x > q) / length(x) }
  } 
  else if (type == "coverage") {
    scoring_fct <- function(x, y, q) {ifelse(y <= quantile(x, 1-q, na.rm = T), ifelse(y >= quantile(x, q, na.rm = T), 1, 0), 0)}
  }
  
  score <- map2_dbl(x, y, scoring_fct, ...)
  
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

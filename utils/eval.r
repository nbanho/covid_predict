library(scoringRules)
library(tidybayes)


# read files
read_files <- function(f) { do.call(rbind, map(f, readRDS)) }

# determine maximum days ahead forecast
get_max_N <- function(D) { D %>% rowwise() %>% mutate(max_N = ifelse(is.null(dim(arima)), 1, nrow(arima))) }

# get number of days predicted ahead as list
get_n_ahead <- function(D) { D %>% rowwise() %>% mutate(n_ahead = list(1:max_N)) }

# get dates corresponding to days ahead 
get_dates_ahead <- function(D) { D %>% rowwise() %>% mutate(date_ahead = list(date %m+% days(0:(max_N-1)))) }

# get target ahead
get_target_ahead <- function(D, target = "new_confirmed") { 
  D[[paste0(target, "_ahead")]] <- map2(D$id, D$date_ahead, function(i,d) {
    D %>% dplyr::filter(id == i) %>% dplyr::filter(date %in% d) %>%
      dplyr::select(target) %>% unlist
  })
  return(D)
}

pred_score <- function(
  X, # predicted target
  y, # observed target
  z = NULL, # values to normalize target (e.g. cumulative cases or pop)
  type = "crps", # type of score (default: continuous ranked probability score)
  ... # additional arguments to compute type of score
) {
  
  if (!is.null(z)) {
    y <- y / z
    X <- X / z
  }
  
  if (is.null(dim(X))) { X <- t(as.matrix(X)) } # temporary fix
  
  Xy <- cbind(X, y)
  
  if (type == "crps") {
    score <- apply(Xy, 1, function(v) scoringRules::crps_sample(v[length(v)], v[-length(v)]))
  } 
  else if (type == "bias") {
    score <- apply(Xy, 1, function(v) sum(v[length(v)] <= v[-length(v)]) / (length(v)-1))
  } 
  else if (type == "sharpness") {
    args <- as.list(match.call())
    q = args$q 
    score <- apply(X, 1, function(x) quantile(x, 1-q) - quantile(x, q))
  }
  
  return(list(score))
  
}


plot_predict <- function(
  dat, # the data frame with columns id, date, variable, value
  smoothing = NULL, # should date be smoothed? (default = NULL, i.e. no)
  interval = TRUE # plot with uncertainty intervals
) {
  
  if (!is.null(smoothing)) {
    group_vars <- "variable"
    if (!interval) { group_vars = c(group_vars, "n_ahead") }
    dat <- dat %>%
      group_by_at(c(group_vars, "date")) %>%
      mutate(draw = 1:n()) %>%
      ungroup() %>%
      group_by_at(c(group_vars, "draw")) %>%
      mutate(value = zoo::rollmean(value, k = smoothing, fill = NA, align = "center")) %>%
      ungroup() %>%
      na.omit()
  }
  
  
  dat_pred <- dplyr::filter(dat, variable != "observed")
  dat_obse <- dat %>%
    dplyr::filter(variable == "observed") %>%
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
      geom_line(data = dat_pred, mapping = aes(color = n_ahead, group = n_ahead)) +
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
  dat, # the data frame with columns id, variable, facet_var, and score
  facet_name = "#days ahead: ",
  ... # additional arguments to scale_fill_distiller
  
) {
  
  pl <- ggplot(dat, aes(x = id, y = variable, fill = score)) +
    geom_tile() +
    facet_wrap(~ facet_var, labeller = labeller(facet_var = label_facet(dat$facet_var, facet_name))) +
    labs(y = "variable", x = "Country") +
    scale_fill_distiller(palette = "Spectral", ...) +
    theme_bw() +
    theme(legend.position = "top", plot.margin = unit(c(0.25,1,0.25,0.25),"cm"))
  
  panel_width = unit(1,"npc") - sum(ggplotGrob(pl)[["widths"]][-3]) - unit(1,"line")
  pl <- pl + guides(fill = guide_colorbar(barwidth = panel_width, title.position = "top", title.hjust = 0.5))
  
  return(pl)
  
}


plot_score.time <- function(
  dat, # the data frame with columns date, variable, facet_var, and score
  smoothing = NULL, # should date be smoothed? (default = NULL, i.e. no)
  facet_name = "#days ahead: ",
  ... # additional arguments to scale_y_continuous
) {
  
  if (!is.null(smoothing)) {
    dat <- dat %>%
      group_by(variable, facet_var) %>%
      mutate(score = zoo::rollmean(score, k = smoothing, align = "center", fill = NA)) %>%
      ungroup() %>%
      na.omit()
  }
  
  pl <- ggplot(dat, aes(x = date, y = score, color = variable)) +
    geom_line() +
    facet_wrap(~ facet_var, labeller = labeller(facet_var = label_facet(dat$facet_var, facet_name))) +
    labs(x = "Date", color = "variable") +
    scale_y_continuous(...) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(legend.position = "top")
  
  return(pl)
  
}


plot_score_overall <- function(
  dat, # the data frame with columns id, variable, n_ahead, and score
  ylab = "Average conditional ranked probability score" # 
) {
  
  pl <- ggplot(dat, aes(x = variable, y = score)) +
    geom_boxplot(stat = "boxplot", position = "dodge") +
    geom_jitter(alpha = .5, shape = 3) +
    facet_wrap(~ n_ahead, labeller = labeller(n_ahead = label_facet(dat$n_ahead))) +
    labs(y = ylab, x = "variable") +
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank())
  
  return(pl)
  
}


plot_calibration <- function(
  dat # the data frame with columns id, n_ahead, variable, and score
) {
  
  pl <- ggplot(dat, aes(x = id, y = variable, fill = mean_value)) +
    geom_tile() +
    facet_wrap(~ n_ahead, labeller = labeller()) +
    labs(y = "variable", x = "Country",
         fill = "Average probability to predict higher than observed value (%)") +
    scale_fill_distiller(palette = "Spectral", limits = c(0, 1), breaks = seq(0, 1, .25),
                         labels = c("0.00 \n Under-predict", "0.25", 
                                    "0.50 \n Well-calibrated", 
                                    "0.75", "1.00 \n Over-predict")) +
    theme_bw() +
    theme(legend.position = "top",
          plot.margin=unit(c(0.25,1,0.25,0.25),"cm"))
  
  panel_width = unit(1,"npc") - sum(ggplotGrob(pl)[["widths"]][-3]) - unit(1,"line")
  pl <- pl + guides(fill = guide_colorbar(barwidth = panel_width, title.position = "top", title.hjust = 0.5))
  
  return(pl)
  
}

library(scoringRules)
library(tidybayes)


pred_score <- function(
  X, # predicted target
  y, # observed target
  z = NULL, # values to normalize target (e.g. cumulative cases or pop)
  type = "crps" # type of score (default: continuous ranked probability score)
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
  else if (type == "calibration") {
    score <- apply(Xy, 1, function(v) sum(v[length(v)] <= v[-length(v)]) / (length(v)-1))
  }
  
  return(list(score))
  
}


plot_predict <- function(
  dat, # the data frame with columns id, date, variable, value
  interval = TRUE # plot with uncertainty intervals
) {
  
  dat_pred <- dplyr::filter(dat, variable != "observed")
  dat_obse <- dat %>%
    dplyr::filter(variable == "observed") %>%
    dplyr::select(id, date, value) %>%
    group_by(id, date) %>%
    slice(1) %>%
    ungroup()
  
  if (interval) {
    
    # plot interval
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      stat_lineribbon(data = dat_pred, .width = c(.99, .95, .8, .5), color = "#08519C") +
      geom_line(data = dat_obse, size = 1) +
      scale_fill_brewer() +
      facet_wrap(~ id + variable, scales = "free_y") +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  } else {
    
    # plot point estimate
    pl <- ggplot(mapping = aes(x = date, y = value)) +
      geom_line(data = dat_pred, mapping = aes(color = n_ahead, group = n_ahead)) +
      geom_line(data = dat_obse, color = "black") +
      facet_wrap(~ id + variable, scales = "free_y") +
      scale_color_viridis_c() +
      labs(y = "Number of new cases", color = "N") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    
  }
  
  return(pl)
  
}


plot_score <- function(
  dat, # the data frame with columns id, variable, n_ahead, and score
  ... # additional arguments to scale_fill_distiller
  
) {
  
  pl <- ggplot(dat, aes(x = id, y = variable, fill = score)) +
    geom_tile() +
    facet_wrap(~ n_ahead, labeller = labeller(n_ahead = label_facet(dat$n_ahead))) +
    labs(y = "Model", x = "Country") +
    scale_fill_distiller(palette = "Spectral", ...) +
    theme_bw() +
    theme(legend.position = "top", plot.margin=unit(c(0.25,1,0.25,0.25),"cm"))
  
  panel_width = unit(1,"npc") - sum(ggplotGrob(pl)[["widths"]][-3]) - unit(1,"line")
  pl <- pl + guides(fill = guide_colorbar(barwidth = panel_width, title.position = "top", title.hjust = 0.5))
  
  return(pl)
  
}


plot_score_overall <- function(
  dat, # the data frame with columns id, variable, n_ahead, and score
  weighted = FALSE # whether the scores were weighted
) {
  
  if (weighted) {ylab <- "Weighted average conditional ranked probability score"}
  else {ylab <- "Average conditional ranked probability score"}
  
  pl <- ggplot(dat, aes(x = variable, y = score)) +
    geom_boxplot(stat = "boxplot", position = "dodge") +
    geom_jitter(alpha = .5, shape = 3) +
    facet_wrap(~ n_ahead, labeller = labeller(n_ahead = label_facet(dat$n_ahead))) +
    labs(y = ylab, x = "#days ahead") +
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
    labs(y = "Model", x = "Country",
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

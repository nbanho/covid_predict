require(GGally)

# ggplot theme for Nature style
text_size = 8
update_geom_defaults("text", list(size = text_size))
theme_nature <- function () { 
  theme_classic(base_size = text_size, base_family = 'sans') %+replace% 
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 5, 0))
    )
}

theme_bw2 <- function () { 
  theme_bw(base_size = text_size, base_family = 'sans') %+replace% 
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size, face = "bold", hjust = 0, margin = ggplot2::margin(0, 0, 5, 0)),
      plot.subtitle = element_text(size = text_size, hjust = 0, margin = ggplot2::margin(0, 0, 5, 0)),
      plot.caption = element_text(size = text_size - 2, hjust = 1, margin = ggplot2::margin(5, 0, 0, 0)),
      plot.margin = unit(c(0.25,.5,0.25,0.25), "cm"),
      panel.spacing = unit(1, "lines"),
      legend.position = "top", 
      legend.justification = "left"
    )
}

# Correlation plot 
# Helpful code:
# https://stackoverflow.com/questions/44961437/how-to-include-density-coloring-in-pairwise-correlation-scatter-plot
# https://stackoverflow.com/questions/37889222/change-colors-in-ggpairs-now-that-params-is-deprecated?answertab=votes#tab-top
contours <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d() +
    scale_x_continuous(breaks = seq(-.6,0,.2)) +
    scale_y_continuous(breaks = seq(-.6,0,.2))
  p
}

# Plot for correlation
contours <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d() #+
  #scale_x_continuous(breaks = seq(-.8,.4,.4)) +
  #scale_y_continuous(breaks = seq(-.8,.4,.4))
  p
}
hex <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_hex() #+
  #scale_x_continuous(breaks = seq(-.8,.4,.4)) +
  #scale_y_continuous(breaks = seq(-.8,.4,.4))
  p
}
cor_fun <- function(data, mapping, method="pearson", ndp = 2, sz = 5, stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  #lb.size <- sz* abs(est) 
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data=data, mapping=mapping) + 
    annotate("text", x = mean(x, na.rm = TRUE), y = mean(y, na.rm= TRUE), label = lbl, size= sz,...)+
    theme(panel.grid = element_blank())
}
hist_fun <- function(data, mapping) {
  p <- ggplot(data, mapping) +
    geom_histogram(bins = 20) #+
  #scale_x_continuous(limits = c(-1, 1), breaks = seq(-.8, .4, .4))
}



save_plot <- function(pl, pdf_file = NULL, tikz_file = NULL, w = 8, h = 4) {
  if (!is.null(pdf_file)) {
    ggsave(pdf_file, pl, width = w / cm(1), height = h / cm(1)) 
  }
  if (!is.null(tikz_file)) {
    tikz(tikz_file, width = w / cm(1), height = h / cm(1))
    print(pl)
    dev.off()
  } 
}


label_facet <- function(original_var, custom_name){
  lev <- levels(as.factor(original_var))
  lab <- paste0(custom_name, ": ", lev)
  names(lab) <- lev
  return(lab)  
}


#' Make first letter upper case
#' @param x string
#' 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

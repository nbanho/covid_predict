# plot observed vs predicted target
plot.predict <- function(
  date, # date
  target, # target
  pred # matrix with posterior predictions
) {
  
  # true
  true <- data.frame(date = as.Date(date), value = target)
  
  # number of predictions
  n <- nrow(pred)
  
  # predictions as data frame
  pred_df <- data.frame(t(pred)) %>%
    set_names(as.character(tail(true, n)$date)) %>%
    gather() %>%
    mutate(key = as.Date(key))
  
  ggplot() +
    geom_line(data = true, mapping = aes(x = date, y = value)) +
    stat_lineribbon(data = pred_df, mapping = aes(x = key, y = value), .width = c(.95, .80, .50), alpha = 1/4, color = "blue") +
    theme_bw()
  
}

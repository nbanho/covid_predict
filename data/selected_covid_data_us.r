# libraries
library(covidcast)
library(tidyverse)
library(highcharter)
library(lubridate)
source("helper/plotting.r")
source("settings/defaults.r")

# selected US states
# time series data for all states
df_all_us <- aggregate_signals(
  covidcast_signals(data_source = rep("jhu-csse", 2),
                    signal = c("confirmed_incidence_num", "confirmed_incidence_prop"),
                    geo_type = "state", start_day = "2020-03-01", end_day = "2021-06-01"),
  format = "long") %>%
  data.frame()

# plot all states
pdf("data/us-states-incidence.pdf", w = 21 / cm(1), h = 14 / cm(1))
for (geo in unique(df_all_us$geo_value))
print(
  df_all_us %>%
    dplyr::filter(geo_value == geo) %>%
    ggplot(aes(x = time_value, value)) +
    geom_line() +
    facet_wrap(~ signal, scales = "free_y") +
    ggtitle(geo) +
    theme_bw()
)
dev.off()


# investigate selected states more closely
selected_states <- c("az", "ca", "il", "md", "nj", "ny")
df_all_us %>%
  dplyr::filter(signal == "confirmed_incidence_prop") %>%
  dplyr::filter(geo_value %in% selected_states) %>%
  hchart('line', hcaes(x = 'time_value', y = 'value', group = "geo_value"))

# selected time period
start_date <- as.Date("2020-03-15")
end_date <- as.Date("2021-03-15")
df_sel_us <- aggregate_signals(
  covidcast_signals(data_source = rep("jhu-csse", 2),
                    signal = c("confirmed_incidence_num", "confirmed_incidence_prop"),
                    geo_type = "state", start_day = start_date %m-% days(56+n_preds-1), end_day = end_date,
                    geo_values = selected_states),
  format = "wide") %>% # Data not fetched for the following days: 2020-01-15, 2020-01-16, 2020-01-17, 2020-01-18, 2020-01-19, 2020-01-20, 2020-01-21 
  data.frame() %>%
  set_names(c("state", "date", "cases", "incidence")) %>%
  mutate(state = toupper(state))

df_sel_us %>%
  hchart('line', hcaes(x = 'date', y = 'incidence', group = "state"))

# impute negative values
df_sel_us %>% dplyr::filter(incidence < 0)
df_sel_us <- df_sel_us %>%
  mutate_at(vars(cases, incidence), ~ ifelse(. < 0, NA, .)) %>%
  group_by(state) %>%
  arrange(date) %>%
  tidyr::fill(cases, incidence, .direction = "down") %>% 
  ungroup()

# save data
saveRDS(df_sel_us, "data/us-selected-states_ts-date.rds")

# generate training and prediction data
date_seq <- seq.Date(start_date %m-% days(n_preds-1), end_date, by = "day")
for (st in toupper(selected_states)) {
  df_st <- dplyr::filter(df_sel_us, state == st)
  list_st_train <- list()
  list_st_test <- list()
  for (d in 1:length(date_seq)) {
    list_st_train[[d]] <- df_st %>% 
      dplyr::select(date, cases, incidence) %>%
      dplyr::filter(date < date_seq[d]) %>% 
      dplyr::filter(date >= date_seq[d] %m-% days(56))
    list_st_test[[d]] <- df_st %>% 
      dplyr::select(date, cases, incidence) %>%
      dplyr::filter(date >= date_seq[d]) %>% 
      dplyr::filter(date < date_seq[d] %m+% days(n_preds)) %>%
      tail(d)
  }
  df_st_train <- tibble(state = st, forecast_date = date_seq, data = list_st_train)
  df_st_test <- tibble(state = st, forecast_date = date_seq, data = list_st_test)
  saveRDS(df_st_train, paste0("data/us-selected-states/", st, "-train.rds"))
  saveRDS(df_st_test, paste0("data/us-selected-states/", st, "-test.rds"))
}


# data for labeling
df_sel_us_sm <- df_sel_us %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(incidence_7d = zoo::rollmean(incidence, k = 7, fill = NA, align = "right")) %>%
  ungroup() %>%
  dplyr::filter(date >= start_date)

# plot for labeling
df_sel_us_sm %>%
  dplyr::filter(state == "IL") %>%
  hchart('line', hcaes(x = 'date', y = 'incidence_7d'))


# labeled plot
states <- toupper(c("az","ca","il","md","nj","ny"))
state_names <- c("Arizona", "California", "Illinois", 
                 "Maryland", "New Jersey", "New York")
names(state_names) <- states

phase_labs <- read_csv("data/us-selected-states_labeled-phases.csv") %>%
  rename(start_date = date) %>%
  group_by(state) %>%
  arrange(start_date) %>%
  mutate(end_date = dplyr::lead(as.character(start_date), 1)) %>%
  ungroup() %>%
  mutate(end_date = ifelse(is.na(end_date), "2021-03-15", end_date)) %>%
  mutate(end_date = as.Date(end_date)) %>%
  mutate(epidemic_phase = firstup(epidemic_phase)) %>%
  mutate(epidemic_phase = factor(epidemic_phase, levels = c(c("Exponential growth", "Subexponential growth", "Plateau", 
                                                              "Subexponential decline", "Exponential decline")))) %>%
  mutate(state = recode(state, !!! state_names))


phases_pl <- ggplot() +
    facet_wrap(~ state) +
    geom_rect(data = phase_labs, mapping = aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = epidemic_phase), 
            alpha = .5) +
    geom_line(data = df_sel_us_sm %>% mutate(state = recode(state, !!! state_names)), mapping = aes(x = date, y = incidence_7d)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_date(expand = c(0,0), breaks = "1 months", date_labels = "%b %y") +
    scale_fill_brewer(palette = "RdBu") +
    labs(y = "Incidence (new cases per 100,000 population)", x = "", fill = "Epidemic phase") +
    theme_bw2() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
    guides(fill = guide_legend(override.aes = list(colour = "gray")))

phases_pl

save_plot(phases_pl, "data/us-selected-states_phases_plot.png", w = 16, h = 12)
save_plot(phases_pl, "data/us-selected-states_phases_plot.pdf", w = 16, h = 12)


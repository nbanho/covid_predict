# models
models <- c("epiestim", "epinow2", "epidemia", "arima", "prophet", "gp")
model_names <- c("EpiEstim", "EpiNow2", "Epidemia", "ARIMA", "Prophet", "GP")
names(model_names) <- models

# states
states <- c("az","ca","il","md","nj","ny")
state_names <- c("Arizona", "California", "Illinois", 
                 "Maryland", "New Jersey", "New York")
names(state_names) <- states
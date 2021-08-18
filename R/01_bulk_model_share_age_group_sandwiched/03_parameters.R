# Parameters are shared by SOCSIM and model estimates

# Shared ----

# country_keep <- c("united states of america", "guatemala", "sweden", "niger")
country_keep <- unique(ASFRC_all$country)

numCores <- 3

# For plotting

# For plotting specific cohorts in grid with countries
cohort_small <- c(1999, 2009, 2029, 2039)

# # SOCSIM 

min_age_socsim <- 15
max_age_socsim <- 100

age_range_socsim <- min_age_socsim:max_age_socsim

# Determine which ages to consider only
ages_socsim_in_months <- (min_age_socsim*12):(max_age_socsim*12)

print("Achtung! The chosen cohorts must be between 1999 and 2040")

# Confidence intervals for plots

ci <- 0.95
ci_lab <- paste0(ci*100, "%")

# Model ---- 

cos_model <- seq(2000, 2020, 5)

# 20200501: I increased age to 65 to be able to change kappa and tau parameters
min_age_model <- 15
max_age_model <- 70
age_range_model <- min_age_model:max_age_model

if(max_age_model + max(cos_model) > 2100) stop("You are asking for data beyonf 2100, which is not in WPP")

# Determine which ages to consider only
ages_model_in_months <- (min_age_model*12):(max_age_model*12)

# Reproductive age
rep_age <- xs <- 15:49

# Plot ----

ages_to_plot <- 15:70

labs_p <- c("Model", "SOCSIM (one line per simulation run)")

countries_plot <- c("niger","guatemala", "sweden")

cohort_small <- c(1999, 2029)
line_size <- 1
alpha <- 0.2
w_p <- 5
h_p <- 3


sources_p <- c(
  "Model"
  , paste0("SOCSIM (",ci_lab ," CI interval from ",2," simulation runs)")
)

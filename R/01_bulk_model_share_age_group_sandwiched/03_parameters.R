# Parameters are shared by SOCSIM and model estimates

# Shared ----

# country_keep <- c("united states of america", "guatemala", "sweden", "niger")
country_keep <- unique(ASFRC_all$country)

numCores <- 3

# For plotting

# For plotting specific cohorts in grid with countries
cohort_small <- c(1999, 2009, 2029, 2039)

# # SOCSIM 
# 
# # Run on a single simulation output or use multiple
# # simulations? 
# # If T, this will take considerably longer
# different_simulation_runs <- T
# 
# # Countries included
# countries_in_socsim <- tolower( gsub("_[0-9]+", "", sim_names) )
# countries_in_socsim[countries_in_socsim == "usa"] <- "united states of america"

## check .sup file and give this careful consideration
# FinalSimYear <- 2145  
## last month of simulation See Socsim output
# endmo <- 6541  

# FinalSimYear <- 2200
# endmo <- 1798

# These should work for the SOCSIM 2000-2100 opop
# FinalSimYear <-  2099
# endmo <-  1800

# 20200414 This should work for new estimates up to 2200
# FinalSimYear <-  2200
# endmo <-  5400
# 
# 
# # Not sure what this is
# EndYr <- endmo:(endmo - 11)
# 
# # For filtering data:
# 
# # For estimating the average of cohorts:
# # cos <- c(1995:2045)
# 
# # FOR TESTING
# cos <- c(1999, 2009, 2029, 2039)

# min_age <- 0
# max_age <- 90

min_age_socsim <- 15
max_age_socsim <- 100

age_range_socsim <- min_age_socsim:max_age_socsim

# Determine which ages to consider only
ages_socsim_in_months <- (min_age_socsim*12):(max_age_socsim*12)

# ages_to_keep_socsim <-  split(rep(ages_socsim_in_months, length(sim_names)), sort(rep(sim_names, length(ages_socsim_in_months))))

# For creating cohorts from birth years
# Define cohort breaks
# First, how many years does a cohort have?

# For 10-year cohorts
# by <- 10
# br <- seq(min(cos), max(cos), by)
# labs <- seq(min(cos)+ floor(by/2), max(cos) - floor(by/2), by)

# For 5-year cohorts
# by <- 5
# br <- seq(1997, 2042, by)
# labs <- seq(min(br) + floor(by/2), max(br) - floor(by/2), by)

print("Achtung! The chosen cohorts must be between 1999 and 2040")

# Confidence intervals for plots

ci <- 0.95
ci_lab <- paste0(ci*100, "%")

# Model ---- 


# cos_model <- c(2000, 2020)
cos_model <- seq(2000, 2020, 5)

# FOR TESTING
# cos_model <- c(2000)

# For the model, the values must be between 1999 and 2040
# if the max_age is 55
# COmmented out so they will be inherited from previous
# script

# sim_names <- unique()

# 20200501: I increased age to 65 to be able to change kappa and tau parameters
min_age_model <- 15
max_age_model <- 70
age_range_model <- min_age_model:max_age_model

if(max_age_model + max(cos_model) > 2100) stop("You are asking for data beyonf 2100, which is not in WPP")

# Determine which ages to consider only
ages_model_in_months <- (min_age_model*12):(max_age_model*12)

# ages_to_keep_model <-  split(rep(ages_model_in_months, length(sim_names)), sort(rep(sim_names, length(ages_model_in_months))))

# Reproductive age
rep_age <- xs <- 15:49

# Plot ----

# ages_to_plot <- 15:60
ages_to_plot <- 15:70

labs_p <- c("Model", "SOCSIM (one line per simulation run)")

countries_plot <- c("niger","guatemala", "sweden")
# cohort_small <- c(1999, 2039)

cohort_small <- c(1999, 2029)
# cohort_small <- c(1999, 2009, 2029, 2039)
line_size <- 1
alpha <- 0.2
w_p <- 5
h_p <- 3


sources_p <- c(
  "Model"
  , paste0("SOCSIM (",ci_lab ," CI interval from ",2," simulation runs)")
)

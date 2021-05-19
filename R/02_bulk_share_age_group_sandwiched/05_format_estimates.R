# library(tidyverse)
# 1. Load estiamates ----

# Parameters for running interactively:
# library(tidyverse)
# library(countrycode)
# setwd("../..")
# source("R/functions.R")
# Define type of sandwich (menu) in 03_parameters


pattern <- paste0("^socsim_", pat)

socsim_estimates_files <- list.files(path = "Data/estimates", pattern = pattern, full.names = T)
socsim_estimates_files <- socsim_estimates_files[!grepl("aggregate", socsim_estimates_files)]
socsim_estimates_files <- socsim_estimates_files[!grepl("_full", socsim_estimates_files)]
socsim_estimates_l <- lapply(socsim_estimates_files, read.csv, stringsAsFactors = F)

# Get full df of all countries and cohorts

sand_socsim_full <- 
  data.frame(do.call(rbind, socsim_estimates_l)) %>% 
  select(-source, -kappa_tau, - birthYr) %>% 
  fix_socsim_countries(keep_regions = T)
  
# Export -----

write.csv(
  sand_socsim_full
  , paste0("Data/estimates/socsim_",pat, "full.csv")
  , row.names = F)

print(paste0("Full file with all countries and simulations saved!"))
print(paste0("Data/estimates/socsim_",pat, "full.csv"))

# 4. Aggregate by cohort  and get CI ~~~~~~~~~~~~~~

# We want a df

sand_socsim_aggregated_simulations <-
  sand_socsim_full %>%
  # Group by simulaiont run
  # i.e. get rid of simu;ation columns
  group_by(country, cohort, age) %>%
  dplyr::summarise(
    lower = confidence_interval(value, type = "lower", ci = ci)
    , upper = confidence_interval(value, type = "upper", ci = ci)
    , value = mean(value)
  ) %>%
  ungroup() 
  

# 3. Export --------------

write.csv(
  sand_socsim_aggregated_simulations
  , paste0("Data/estimates/socsim_",pat, "aggregate.csv")
  , row.names = F)

print(paste("Aggregated",type,"estimate df exported!"))
print(paste0("Data/estimates/socsim_",pat, "aggregate.csv"))

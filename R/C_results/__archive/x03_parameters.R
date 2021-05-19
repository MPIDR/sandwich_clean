# Parameters are shared by SOCSIM and model estimates

# Shared 

# country_keep <- c("united states of america", "guatemala", "sweden", "niger")
# country_keep <- c("guatemala", "sweden", "niger")
# country_keep <- c("guatemala", "japan")

# numCores <- 10

# Convert SOCSIM raw output to csv files
# save_rsaves_as_csv <- T

# For plotting

# For plotting specific cohorts in grid with countries
# cohort_small <- c(1999, 2009, 2029, 2039)

# SOCSIM ----

# 1.2. Re-estimate sandwich? ====
# SHould sandwichness prevalence be estimated all over again?
# Normally, I won't compute SOCSI estimates everytime I run this
# since it takes a while. Rather, just read the estimates computed 
# in the cluster for a selected numer of countries
# estimate_sandwich <- F

# 1.3 Define cohorts to compute ====

# For estimating the average of cohorts:

  # cos <- c(1970: 2040)
  # br <- c(cos[1] - 1, cos)
  # labs <- cos
  # 
  # cos_or <- cos
  # cos_size <- 1



# Confidence intervals for plots

# ci <- 0.95
# ci_lab <- paste0(ci*100, "%")
# 
# # Plot 
# 
# 
# lab_g <- c("Sandwiched", "Grandsandwiched")
# # br_g <- sort(unique(comb$level), decreasing = T)
# br_g <- c("sandwich", "grandsandwich")
# 
# # cohort_plot <- cos_or[floor(seq(1, length(cos_or), length.out = 4))]
# 
# cohort_plot <- c(1965, 2054)
# 
# ages_to_plot <- 15:80

# base_size <- 10
# regions_to_ignore <- c("AUS & NZ", "Oceania (other)")
# 
# x_lim <- c(1970, 2040)

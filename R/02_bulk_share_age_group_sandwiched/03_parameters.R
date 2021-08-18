
# !!! Define menu_choice ---------------------

# This will determine which parameters are chosen for each type of 
# generational squeeze. Depending on the choice of sandwich, 
# the parameters.r script assigns the corresponding values.
# Note that the super-asssignemtn operator is required if running
# this from job scripts <<-

# Sandwich
# menu_choice <<- "sand_full"
# menu_choice <<- "sand_blood"
# menu_choice <<- "sand_female"

# Grandsandwich
# menu_choice <- "gsand_full"
# menu_choice <<- "gsand_blood"
# menu_choice <<- "gsand_female"


if(menu_choice == "sand_full"){
  type <- "sandwich"
  cohorts_to_keep <- "every_5"
  kappa <- switch(type, sandwich = 18, grandsandwich = 15)
  tau = 5
  in_laws <- TRUE
  sex_keep <-  "both"
} else if(menu_choice == "sand_blood"){
### Blood sandwich
# Same as full sandwich, but ignores in-laws.
  type <- "sandwich"
  cohorts_to_keep <- "every_5"
  kappa <- switch(type, sandwich = 18, grandsandwich = 15)
  tau = 5
  in_laws <- F
  sex_keep <-  "both"
} else if(menu_choice == "sand_female"){
### Female sandwich
# For comparing to formal model.
  type <- "sandwich"
  cohorts_to_keep <- "every_5"
  kappa <- switch(type, sandwich = 18, grandsandwich = 15)
  tau = 5
  in_laws <- T
  sex_keep <-  "women"
} else if(menu_choice == "sand_medium") {
  type <- "sandwich"
  # cohorts_to_keep <- "every_5"
  cohorts_to_keep <- "minimal"
  kappa <- switch(type, sandwich = 15, grandsandwich = 15)
  tau = 5
  in_laws <- TRUE
  sex_keep <-  "both"
} else if(menu_choice == "gsand_full"){
### FUll grand-sandwich
  type <- "grandsandwich"
  # cohorts_to_keep <- "every_5"
  cohorts_to_keep <- "minimal"
  kappa <- switch(type, sandwich = 15, grandsandwich = 15)
  tau = 5
  in_laws <- TRUE
  sex_keep <-  "both"
} else if(menu_choice == "gsand_blood"){
### Blood grand-sandwich
# Same as full sandwich, but ignores in-laws.
  type <- "grandsandwich"
  cohorts_to_keep <- "every_5"
  kappa <- switch(type, sandwich = 18, grandsandwich = 15)
  tau = 5
  in_laws <- F
  sex_keep <-  "both"
} else if(menu_choice == "gsand_female"){
### Female grand-sandwich
# For comparing to formal model.
  type <- "grandsandwich"
  cohorts_to_keep <- "every_5"
  kappa <- switch(type, sandwich = 18, grandsandwich = 15)
  tau = 5
  in_laws <- T
  sex_keep <-  "women"
}

print("%^%^%^%^%^%^%^%^%^")
print(paste("WORKING ON", menu_choice, "!!!"))
Obj <- list(
  type = type, 
  cohorts_to_keep = cohorts_to_keep
  , kappa = kappa
  , tau = tau
  , in_laws = in_laws
  , sex_keep = sex_keep
)
print(Obj)
print("%^%^%^%^%^%^%^%^%^")

# # !!.1. Countries to run the analysis for =========================


# 
countries_to_analyse <- "by_region"
countries_ignore <- ""
# 

# !!.2 Cohorts to compare ====================

#  Parameters stay the same ----------------------

# Decide whether countries for which I have already estimated sandwicheness
# should be estimated again...
keep_only_new  <-  T

# 0. OS ---------------------------

numCores <- switch(os, Windows = 25, Linux = 10)
print(paste0("numCores =", numCores))

# SOCSIM ----

in_law_lab <- ifelse(in_laws, "in_laws", "blood_kin")


print(paste("Keeping sex:", sex_keep))

if(in_laws) print("INCLUDING IN-LAWS!!")

print("^~^~^~^~^~^~^~^~^~^~^~")
print(paste("Since type is",type, "then, kappa =", kappa, "and tau = ", tau))
print("^~^~^~^~^~^~^~^~^~^~^~")


# 1.3 Define cohorts to compute ====

cohorts_labs <- paste0("cohort_", cohorts_to_keep)

# For estimating the average of cohorts:
# All cohorts - too long
if(cohorts_to_keep == "all"){
  
  cos <- c(1970: 2040)
  
  br <- c(cos[1] - 1, cos)
  labs <- cos
  
  cos_or <- cos
  cos_size <- 1

} else if(cohorts_to_keep == "some_years"){
  # REduced number of cohorts for fast run
  
  # If we only want two cohorts, we need to get those +/- two, to get the average values
  
  # Group with cohorts +/- n years appart
  cos_size <- 2
  cos_or <- floor(seq(1965, 2090, length.out = 15))
  
  if(cos_size == 1) cos <- sort(c(cos_or, cos_or -cos_size, cos_or +cos_size))
  if(cos_size == 2) cos <- sort(c(cos_or, cos_or - 1, cos_or - cos_size, cos_or + 1, cos_or +cos_size))

  # For 5-year cohorts
  br <- sort(c(cos_or - (cos_size +1), cos_or + (cos_size + 1)))
  
  labs <- rep(NA, (length(cos_or)*2 - 1))
  labs[seq(1,length(labs), length.out = length(cos_or))] <- cos_or
  
} else if(cohorts_to_keep == "max_min"){
  
  cos <- c(1970, 2040)
  
  br <- c(cos[1] - 1, cos)
  labs <- cos
  
  cos_or <- cos
  cos_size <- 1
  
} else if(cohorts_to_keep == "every_5"){
  
  cos <- seq(1970, 2040, 5)
  
  br <- c(cos[1] - 1, cos)
  labs <- cos
  
  cos_or <- cos
  cos_size <- 5
  
} else if(cohorts_to_keep == "minimal"){
  cos <- c(1970, 2005, 2040)
  
  br <- c(cos[1] - 1, cos)
  labs <- cos
  
  cos_or <- cos
}

print(paste("Looking at cohorts", cohorts_to_keep, "-", paste(cos, collapse = ",")))

# At the moment 20200423, only these ohorts are allowd since, they are the only
# ones for which sandwishcness can be estimated from the models for the entire lifecourse, 
# given the way that period and cohort combinations are required but not available from the UNWPP
# data. If onyl doing SOCSIM, this doesn't matter

# Run on a single simulation output or use multiple
# simulations? 
# If T, this will take considerably longer
different_simulation_runs <- T

# Get names of simulation files
sim_names <- gsub(
  "^socsim_opop_"
  , ""
  , list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)
)

sim_names_full <- list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)

# Countries included
countries_in_socsim <- tolower( gsub("_[0-9]+", "", sim_names) )
countries_in_socsim[countries_in_socsim == "usa"] <- "united states of america"

## check .sup file and give this careful consideration

# 20200414 This should work for new estimates up to 2200
FinalSimYear <-  2200
endmo <-  5400


# Not sure what this is
EndYr <- endmo:(endmo - 11)

# 1.4. Age parameters ====

min_age_socsim <- 15
max_age_socsim <- 100

age_range_socsim <- min_age_socsim:max_age_socsim

# Determine which ages to consider only
ages_socsim_in_months <- (min_age_socsim*12):(max_age_socsim*12)

# Confidence intervals for plots

ci <- 0.95
ci_lab <- paste0(ci*100, "%")

# Model ---- 

# For the model, the values must be between 1999 and 2040
# if the max_age is 55

min_age_model <- 15
max_age_model <- 55

age_range_model <- min_age_model:max_age_model
# 
# # Determine which ages to consider only
ages_model_in_months <- (min_age_model*12):(max_age_model*12)

# # Reproductive age
rep_age <- xs <- 15:49

# 3. Un regions -----------

# Add county code to UN regions
# Only need to do this once
# I did this 20200506

if(F) {
  un_regions <- 
    un_regions %>% 
    mutate(level1_iso3 = countrycode(level1, origin = "country.name", destination = "iso3c", warn = F))
  
  un_regions$level1_iso3[un2$level1 %in% c("eswatini", "channel islands", "less developed regions, excluding china")] <- c("SWZ", "CYM", NA)  
  
  write.csv(un_regions, "Data/wpp_data/un_regions.csv", row.names = F)
}

# Define path!! ===============
# For saving and importing and excluding simulations
# Old pattern, without cohorts_labs

# New patterns
pat <- paste0(type,"_", sex_keep, "_", cohorts_labs, "_", in_law_lab, "_", kappa, "_", tau, "_")

print(paste("Path is", pat))

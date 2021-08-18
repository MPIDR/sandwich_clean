# !!! Define menu_choice ---------------------

# This will determine which parameters are chosen for each type of 
# generational squeeze. Depending on the choice of sandwich, 
# the parameters.r script assigns the corresponding values.
# Note that the super-asssignemtn operator is required if running
# this from job scripts <<-

# Note that the estiamtes include sandwich and grandsandwich so no need
# to run separately

if(menu_choice == "small_affinal"){
  tau <- 5
  kappa_kids <- 5
  kappa_gkids <- 5
  cohorts_to_keep <- "every_5"
  include_in_laws <- T
  sex_keep <-  "both"  
} else if(menu_choice == "medium_affinal"){
  tau <- 5
  kappa_kids <- 15
  kappa_gkids <- 15
  # cohorts_to_keep <- "every_5"
  cohorts_to_keep <- "minimal"
  include_in_laws <- T
  sex_keep <-  "both"    
} else if(menu_choice == "large_affinal"){
  tau <- 5
  kappa_kids <- 18
  kappa_gkids <- 18
  cohorts_to_keep <- "every_5"
  include_in_laws <- T
  sex_keep <-  "both"  
} else if(menu_choice == "medium_blood"){
  tau <- 5
  kappa_kids <- 15
  kappa_gkids <- 15
  cohorts_to_keep <- "every_5"
  include_in_laws <- F
  sex_keep <-  "both"    
} else if(menu_choice == "female_medium"){
  # The existing code is for a two-sex population
  # If you want sex-specific estimates that would require tweaking with the
  # functions, but maybe not needed??
  stop("Not specified")
}

print("%^%^%^%^%^%^%^%^%^")
print(paste("WORKING ON", menu_choice, "!!!"))
Obj <- list(
  menu_choice = menu_choice
  , cohorts_to_keep = cohorts_to_keep
  , kappa_kids = kappa_kids
  , kappa_gkids = kappa_gkids
  , tau = tau
  , include_in_laws = include_in_laws
  , sex_keep = sex_keep
)
print(Obj)
print("%^%^%^%^%^%^%^%^%^")


# PArameters --------------


if(cohorts_to_keep == "all"){
  brtYR <- seq(1970, 2040, by = 1) #~ specify birth years of cohorts of interest
} else if(cohorts_to_keep == "every_5") {
  brtYR <- seq(1970, 2040, by = 5) #~ specify birth years of cohorts of interest
} else if(cohorts_to_keep == "minimal") {
  brtYR <- c(1970, 2005, 2040) #~ specify birth years of cohorts of interest
} 



# For exporting
cohorts_labs <- paste0("cohort_", cohorts_to_keep)
in_law_lab <- ifelse(include_in_laws, "in_laws", "blood_kin")

os <- Sys.info()['sysname']

# Function parameters --------------
# For converting SOCSIM months to SOCSIM years
FinalSimYear <- 2200
endmo <-5400

numCores <- switch(os, Windows = 25, Linux = 10)


# Define path!! ===============
# For saving and importing and exluding siulations
# Old pattern, without cohorts_labs

# New patterns
pat <- paste0(sex_keep, "_", cohorts_labs, "_", in_law_lab, "_k_s_", kappa_kids, "_k_gs_", kappa_gkids, "_", tau)

Cave <- switch(os, 
               Linux = "../90days/SandwichRESULTS/preliminary_june20"
               , Windows = "../socsim_all"
)

ResultsRepo <- "Data/temp"

out_temp_specific <- paste0(ResultsRepo, "/", pat)

print(paste("Path is", out_temp_specific))

if(os == "Windows"){
  if(!dir.exists(out_temp_specific)) {
    dir.create(out_temp_specific)
    print(paste0("Created directory: ", out_temp_specific)) 
  } else {
    print(paste0("Directory: ", out_temp_specific, " already existed")) 
    }
} else if(os =="Linux") {
  stop("Pending")
}

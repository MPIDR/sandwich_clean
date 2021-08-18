
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------
# CHOSE FROM THE GRANDSANDWICH MENU ---------------------------------------
# This will determine which parameters are chosen for each type of 
# generational squeeze. Depending on the choice of sandwich, 
# the parameters.r script assigns the corresponding values.
# Note that the super-asssignemtn operator is required if running
# this from job scripts <<-

menu_choice <<- "gsand_full"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------

setwd("../..")

(files <- list.files(path = "R/02_bulk_share_age_group_sandwiched", pattern = ".R$", full.names = T))

# Load functions ##############
source(files[grepl("load_functions.R", files)])

# Load data ###################
source(files[grepl("load_data.R", files)])


# Define parameters ###################
source(files[grepl("parameters.R", files)])


# SOCSIM sandwich #################
# Normally, I won't compute SOCSI estimates everytime I run this
# since it takes a while. Rather, just read the estimates computed 

# Run in a loop for each region separaetly

regions <- c(
  "LATAM & Caribbean"
  , "N Africa & W Asia"
  )

print(paste0("REGIONS WILL BE PROCESSED IN THIS ORDER: ", paste(regions, collapse = " - " )))

for(region_n in regions){
  
  print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
  print(paste0("^~^~^~^~^~^~ TOP LEVEL CALL ^~^~^~^~^~^~"))
  print(paste0("ESTIMATING SANDWICHNESS FOR ALL COUNTRIES ORDERED BY REGION"))
  print(paste0("CURRENT REGION IS ", region_n))
  print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
  
  region_to_keep <<- region_n
  
  source(files[grepl("estimate_prevalence_by_age.R", files)])
  
}

print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~ SANDWICH ESTIMATES COMPELTE ^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))

# Format sandwich output ###############

# source(files[grepl("format_estimates.R", files)])
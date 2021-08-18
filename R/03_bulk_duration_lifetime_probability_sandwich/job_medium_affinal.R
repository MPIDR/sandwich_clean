
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHOSE FROM THE menu_choice
# This will determine which parameters are chosen for each type of 
# generational squeeze. Depending on the choice of sandwich, 
# the parameters.r script assigns the corresponding values.
# Note that the super-asssignemtn operator is required if running
# this from job scripts <<-

# Sandwich
# 20201106: # USE medium_affinal!!!
menu_choice <<- "medium_affinal"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd("../..")

(files <- list.files(path = "R/03_bulk_duration_lifetime_probability_sandwich", pattern = ".R$", full.names = T))

# Load functions ##############
source(files[grepl("load_functions.R", files)])

# Load data ###################
source(files[grepl("load_data.R", files)])


# Define parameters ###################
source(files[grepl("parameters.R", files)])

print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~ STARTING DURATION ESTIMATES ^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))

# Run estimates in parallel
source(files[grepl("parallel_estimates", files)])


print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~ DURATION ESTIMATES COMPELTE ^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
print(paste0("^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~"))
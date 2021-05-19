# NOT RUN
# Just as an example of how this works
# The function should actuallz be estimated from the script using the 
# parallel processing

# Code adapted from Carl's 
# All functions for the basic operations should be in:
# 01ProcessSim.r and 01CrunchSimCohort.r

# sfile is rsave file
# Try this out first with one simulation by replacing this with the location 
# of a given Rsaves simulation file:

if(detectCores() > 20){
  # Stem <- "../socsim_all/USA_Medium.sup/246306/SimResults/sims"
  # Stem <- "../socsim_all/United_Republic_of_Tanzania_Medium.sup/228391/SimResults/sims"
  Stem <- "../socsim_all/Afghanistan_Medium.sup/205969/SimResults/sims"
  sfile <- paste(Stem,".Rsave",sep='')  
} else {
  Stem <- "C:/Cloud/Projects/sandwich/Data/socsim/Guatemala_Medium.sup/433035/SimResults/sims" 
  sfile <- paste(Stem,".Rsave",sep='')
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20201104
# ISSUE 
# problem with TZA is that birth cohrots only include:# 1750-1901
# it should include 1751-2200
# SOLUTION:
# Fix simulation file
# DIAGNOSTIC:
# load("../socsim_all/United_Republic_of_Tanzania_Medium.sup/228391/SimResults/sims.Rsave")
# opop <- add2opop(sims$opop,omar)
# range(opop$cohort)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This returns a list with varying number of elements depending on what we ask the function 
# to return.
# In general, it returns one estimate per cohort (e.g average number of months sandwiched 
# over the lifecourse)
# Possible improvements:
# - Return for men and women separately
# - Return age-specific duration of sandwichness (currently sums over all ages)
res <- 
  CrunchSim(
    sfile
    , Obj = NA
    , kappa_kids = 15
    , kappa_gkids = 15
    , tau = 5
    , brtYR = c(1970, 2040)
    # DECLARE which outputs you would like the function to return:
    # Number of months, on average, that members of each birth cohort will spend in 
    # a state of sandwichness throughout all of their lifes
    , duration_of_sandwich = T
    # If include_in_laws, parents in law (of last spouse only) will also be considered
    # when estimating the months spent sandwiched
    , include_in_laws = T
    , first_experience_of_death = F
    , kinship_network_size = F
  )

res

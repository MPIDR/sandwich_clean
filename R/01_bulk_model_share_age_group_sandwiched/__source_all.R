# 20200415
# Estimates probability of sanswichness over the life course
# from SOCSIM output and using model estimates (using 2019 UNWPP)

# For multiple countries 
# FOr Testing: US, Guatemala and Sweden
# Note that this runs all on the new sandwich socsim simulation
# data produced by Carl on 20200415 - that measn that all data from 
# the MPIDR SOCSIM course was ditched!

# Run socsim estimates in cluster because they take too long but, wih only
# three countries, the rest can be run in laptop locally since it's not 
# too large or long. 



# PENDING:
# - Change SOCSIM estimates so that they refer to 
# - Scale up for all cohorts and countries
# - Update SOCSIM models with UN WPP 2019 data
# - Change function parameters to amake it easy to dchange definition of sandwiched
#   (eg children under 10 years)
# - Estimate lx_df_full for all countries

(files <- list.files(pattern = ".R$")[-1])

# Load functions
source(files[1])

# Load data
source(files[2])

# Define parameters
source(files[3])

# 04_model_estimates_discrete
source(files[4])

# 05_model_estimates_stable_pop
source(files[5])
# 
# # Plot means
# source(files[6])
# 
# 
# source(files[7])
# 
# source(files[8])
# 
# source(files[9])
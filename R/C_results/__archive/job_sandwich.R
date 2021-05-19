
(files <- list.files(pattern = ".R$")[-1])

# Load functions
source(files[1])

# Load data
source(files[2])

# Define parameters
source(files[3])

# Overwrite par
estimate_sandwich <- T

# SOCSIM estimates
# Normally, I won't compute SOCSI estimates everytime I run this
# since it takes a while. Rather, just read the estimates computed 
# in the cluster for a selected numer of countries
# Since this is just a preliminary 'test', 
# there is no need to run for all countries but only
# for a selected few. 
# In 20200417, these were Guatemala, US, Sweden and Niger
source(files[4])

# grandsand
source(files[5])
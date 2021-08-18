
# Convert SOCSIM raw output to csv files
save_rsaves_as_csv <- F

# 1. UN regions ====

un_regions <- read.csv(file = paste0("Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

# 2. SOCSIM opops ----

os <- Sys.info()['sysname']

# Berlekely
if(os == "Linux"){
  Cave <- "../90days/SandwichRESULTS/preliminary_june20"
# MPIDR Hydra cluster
} else if(os == "Windows") {
  Cave <- "../socsim_all"
}


# These files are saved in Carl's Rsave format in different folders, as they are
# saved by the SOCSIM simulations in Keyfitz

# Use this function to unpack them and save as csv files the opop and omar

if(save_rsaves_as_csv){
  unpack_simulation_rsaves(Cave = Cave)
}


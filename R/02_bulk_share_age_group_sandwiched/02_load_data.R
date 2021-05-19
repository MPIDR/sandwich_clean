
# Convert SOCSIM raw output to csv files
save_rsaves_as_csv <- F

# 1. UN regions ====

un_regions <- read.csv(file = paste0("Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

# 2. SOCSIM opops ----

os <- Sys.info()['sysname']

# Berlekely
if(os == "Linux"){
  Cave <- "../90days/SandwichRESULTS/preliminary_june20"
# Hydra
} else if(os == "Windows") {
  Cave <- "../socsim_all"
  # Cave <- "Data/socsim/"
}


# These files are saved in Carl's Rsave format in different folders, as they are
# saved by the SOCSIM simulations in Keyfitz

# Use this function to unpack them and save as csv files the opop and omar

if(save_rsaves_as_csv){
  unpack_simulation_rsaves(Cave = Cave)
}

# Load csv files ----

# Get names of simulation files
# sim_names <- gsub(
#   "^socsim_opop_"
#   , ""
#   , list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)
# )
# 
# sim_names <- gsub(".csv", "", sim_names)

# Load opop  as list

# opops <- list.files(pattern = "^socsim_opop", path = Cave, full.names = T)
# opop_l <- lapply(opops, data.table::fread, stringsAsFactors = F)
# names(opop_l) <- sim_names
# # opop_df <- data.frame(do.call(rbind, opop_l))
# 
# # Load omar 
# 
# omars <- list.files(pattern = "^socsim_omar", path = Cave, full.names = T)
# omar_l <- lapply(omars, data.table::fread, stringsAsFactors = F)
# names(omar_l) <- sim_names

# 2.1.2 Sandwich estimates ====

# With different values of kappa and tau
# socsim_estimates_files <- list.files(path = "Data/estimates", pattern = "^socsim_sandwich_full_prop_", full.names = T)
# socsim_estimates_l <- lapply(socsim_estimates_files, readRDS)
# 
# socsim_estimates_files_grand <- list.files(path = "Data/estimates", pattern = "^socsim_grandsandwich_full_prop_", full.names = T)
# socsim_estimates_l_grand <- lapply(socsim_estimates_files_grand, readRDS)
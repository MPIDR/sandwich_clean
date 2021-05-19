
# 1. Data for model estimates ----

# Need Fertilty and mortality data from UNWPP

# 0. Read wpp data 

# csv version. including with different indicators for the medium scenario

wpp_period_med <- read.csv(
  file = paste0("../../Data/wpp_data/","WPP2019_Period_Indicators_Medium.csv")
  , stringsAsFactors = F
) %>% 
  mutate(
    region = tolower(Location)
    , region = fix_un_countries(region)
  ) %>% 
  select(-Location)

# Note that these are the ungrouped version of the UN data, which is
# groups in 5-age groups for every 5 calendar-year interva
# I did the ungrouping with the scripts in foled WPP_ungroup_data

# 1.1. Period fertility data ====
# UN fertility projections

fert_wpp_per <- wpp_period_med %>% 
  select(
    region
    , period = Time
    , TFR
    , CBR
    , births = Births
    , MAC
  )

# 1.2. Cohort ASFR ====

# , derived from WPP data in previous script
# convert to per 100,000

ASFRC_all <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F) %>% 
  mutate(ASFR = ASFR / 1e3)

## 1.2. Cohort life tables ====

# Female
LTCF_all <- data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame

# 1.2. Female births ----

female_births_all <- read.csv(
  file = paste0("../../Data/derived/","wpp_female_births_1_1.csv")
  , stringsAsFactors = F
)

# 1.3. Women surviving to each age by birth cohort (denominator) ====

# This cannot be obtained from the WPP, which only has period 
# poulations estimates
# Therefore, I need to apply the specific female cohort life table for the
# respective population of women by birth cohort and country/region

# Since these are real numbers, I need to get the size of the female 
# birth cohorts by country and year
# This can be obtained from the WPP estimates of the yearly number of births
# Note that this value is grouped and was ungrouped in another script
# cf WPP_ungroup_age_year

# The function below applies cohort life tables to real-life populatinos
# with the intention of gettin the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data

# lx_df is a df with the number of women surviving up to age 100 for specific
# birth cohort-country cobminations
# Put differently, it is the number of woman at risk of losing a child
# ie the denominator for the absolute measure of child loss

# It was estimated for the child_death analysis (7_[x].R)

lx_df_full <- data.table::fread("../../Data/estimates/lx_df_full.csv", stringsAsFactors = F) %>% 
  data.frame

# Estimate surviving women by applying life tbale 
# to population of girls born in each cohort
# THis is different from the lx_df estimated for child
# death, because it inlcudes cohorts oup to 2099
# even if not all ages are complete
# I need this here since poplation values for cohorts 
# over 2000 are used in the estimation 
# below

  print("Running model estimates from scracth.")
  print("If you trust me, there's no need to do this every time, just load them from the disk!")
  print("This will take arounf 5 minutes.")
  
  if(!exists("lx_df_full")) {
    
    numCores <- ifelse(detectCores() > 8, 25, 2)
    
    lx_df_full <- apply_lt2(
      female_births = female_births_all
      , LTCF = LTCF_all
      , cohorts = 1950:2099
      , numCores = numCores 
    ) 
    
    write.csv(lx_df_full, "../../Data/estimates/lx_df_full.csv", row.names = F)
    
  }
  
# 2. SOCSIM opops 

# Cave <- "../../Data/socsim/"
# 
# # These files are saved in Carl's Rsave format in different folders, as they are
# # saved by the SOCSIM simulations in Keyfitz
# 
# # Use this function to unpack them and save as csv files the opop and omar
# 
# 
# if(save_rsaves_as_csv){
#   unpack_simulation_rsaves(Cave = Cave)
# }
# 
# # Load csv files 
# 
# # Get names of simulation files
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

# 2. SOCSIM Sandwich estimates ====

# With different values of kappa and tau
# socsim_estimates_files <- list.files(path = "../../Data/estimates", pattern = "^socsim_sandwich_proportion_", full.names = T)
# socsim_estimates_l <- lapply(socsim_estimates_files, readRDS)
# 
# sand_socsim_aggregated_simulations <- data.frame(data.table::fread("../../Data/estimates/socsim_sandwich_aggregate.csv", stringsAsFactors = F)) 

# 3. Model sandwich estimates

sand_model <- read.csv("../../Data/estimates/model_sandwich_proportion.csv", stringsAsFactors = F)

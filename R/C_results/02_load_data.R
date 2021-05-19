
# 1. Data for model estimates ----

# Need Fertilty and mortality data from UNWPP

# 0. Read wpp data 

# 1. UN regions ====

un_regions <- read.csv(file = paste0("../../Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

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
  )

# 1.2. Cohort ASFR ====

# , derived from WPP data in previous script
# convert to per 100,000

ASFRC_all <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F) %>% 
  mutate(ASFR = ASFR / 1e3)

## 1.2. UN Cohort life tables ====

# Female
LTCF_all <- data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame


## 1.2. SOCSIM Cohort life tables ====
LTCF_socsim <- 
  data.table::fread(file = paste0("../../Data/derived/","LTC_socsim.csv"), stringsAsFactors = F) %>% 
  filter(sex == "female") %>% 
  select(-sex) %>% 
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

# 2. SOCSIM opops ----


# New sandwich estimates =========
# 20200605

# DECIDE CRITERIA FOR LOADING SANDWIHC DATA
sex_keep <- "both"
# cohorts_labs <- "cohort_every_5"
cohorts_labs <- "cohort_minimal"
in_law_lab <- "in_laws"
kappa_sand <- 15
kappa_gsand <- 15
tau <- 5

print(paste("Loading g-sandwich estimates (size and duration):"))
print(paste("Parent is within", tau, "years of death."))
print(paste("Has a child younger than", kappa_sand))
print(paste("Has a grandchild younger than", kappa_gsand))

# Define paths to locate data
sand_path <- paste0(sex_keep, "_", cohorts_labs, "_", in_law_lab, "_", kappa_sand,"_",tau)
gsand_path <- paste0(sex_keep, "_", cohorts_labs, "_", in_law_lab, "_", kappa_gsand,"_",tau)
# dur_path <- pat <- paste0(sex_keep, "_", cohorts_labs, "_", in_law_lab, "_k_s_", kappa_sand, "_k_gs_", kappa_gsand, "_", tau)
dur_path <- pat <- paste0(sex_keep, "_", "cohort_minimal", "_", in_law_lab, "_k_s_", kappa_sand, "_k_gs_", kappa_gsand, "_", tau)
path_stable <- paste0("female", "_", "cohort_all", "_", "blood", "_", kappa_sand, "_", tau)

# Load data

sand_socsim_aggregated_simulations <- 
  data.frame(data.table::fread(
    paste0("../../Data/estimates/socsim_sandwich_",sand_path,"_aggregate.csv")
    , stringsAsFactors = F))

grandsand_socsim_aggregated_simulations <- 
  data.frame(data.table::fread(
    paste0("../../Data/estimates/socsim_grandsandwich_",gsand_path,"_aggregate.csv")
    , stringsAsFactors = F))

# Model 

sand_model_stable <- 
  read.csv(
    paste0("../../Data/estimates/model_sandwich_proportion_stable_",path_stable,".csv")
           , stringsAsFactors = F
           ) %>% 
  fix_socsim_countries()

# unique(sand_model_stable$country)
# unique(sand_model_stable2$country)

# Sandwich duration ===============

duration <- read.csv(
  paste0("../../Data/estimates/duration_",dur_path,".csv")
  , stringsAsFactors = F
)



# Dependency rates ==================
# Read estimates of comparing definitions of dependency ratios:

# 1. Dependency ratio. Numerator:people younger than 15 and older that 64; denominator: between 15 and 64 years old. 
# 2. Young dependency ratio. Numerator: younger than 15 years old. 
# 3. Old dependency ratio. Numerator older than 64 years.  
# 4. Prospective old-age dependency ratio. Numerator: people expected to die within a certain number of years.

# This is created in B_extract_demographic_rates
dependency <- read.csv("../../Data/estimates/dependency_rates.csv", stringsAsFactors = F)

# Pop expected to die within tau years
doomed_pop <- read.csv(paste0("../../Data/estimates/share_doomed_",tau,".csv"), stringsAsFactors = F)

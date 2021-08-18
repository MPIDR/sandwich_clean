
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

# The function below applies cohort life tables to real-life populations
# with the intention of gettin the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data

# lx_df is a df with the number of women surviving up to age 100 for specific
# birth cohort-country combinations
# Put differently, it is the number of woman at risk of losing a child
# ie the denominator for the absolute measure of child loss

lx_df_full <- data.table::fread("../../Data/estimates/lx_df_full.csv", stringsAsFactors = F) %>% 
  data.frame

# Estimate surviving women by applying life table
# to population of girls born in each cohort
# THis is different from the lx_df estimated for child
# death, because it includes cohorts up to 2099
# even if not all ages are complete
# I need this here since population values for cohorts 
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
  
# 3. Model sandwich estimates

sand_model <- read.csv("../../Data/estimates/model_sandwich_proportion.csv", stringsAsFactors = F)

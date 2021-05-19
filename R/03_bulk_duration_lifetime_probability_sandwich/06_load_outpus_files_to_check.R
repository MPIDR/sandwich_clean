library(countrycode)
library(tidyverse)

# menu_choice <<- "medium_affinal"


# Find which loops gave an error

# Given a foo object 

status <- unlist(lapply(foo, '[[', 1))
failed <- grep("Error", status)

fail <- SIMS[failed]

# [1] "../../../socsim_all/Africa_Medium.sup/224712/SimResults/sims.Rsave"  
# [2] "../../../socsim_all/Cameroon_Medium.sup/580145/SimResults/sims.Rsave"

# CHECK WHY!!

# B> Check which countries are not included -----------------

out_temp_specific <- "../../Data/temp/both_cohort_every_5_blood_kin_k_s_15_k_gs_15_5"

# 1. COuntries in INPUT ==============

# Check which countries have microsimulation opops at all!

input <- 
  list.files(paste0(Cave), pattern = ".sup$") %>% 
  gsub("_Medium.sup", "", .) %>% 
  recode(China_Macao_SAR = "Macao"
         , Eswatini = "Swaziland"
         , Lao_People_s_Democratic_Republic= "Laos"
         , R_union = "Reunion"
         , Republic_of_Korea = "Korea"
         , Saint_Lucia = "Saint Lucia"
         , Channel_Islands= "Channel Islands"
         , Cura_ao = "Curacao"
         # TANZANIAAAAAA
         # , Tanzania = "United Republic of Tanzania"
  ) %>% 
  gsub("_", " ", .) %>% 
  unique() %>% 
  countrycode(origin = "country.name", destination = "iso3c", warn = T) %>% 
  na.omit() %>% 
  sort() 

# dput(input)

# 2. Countries in output ======

# This comes from scripts in 03_bulk_duration_sandwich

path_out <- "../../Data/temp/both_cohort_every_5_in_laws_k_s_15_k_gs_15_5"

output <- 
  list.files(path = out_temp_specific) %>% 
  gsub("_Medium[0-9]+crunch.Rsave", "", .) %>% 
  recode(China_Macao_SAR = "Macao"
         , Eswatini = "Swaziland"
         , Lao_People_s_Democratic_Republic= "Laos"
         , R_union = "Reunion"
         , Republic_of_Korea = "Korea"
         , Saint_Lucia = "Saint Lucia"
         , Channel_Islands= "Channel Islands"
         , Cura_ao = "Curacao"
  ) %>% 
  gsub("_", " ", .) %>% 
  unique() %>% 
  sort() %>% 
  countrycode(origin = "country.name", destination = "iso3c", warn = T) %>% 
  na.omit() %>% 
  sort() 

# dput(output)
 
# 3. Processed ===========

# Df with estimates of sandwich duration

duration <- 
  # read.csv(paste0("../../Data/estimates/duration_",pat,".csv"), stringsAsFactors = F) %>%
  read.csv(paste0("../../Data/estimates/duration_",pat,".csv"), stringsAsFactors = F) %>% 
  # Data/estimates/socsim_sandwich_both_cohort_every_5_in_laws_18_5_full.csv
  pull(country) %>% 
  unique() %>% 
  sort()

# duration <- 
#   sandwich_duration_months %>% 
#   pull(country) %>% 
#   unique() %>% 
#   sort()



# 4. From results object ===========

res_names <-
  unlist(lapply(results, '[[', 'sim.name')) %>% 
  unname() %>% 
  gsub("_Medium", "", .) %>% 
    recode(China_Macao_SAR = "Macao"
           , Eswatini = "Swaziland"
           , Lao_People_s_Democratic_Republic= "Laos"
           , R_union = "Reunion"
           , Republic_of_Korea = "Korea"
           , Saint_Lucia = "Saint Lucia"
           , Channel_Islands= "Channel Islands"
           , Curacao = "Cura_ao"
    ) %>% 
    gsub("_", " ", .) %>% 
    unique() %>% 
    countrycode(origin = "country.name", destination = "iso3c", warn = T) %>% 
    na.omit() %>% 
    sort() 

# B. Find missing =============

# WHich INPUT are not in OUTPUT?

input[!input %in% output]
# Tanzania and Northern Macenoia MKD

# Which PROCESSED are not in input?
# These are the ones that were lost in the translation and could easily retrieved!

input[!input %in% duration]
# Afghanistan, Tanzania and Northern Macenoia MKD

# From RESULTS obect

input[!input %in% res_names]

# 
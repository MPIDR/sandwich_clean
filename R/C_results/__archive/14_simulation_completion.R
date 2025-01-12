
# TODO 20201030
# Rerun SOCSIM for Afghanistan and Tanzania
# These are the scripts to rerun:


# Check that all countries are included in the simulation outputs
# or in the formatted version of the simulated outputes (i.e. 
# the datasets that have been analysed to represtena specific
# outcome variable such as size of the sandwiche generation or 
# duration of the sandwich state)

# To recap, these are the files used for all the analyses:
c("AFG", "TZA")
# There is omsething weird geoing on ith these only have ppl born between
# 1751 1901, when the correct range is 1751 2201
# SOLUTION: re-run those simulatinos
# HOWEVER, it seems that there is a problem with the (perl?) script that
# creates the sup files, as the initial segment of length 2400 is not there!

# 0. Recap of data sources ====================

# These is the complete list of countries:

iso <-
  un_regions$level1_iso3 %>% 
  na.omit() %>%
  sort()

# This is equivalent to getting it from an empirical dataset fro UNWPP
# iso <- 
#   wpp_period_med %>% 
#   pull(region) %>% 
#   unique() %>% 
#   countrycode(origin = "country.name", destination = "iso3c", warn = F) %>% 
#   na.omit() %>% 
#   sort()

# Expandable countries

expandable_countries <- c("MYT", "SHN", "AIA", "BES", "BLM", "CYM", "DMA", 
                          "FLK", "KNA", "MAF", "MSR", "SXM", "TCA", "VGB", "ASM", "COK", 
                          "FSM", "MHL", "MNP", "NIU", "NRU", "PLW", "TKL", "TUV", "WLF", 
                          "AND", "BMU", "CYM", "FRO", "GIB", "GRL", "IMN", "LIE", "MCO", 
                          "MKD", "SMR", "SPM", "VAT")

# 0.1. duration # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "../../Data/estimates/duration_both_cohort_every_5_in_laws_k_s_15_k_gs_15_5.csv"

# The scripts to produce this code are stored in:
# 01_bulk_duration_sandwich
# in particular the script "04_parallel_estimates.R"

# These are the raw Rsvaes files generated by that script and saved only in the U drive
# because they are generated in Hydra (recreate via the script:
# R\03_bulk_duration_sandwich\06_load_outpus_files_to_check.R): 

dur_hydra <- c("ABW", "AGO", "ALB", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", 
               "AZE", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", 
               "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", 
               "CAF", "CAN", "CHE", "CHL", "CHN", "CHN", "CIV", "CMR", "COD", 
               "COG", "COL", "COM", "CPV", "CRI", "CUB", "CYP", "CZE", "DEU", 
               "DJI", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", 
               "EST", "ETH", "FIN", "FJI", "FRA", "GAB", "GBR", "GEO", "GHA", 
               "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", "GTM", "GUF", 
               "GUM", "GUY", "HKG", "HND", "HRV", "HTI", "HUN", "IDN", "IND", 
               "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", "JPN", 
               "KAZ", "KEN", "KGZ", "KHM", "KIR", "KOR", "KWT", "LAO", "LBN", 
               "LBR", "LBY", "LCA", "LKA", "LSO", "LTU", "LUX", "LVA", "MAC", 
               "MAR", "MDA", "MDG", "MDV", "MEX", "MLI", "MLT", "MMR", "MNE", 
               "MNG", "MOZ", "MRT", "MTQ", "MUS", "MWI", "MYS", "NAM", "NCL", 
               "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "NZL", "OMN", "PAK", 
               "PAN", "PER", "PHL", "PNG", "POL", "PRI", "PRK", "PRT", "PRY", 
               "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", "SAU", "SDN", 
               "SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "SRB", "SSD", "STP", 
               "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCD", "TGO", 
               "THA", "TJK", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TWN", 
               "UGA", "UKR", "URY", "USA", "UZB", "VCT", "VEN", "VIR", "VNM", 
               "VUT", "WSM", "YEM", "ZAF", "ZMB", "ZWE")

iso[!iso %in% dur_hydra]

# 2. sand_socsim_aggregated_simulations
# 3. grandsand_socsim_aggregated_simulations

# I only condier 1-2 as the same countries are present in 2-3.

# A. Missing countries ------------

# 1. In socims outputs ===================

# Which countries are missing in SOCSIM altoether?
# Or, for which countries do we NOT have simulation outputs?

# The scripts to produce this code are stored in:
# 01_bulk_duration_sandwich
# in particular the script "04_parallel_estimates.R"

# These are the raw Rsvaes files generated by that script and saved only in the U drive
# because they are generated in Hydra (recreate via the script:
# 06_load_outpus_files_to_check.R): 

socsim_countries <- c("ABW", "AGO", "ALB", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", 
                      "AZE", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", 
                      "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", 
                      "CAF", "CAN", "CHE", "CHL", "CHN", "CHN", "CIV", "CMR", "COD", 
                      "COG", "COL", "COM", "CPV", "CRI", "CUB", "CUW", "CYP", "CZE", 
                      "DEU", "DJI", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", 
                      "ESP", "EST", "ETH", "FIN", "FJI", "FRA", "GAB", "GBR", "GEO", 
                      "GHA", "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", "GTM", 
                      "GUF", "GUM", "GUY", "HKG", "HND", "HRV", "HTI", "HUN", "IDN", 
                      "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", 
                      "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KOR", "KWT", "LAO", 
                      "LBN", "LBR", "LBY", "LCA", "LKA", "LSO", "LTU", "LUX", "LVA", 
                      "MAC", "MAR", "MDA", "MDG", "MDV", "MEX", "MLI", "MLT", "MMR", 
                      "MNE", "MNG", "MOZ", "MRT", "MTQ", "MUS", "MWI", "MYS", "NAM", 
                      "NCL", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "NZL", "OMN", 
                      "PAK", "PAN", "PER", "PHL", "PNG", "POL", "PRI", "PRK", "PRT", 
                      "PRY", "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", "SAU", 
                      "SDN", "SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "SRB", "SSD", 
                      "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCD", 
                      "TGO", "THA", "TJK", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", 
                      "TWN", "UGA", "UKR", "URY", "USA", "UZB", "VCT", "VEN", "VIR", 
                      "VNM", "VUT", "WSM", "YEM", "ZAF", "ZMB", "ZWE")

miss_socsim <- 
  data.frame(country = iso[!iso %in% socsim_countries], stringsAsFactors = F) %>% 
  mutate(full = countrycode(country, origin = "iso3c", destination = "country.name")) %>% 
  mutate(expendable = ifelse(country %in% expandable_countries, T, F)) %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  arrange(expendable, region, country)

miss_socsim %>% 
  filter(!expendable)

# We can ignore some countries, but ones we cannot drop are:
# Tanzania, Afghanistan
print("why are Tanzania, Afghanistan not is SOCSIM output??")

# 2. In duration dfs ==================

# STATUS:
# 20200810 sorted
# Only Curacao, Afghanistan and Tanzania are missing

miss_dur <- 
  data.frame(country = iso[!iso %in% duration$country], stringsAsFactors = F) %>% 
  mutate(full = countrycode(country, origin = "iso3c", destination = "country.name")) %>% 
  mutate(expendable = ifelse(country %in% expandable_countries, T, F)) %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  arrange(expendable, region, country)

kable(miss_dur)

writeLines(kable(miss_dur), "../../Output/missing_duration.txt")

# Basically, there are countries which were present in the initial input of the function
# that estiamted duration but are no longer presnt in the output of that function. Where did they
# disappear?

# 

# 3. In sandwich size dfs ==================

# STATUS 20200810
# FIXED
# There was an issue when I recoded the country names before running the large
# bunk estimates. I re-estiamted SWZ by hand (for sand and gsand separately). 

miss_size <-   
  data.frame(country = iso[!iso %in% sand_socsim_aggregated_simulations$country], stringsAsFactors = F) %>% 
  mutate(full = countrycode(country, origin = "iso3c", destination = "country.name")) %>% 
  mutate(expendable = ifelse(country %in% expandable_countries, T, F)) %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  arrange(expendable,region, country)

# Basically, important countries missing:

# Swaziland
# St. Lucia 

kable(miss_size)

writeLines(kable(miss_size), "../../Output/missing_size.txt")


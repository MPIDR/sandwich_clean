
# For the US only, the output of the multiple simulation estimates
# can be loaded:
# socsim_sandwich <- readRDS("../../Data/estimates/socsim_sandwich_prop.RDS")

# 20200325
# Proportion women sandwiched over age.
# Considering if a women has a a child (boy or girl) aged 5 or less
# while having a parent within 5 years of death.

# Numerator ~~~~~~~~
# number of women of age “a” who are alive in year “t” 
# AND gave birth within the last 5 years 
# AND have a living mother who will die within 5 years.

# Denominator ~~~~~~~
# number of women of age “a” who are alive in year “t”


# Improvements Needed: 
# - Ignore mortality of children

# 1. Sandwich probability from SOCSIM ----

# Chose which countries to keep -----

# Get clean country names
# Get a df that identifies the country codes from the opop and omar filenames
# The idea is to run the code in chunks for each country, so that the results 
# are stored after each country is processed
# Doing it like this also allows me to run analysis for a selected number of countries 
# exclusively. This can come in handy if I only want to run the simulation for 3 countries, 
# for example, since it takes a couple of days to do this for all countries. 

countries_clean <-
  data.frame(opop = sim_names_full, original = gsub("_[0-9]+", "", sim_names), stringsAsFactors = F) %>% 
  mutate(
    omar = gsub("opop", "omar", opop)
    , original = gsub(".csv", "", original)
    , original = recode(original
                        , China_Macao_SAR = "Macao"
                        , Eswatini = "Swaziland"
                        , Lao_People_s_Democratic_Republic= "Laos"
                        , R_union = "Reunion"
                        , Republic_of_Korea = "Korea"
                        , Saint_Lucia = "Saint Lucia"
                        , Channel_Islands= "Channel Islands"
                        , Cura_ao = "Curacao"
                   )
    , country_name = countrycode(original, origin = "country.name", destination = "country.name", warn = F)
    , country = countrycode(country_name, origin = "country.name", destination = "iso3c", warn = F)
  ) %>%
  filter(!original %in% "China_and_dependencies") %>% 
  # Ged rid of regions
  # NOTE THAT THIS FUNCTION ONLY KEEPS COUNTRY BY DEFINITION!
  find_regions_code(ignore_regions = F, pretty_names = T) 

  # select(country, country_name, region, opop, omar)

# Pick here which countries you want to run the analysis for.


if(countries_to_analyse == "by_region") {
  print(paste("Looking only at", region_to_keep))
  
  countries_clean <- 
    countries_clean %>% 
    filter(region %in% region_to_keep) %>% 
    filter(!country %in% countries_ignore)
} else if(countries_to_analyse == "all") {
  print("Analysing all countries, good luck!")
}
  
  
# Decide whether countries for which I have already estimated sandwicheness
  # should be estimated again...
if(keep_only_new){
  print("Since keep_only_new, skipping countries with existing esimtaes...")
  
  countries_clean <-
    countries_clean %>% 
    keep_only_new_simulations(pattern = pat)
}
  
# temp
# countries_clean <- countries_clean %>% filter(country %in% c("AFG", "TZA"))
# countries_clean <- countries_clean %>% filter(country %in% c("SWZ"))

paths_opop_l <- split(countries_clean$opop, countries_clean$country)
paths_omar_l <- split(countries_clean$omar, countries_clean$country)

# We want countries of the same region to run on after the other, so we sort the lists by region


# sim_range is the index of the simulations that will be processed when calling this function
# I split this instead of running all simulations at once to avoid losing information if
# the HPC were to break down at some point, for example
# Note that this is the number of countries, not simulations (there are multiple simulations per country)
# sims_per_run <- 5

# This is all just to get the different sim_range values for the loop
# as a list
# sim_range_all <- length(list.files(pattern = "^socsim_opop_", path = Cave))
# br_range <- c(seq(1, sim_range_all, sims_per_run), Inf)
# split_br <- cut(1:sim_range_all, br_range, include.lowest = T)
# sim_range_l <- split(1:sim_range_all, split_br)

for(n in seq_along(paths_omar_l)){
  socsim_sandwich_estimates_top(
    kappa = kappa
    , tau = tau
    , type = type
    , sim_range
    , paths_opop = paths_opop_l[[n]]
    , paths_omar = paths_omar_l[[n]]
    , Cave = Cave
    , path_out = "Data/estimates/"
    , ages_socsim_in_months
    , in_laws = in_laws
    , sex_keep = "both"
    , pat = pat
  )  
}

print("SOCSIM estimates created!")

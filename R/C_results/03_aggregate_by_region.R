
# Given large df with country-specific estiamtes, get regional means
# Get mean and confidence intervals

# Question: 
# - The aggregated df already had the mean of multiplte simulations. Is it correct to just keep getting the mean
# of the mean of the mean?

# 1. Sandwich ----------------------------

# 1.1. Rate by age ==============================

# This df already shows the mean of the mean and has a confidence intervals from multiple simulations
# for each country. I now want to get the mean of the mean of the mean. 

sand_reg <- 
  sand_socsim_aggregated_simulations %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>%
  group_by(region, cohort, age) %>% 
  dplyr::summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup() 

# Model In a stable population

sand_stable_reg <- 
  sand_model_stable %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>%
  group_by(region, cohort, age) %>% 
  dplyr::summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup() 
  

# 2. Grandsandwich ---------------------------------------

# 2.1. Rate by age ==============================

grandsand_reg <- 
  grandsand_socsim_aggregated_simulations %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>%
  group_by(region, cohort, age) %>% 
  dplyr::summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup() 

# 3. Duration of sandwich --------------------

dur <- 
  duration %>% 
  filter(!is.na(country)) %>% 
  select(-original) %>% 
  pivot_longer(sandwiched:gsandwiched, names_to = "variable", values_to = "value") %>% 
  # Simulation means
  group_by(variable, country, cohort) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  rename(type = variable) %>% 
  mutate(region = as.character(region)) %>% 
  # AS YEARS
  mutate(value = value/12)
  

# Regional means

dur_reg <- 
  dur %>% 
  group_by(type, region, cohort) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup()

# 4. Lifetime probability ------------------

# 20201207 Duration estiamted from lifelines
# After Alyson's comments


lifep <- 
  duration %>% 
  filter(!is.na(country)) %>% 
  select(-original) %>% 
  select(-sandwiched, - gsandwiched) %>% 
  rename(sandwiched = share_sand, gsandwiched = share_gsand) %>% 
  pivot_longer(sandwiched:gsandwiched, names_to = "variable", values_to = "value") %>% 
  # Simulation means
  group_by(variable, country, cohort) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  rename(type = variable) %>% 
  mutate(region = as.character(region)) 


# Regional means

lifep_reg <- 
  lifep %>% 
  group_by(type, region, cohort) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup()

# 5. Dependency rates ------------------

dep <- 
  dependency %>% 
  rename(value = mean) %>% 
  find_regions_code(ignore_regions = F, pretty_names = T)

dep_reg <- 
  dep %>% 
  group_by(variable, region, year) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>%
  ungroup()

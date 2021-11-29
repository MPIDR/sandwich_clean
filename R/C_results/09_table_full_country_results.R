
# Create a big table with all the results at the country level 
# NOTE: not included in published verison of paper

cohort_show <- c(1970, 2005, 2040)

cohorts_in_common <- 1970:2040

regions <- c("N Africa & W Asia", "AUS & NZ", "Europe & N America", "Sub-Sah Africa", 
             "C & S Asia", "LATAM & Caribbean", "E & SE Asia", "Oceania (other)"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------
# 1. Sandwichness rate ================

type_levels <- c("sandwich", "grandsandwich")
types <- c("sand", "gsand")
type_names <- setNames(type_levels, types)

# 1.1. Get objects needed ==================

lt_df <- LTCF_socsim

wx <- 
  lt_df %>% 
  select(country, cohort, age, nLx, Tx) %>% 
  group_by(country, cohort) %>% 
  arrange(country, cohort, age) %>% 
  mutate(wx = nLx / first(Tx)) %>% 
  ungroup %>% 
  select(country, cohort, age, wx) 

# 2. Estimate size of sandwich generation 

# Weight S(a)
# Note that we need all ages and not just over 15

sa_wx <- 
  full_join(
    sand_socsim_aggregated_simulations %>% 
      select(country, cohort, age, sx = value)
    , wx
    , by = c("country", "cohort", "age")
  ) %>% 
  filter(cohort %in% cohorts_in_common) %>% 
  arrange(country, cohort, age) %>% 
  # Fill in missing values
  mutate(
    sx = ifelse(is.na(sx), 0, sx)
    , wx = ifelse(is.na(wx), 0, wx)
  )

# Size of sandwich generation

size <- 
  sa_wx %>% 
  filter(cohort %in% seq(1970, 2040, 5)) %>%
  group_by(country, cohort) %>% 
  summarise(
    value = sum( sx * wx )
  ) %>% 
  ungroup() %>% 
  find_regions_code(ignore_regions = F, pretty_names = T)

# # grandsand

gsa_wx <- 
  full_join(
    grandsand_socsim_aggregated_simulations %>% 
      select(country, cohort, age, sx = value)
    , wx
    , by = c("country", "cohort", "age")
  ) %>% 
  filter(cohort %in% cohorts_in_common) %>% 
  arrange(country, cohort, age) %>% 
  # Fill in missing values
  mutate(
    sx = ifelse(is.na(sx), 0, sx)
    , wx = ifelse(is.na(wx), 0, wx)
  )

gsize <- 
  gsa_wx %>% 
  filter(cohort %in% seq(1970, 2040, 5)) %>%
  group_by(country, cohort) %>% 
  summarise(
    value = sum( sx * wx )
  ) %>% 
  ungroup() %>% 
  find_regions_code(ignore_regions = F, pretty_names = T)

size_both <- bind_rows(
  size %>% mutate(variable = "sand")
  , gsize %>% mutate(variable = "gsand")
) %>% 
  mutate(variable = factor(variable, levels = types))

# 2. lifetime prob (Size) of sandwich generation ================

lifep_tab <- 
  lifep %>% 
  mutate(
    variable = recode(type, "sandwiched"= "sand" , "gsandwiched" = "gsand")
  ) %>% 
  select(region, country, cohort, value, variable)

# 3. Duration of Squeeze ----------------

measure_levels <- c("total duration (years)", "share of lifespan (%)")

type_levels <- c("sandwich", "grandsandwich")
types_long <- c("sandwiched", "gsandwiched")

# Life expectancy by country

ex_country <- 
  LTCF_socsim %>% 
  filter(age == 0) %>% 
  select(country, cohort, ex) 

dur_share <-
  dur %>% 
  left_join(ex_country, by = c("country", "cohort")) %>% 
  mutate(share = (value / ex)*100) %>% 
  select(region, country, type, cohort, duration = value, share) %>% 
  pivot_longer(c(duration:share), names_to = "measure", values_to = "value") %>% 
  mutate(
    measure = factor(measure, labels = measure_levels)
    , type = factor(type, levels = types_long, labels = type_levels)
    , region = factor(region, levels = regions_short)
  )

# 3. All together ---------------------- 

# PArameters for table

old <- c("sand size", "sand total duration (years)", "sand share of lifespan (%)"
         , "gsand size", "gsand total duration (years)", "gsand share of lifespan (%)"
)

new <- paste0(1:6, "_",old)

regions_tab <- c("World","Sub-Sah Africa", "N Africa & W Asia", "C & S Asia", "E & SE Asia",
                 "LATAM & Caribbean", "AUS & NZ", "Oceania (other)", "Europe & N America"
)


tab <-
  dur_share %>%
  mutate(
    type = recode(type, grandsandwich = "gsand", sandwich = "sand")
    # , value = ifelse(measure == "share of lifespan (%)", value*100, value)
  ) %>%
  mutate_if(is.factor, as.character) %>%
  # Sandwichness rate
  bind_rows(
    size_both %>%
      mutate(
        value = value * 100
        , measure = "rate"
      ) %>%
      mutate_if(is.factor, as.character) %>%
      select(region, country, type = variable, cohort, measure, value)
  ) %>%
  # Lifetime probability
  # 20201207
  bind_rows(
    lifep_tab %>%
      mutate(
        value = value * 100
        , measure = "size"
      ) %>%
      mutate_if(is.factor, as.character) %>%
      select(region, country, type = variable, cohort, measure, value)
  )


# 3.2. With two types of duration ===============

old2 <- c(
  "sand size", "sand rate"
  , "sand total duration (years)", "sand share of lifespan (%)"
  , "gsand size", "gsand rate"
  , "gsand total duration (years)", "gsand share of lifespan (%)"
)

new2 <- paste0(1:8, "_",old2)

# For comparisson after retraction

out_temp <- 
  tab %>% 
  # Remove rate
  filter(cohort %in% cohort_show) %>% 
  mutate_if(is.numeric, round, 1) %>% 
  arrange(country, cohort) %>% 
  mutate(
    col = paste(type, measure)
    , col = plyr::mapvalues(col, from = old2, to = new2)
    , row = paste(country, cohort)
  ) %>% 
  select(country, cohort, col, row, value) %>% 
  pivot_wider(names_from = col, values_from = value) %>% 
  select(-row) %>% 
  data.frame() 

out_temp <- out_temp[, order(colnames(out_temp))]

write.csv(out_temp, "../../Output/tablex_full_country_results.csv", row.names = F)

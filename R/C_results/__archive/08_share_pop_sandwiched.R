
# TO DO
# - Estimate nLX and Tx from SOCSIM output files (ie do not take them from UN WPP). The issue
#   with UNWPP is that it only goes to cohort 2000

# 20200610
# This script determines the 'size of the sandwich generation', or the
# share of the population sandwiched. 
# From a period perspective this is the share of a population that is sandwiched in any
# given year. This is easy to estimate. 
# Thinking from a cohort perspective, this must be the share of people in a birth cohort (in any given country)
# who will ever be in a sandwiched position throughout their lifes. 
# There are two ways of determining the 'size of the sandwich generation' (from a cohort perspective): 
# - A. Using life tables
# - B. Counting method

# 0. Shared parameters -------------------

# Cohorts that are both present in the cohort life tables and sandwich estimates
cohorts_in_common <- 1970:2040

# ~~~~~~~~~~~~~~~~~ -------------------------
# PERIOD PERSPECTIVE -------------------
# ~~~~~~~~~~~~~~~~~ -------------------------

# A. Using life tables ~~~~~~~~~~~~~~~~~ -------------------------

# pending...

# ~~~~~~~~~~~~~~~~~ -------------------------
# COHORT PERSPECTIVE -------------------
# ~~~~~~~~~~~~~~~~~ -------------------------
# A. Using life tables ~~~~~~~~~~~~~~~~~ -------------------------

# NOTE that this works with age-specific sandwichness rates derived from the formal model OR estimated from
# SOCSIM outputs. For SOCSIM, you ideally want to compare thi to estimates obtained from Method B (below) to
# double check accuracy of estimates.
# 
# Given S(a) a set of age-specific sandwichness rates (or probabilities of being sandwiched at any given age
# if working with formal models), transform this to population-level prevalence of sandwichness in a similar
# way to how you would go from age-specific mortality to crude death rates: by multiplying each age-specific value
# by a standardized value representing the share of the population containted in the interval a to a+1.
# In a period perspective, this is simply the share of the total population represented by the age-specific population
# aged a. In a cohort perspective, the formulate for the Crude Sandwichness Rate is:
# 
# CSR = sum(s_a * w(a))
# 
# over ages a = [0;100] where w(a) are the person-years lived between ages a and a+1 by women born in cohort c
# (life table column 1_L_a) as a share of the sum of all person years lived by members of cohort c (life table column
# T_a where a = 0):
# 
# w(a) = 1_L_a/T_0

# 1. Get life table columns =========
# We can use the Cohort life table columns estimated in a previous script (child death)
# This already has all the columns we need, so we only need to transform it:

# One alternative is to get them from UNWPP, but this only works for cohorts 1950-2000

# lt_df <- 
#   LTCF_all %>% 
#   rename(country = Country, cohort = Cohort, age = Age) %>% 
  # mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F))

# Prefered to SOCIMS estimates - use SOCSIM life tables

lt_df <- LTCF_socsim

wx <- 
  lt_df %>% 
  select(country, cohort, age, nLx, Tx) %>% 
  group_by(country, cohort) %>% 
  arrange(country, cohort, age) %>% 
  mutate(wx = nLx / first(Tx)) %>% 
  ungroup %>% 
  select(country, cohort, age, wx) 

# Double check that all wx add up to 1

# wx %>% 
#   group_by(country, cohort) %>% 
#   summarise(s = sum(wx)) %>%
#   ungroup() %>%
#   pull(s) %>% 
#   unique()

# warning("C_te_d_Ivoire and Czechia not producing results, maybe an issue with the simulation files?")
# FIX in the script that creates life tables from SOCSIM
# lt_df %>% 
#   filter(country %in% c("CZE", "CIV")) %>% View

# 2. Estimate size of sandwich generation =======================

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

# Aggregate by region 

size_reg <- 
  size %>% 
  group_by(region, cohort) %>% 
  dplyr::summarise(
    lower = confidence_interval(value, type = "lower", ci = ci)
    , upper = confidence_interval(value, type = "upper", ci = ci)
    , value = mean(value, na.rm = T)
  ) %>%
  ungroup() 

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

# Aggregate by region 

gsize_reg <- 
  gsize %>% 
  group_by(region, cohort) %>% 
  dplyr::summarise(
    lower = confidence_interval(value, type = "lower", ci = ci)
    , upper = confidence_interval(value, type = "upper", ci = ci)
    , value = mean(value, na.rm = T)
  ) %>%
  ungroup() 

# 3. Line plot =============

# 3.1. Plot countries ====================

size %>% 
  ggplot(aes(x = cohort, y = value)) +
  # Country lines
  geom_line(aes(group = country), size = 0.4, alpha = 0.1) +
  # Region lines
  geom_smooth(
    aes(group = region), method = lm, formula = y ~ splines::bs(x, 4)
    , se = F, size = 2, colour = "black", data = size_reg
    ) +
  # Plot uncertainty as bands
  scale_x_continuous("Cohort") +
  scale_y_continuous("Share of birth cohort ever sandwiched (%)", labels = function(br) br * 100) +
  # scale_color_discrete("") +
  facet_wrap(~region) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(paste0("../../Output/size_sandwich.pdf"), height = 9, width = 12, units = "cm")

# 3.1.1 Sand + gsand ==================

type_levels <- c("sandwich", "grandsandwich")
types <- c("sand", "gsand")
type_names <- setNames(type_levels, types)

size_both <- bind_rows(
  size %>% mutate(variable = "sand")
  , gsize %>% mutate(variable = "gsand")
) %>% 
  mutate(variable = factor(variable, levels = types))

size_reg_both <- bind_rows(
  size_reg %>% mutate(variable = "sand")
  , gsize_reg %>% mutate(variable = "gsand")
) %>% 
  mutate(variable = factor(variable, levels = types))

# Option1  facets are regions

  # ggplot() +
  # # Country lines
  # geom_line(
  #   aes(x = cohort, y = value, colour = variable, group = country), size = 0.4, alpha = 0.1
  #   , data = size_both %>% filter(variable == "sand")
  #   ) +
  #   geom_line(
  #     aes(x = cohort, y = value, colour = variable, group = country), size = 0.4, alpha = 0.1
  #     , data = size_both %>% filter(variable == "gsand")
  #   ) +
  # # Region lines
  # geom_smooth(
  #   aes(x = cohort, y = value, group = region, colour = variable), method = lm, formula = y ~ splines::bs(x, 4)
  #   , se = F, size = 2, data = size_reg_both %>% filter(variable == "sand")
  # ) +
  #   geom_smooth(
  #     aes(x = cohort, y = value, group = region, colour = variable), method = lm, formula = y ~ splines::bs(x, 4)
  #     , se = F, size = 2, data = size_reg_both %>% filter(variable == "gsand")
  #   ) +
  # # Plot uncertainty as bands
  # scale_x_continuous("Cohort") +
  # scale_y_continuous("Share of birth cohort ever squeezed (%)", labels = function(br) br * 100) +
  # # scale_color_discrete("") +
  # facet_wrap(~region) +
  # theme_bw() +
  # theme(legend.position = "bottom")
  # 
  # ggsave(paste0("../../Output/size_sandwich_gsandwich.pdf"))

  # Version 2: facets are sandwihc type
  
  ggplot() +
    # Country lines
    geom_line(
      aes(x = cohort, y = value, colour = region, group = country), size = 0.4, alpha = 0.1
      , data = size_both %>% filter(variable == "sand")
    ) +
    geom_line(
      aes(x = cohort, y = value, colour = region, group = country), size = 0.4, alpha = 0.1
      , data = size_both %>% filter(variable == "gsand")
    ) +
    # Region lines sandwich
    geom_smooth(
      aes(x = cohort, y = value, group = region, colour = region), method = lm, formula = y ~ splines::bs(x, 4)
      , se = F, size = 1, data = size_reg_both %>% filter(variable == "sand")
    ) +
    # Region lines grandsand
    geom_smooth(
      aes(x = cohort, y = value, group = region, colour = region), method = lm, formula = y ~ splines::bs(x, 4)
      , se = F, size = 1, data = size_reg_both %>% filter(variable == "gsand")
    ) +
    # World average line
    geom_smooth(
      aes(x = cohort, y = value)
      , method = lm, formula = y ~ splines::bs(x, 4)
      , se = F, size = 1.5, colour = "black"
      , data = size_both
    ) +
    scale_x_continuous("Cohort") +
    scale_y_continuous("Share of birth cohort ever sandwiched (%)", labels = function(br) br * 100) +
    scale_color_discrete("") +
    facet_wrap( ~ variable, labeller = labeller(variable = type_names)) +
    theme_bw() +
    theme(
      legend.position = "bottom"
      # Legend labels
      , legend.text=element_text(size=(base_size))
      # Legend title
      , legend.title = element_text(size = (base_size-2))
      # , axis.text.x = element_text(angle = 45)
      , panel.spacing = unit(1.2, "lines")
      # Move y axis closer to the plot
      , plot.margin = unit(c(t=0.2, r=0, b=0.1, l=0), unit="cm")
      # get rid of facet boxes
      , strip.background = element_blank()
      # , strip.text = element_text(hjust = 0)
    )

  ggsave(paste0("../../Output/size_sandwich_gsandwich", "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 10, width = 12, units = "cm")
    
# 3.2. Plot regions ====================


size %>% 
  ggplot(aes(x = cohort, y = value, colour = region)) +
  # Country lines
  geom_line(aes(group = country), size = 0.4, alpha = 0.1) +
  # Region lines
  geom_smooth(
    aes(group = region, colour = region), method = lm, formula = y ~ splines::bs(x, 4)
    , se = F, size = 2, data = size_reg
  ) +
  # Plot uncertainty as bands
  scale_x_continuous("Cohort") +
  scale_y_continuous("Share of birth cohort ever sandwiched (%)", labels = function(br) br * 100) +
  scale_color_discrete("") +
  # facet_wrap(~region) +
  theme_bw() +
  theme(legend.position = "bottom")

# size_reg %>% 
#   ggplot(aes(x = cohort, y = value)) +
#   geom_line(aes( colour = region)) +
#   # Plot uncertainty as bands
#   geom_ribbon(
#     aes(ymin = lower, ymax = upper, fill = region)
#     , alpha = 0.4, show.legend = F
#   ) +
#   # Plot shapes to help distinguish regions
#   geom_point(
#     aes(colour = region, shape = region)
#     , data = . %>% filter(cohort %in% seq(min(cohorts_in_common), max(cohorts_in_common), 10))
#   ) +
#   scale_x_continuous("Cohort") +
#   scale_y_continuous("Share of birth cohort ever sandwiched (%)", labels = function(br) br * 100) +
#   theme_bw() +
#   theme(legend.position = "bottom")

ggsave(paste0("../../Output/size_sandwich_reg.pdf"))


# 4. MAP: Share squeezed  -----------------------------

# ISSUE! 20200803
# lsdk4
# Some countries are missing fro the duration df (loaded from csv):
# sort(unique(ex_country$country)[!unique(ex_country$country) %in% unique(size_both$country)])
# [1] "AFG" "AUS" "FJI" "FSM" "GUM" "KIR" "LCA" "MKD" "MYT" "NCL" "NZL" "PNG" "PYF" "SLB" "SWZ" "TON" "TZA" "VUT" "WSM"

cohort_map <- 1970
# cohort_map <- 2000
# cohort_map <- 2040

type_keep <- "sand"
# type_keep <- "gsand"

# Use this for stacked maps
country_line_size <- 0.0005
title_size <- 9
viridis_direction <- -1

type_lab <- switch(type_keep,
                   sand = "sandwiched"
                   , gsand = "grandsandwiched")

title_lab <- switch(type_keep,
                   sand = "Sandwich"
                   , gsand = "Grandsandwich")

bar_name <- paste0("Share of cohort\n", type_lab)


p_title <- paste0("Size of the ",title_lab," Generation (", cohort_map," cohort)")

bar_lim <- switch (type_keep,
  sand = c(1.5, 8.6)/100
  , gsand = c(0.5, 4.8)/100
)

bar_br <- switch (type_keep,
  sand = c(2,4,6,8)/100
  , gsand = c(1,2,3,4)/100
)

# 0. Create world map 
# Can be reused for all maps

world <- 
  sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country)) %>% 
  assign_contested_iso3_countries(.)
  # mutate(
  #   country = recode(country
  #                    , GRL = "DNK"
  #                    , SOL = "SOM"
  #                    , SDS = "SDN"
  #                    , SAH = "MAR"
  #                    )
  # )


# Get value :
values <-
  size_both %>% 
  rename(type = variable) %>% 
  filter(type == type_keep) %>% 
  filter(cohort %in% cohort_map) %>% 
  select(country, value)

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>%
  filter(! ID %in% "Antarctica")

# Plot

# p1 <-
ggplot(data = w) +
  geom_sf(
    aes(geometry = geometry, fill = value)
    , colour = alpha("white", 1 / 2)
    , size = country_line_size
  ) +
  scale_fill_viridis(
    name = bar_name
    # , option="cividis"
    , option="plasma"
    , breaks = bar_br
    , limits = bar_lim
    , direction = viridis_direction
    , labels = function(br) paste0(br*100, "%")
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  ggtitle(p_title) +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    , plot.title = element_text(size = title_size, face="bold")
    , legend.position = "bottom"
  ) +
  guides(
    fill = guide_colourbar(barheight = 0.5)
  )

ggsave(paste0("../../Output/map_size",  "_", type_keep, "_",cohort_map, "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 7, width = 16, units = "cm")

# ~~~~~~~~~~~~~~~~~ -------------------------
# B. Counting method ~~~~~~~~~~~~~~~~ ----------------------------------
# Counting in the SOCSIM outputs the share of the population that was ever sandwiched
# Need to implement in Hydra

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----
# C. Results ------------------------------

#  1. Plot: All country-cohort in one plot =============

size_extended <- 
  size %>% 
  left_join(CTFR_country, by = c("country", "cohort")) %>% 
  left_join(ex_country, by = c("country", "cohort"))

# Crazy plot tat plots all the duration data in one single space

# Total duration

size_extended  %>% 
  mutate(cohort = as.character(cohort)) %>% 
  mutate(value = value*100) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_point(
    aes(colour = value)
    , shape = "square"
    , size = 6, alpha = 1
  ) +
  scale_color_viridis_c("Share of birth cohort\never sandwiched",direction = -1, labels = function(br) c(br[-length(br)], paste0(br[length(br)], "%")) ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/size_tfr_ex.pdf"), width = 15, height = 12, units = "cm")

# 2. Table by region and cohort ============
# Table of size of sandwich generation (\% of pop) by region, cohort, and kappa-tau
# Mean and 95% CI

# cos_table <- seq(1970, 2040, 10)
cos_table <- c(1970, 1990, 2020, 2040)

tab_out <- 
  size_reg %>% 
  filter(cohort %in% cos_table) %>% 
  mutate_at(c("lower", "upper", "value"), function(x) round(x*100, digits = 1)) %>% 
  mutate(val = paste0(value, " (", lower, ";", upper, ")")) %>% 
  select(Region = region, Cohort = cohort, value = val) %>% 
  pivot_wider(names_from = Cohort, values_from = value)

# save as latex

kable(
  tab_out, "latex", booktabs = T
  , caption = "Expected size of the ‘sandwich generation’ - share of birth cohort that will ever find themelves sandwiched between young children and dependent elderly parents at some point of their lives"
  , label = "size"
) %>% 
  write("../../Output/duration.tex")

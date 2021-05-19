
# 0. Preamble --------------

# Cohorts that are both present in the cohort life tables and sandwich estimates
cohorts_in_common <- 1970:2040
ci <- 0.95

# Create world map 
# Can be reused for all maps

world <-
  sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>%
  select(ID = ADMIN, country = ADM0_A3, geometry) %>%
  mutate(ID = as.character(ID), country = as.character(country)) %>% 
# recode Greenland as denmakr
  assign_contested_iso3_countries(.)

# !  Map 1.2: sandwich rate -----------------------------

type_levels <- c("sandwich", "grandsandwich")
types <- c("sand", "gsand")
type_names <- setNames(type_levels, types)

# 1.1. Get objects needed ==================

# NOTE that this works with age-specific sandwichness rates derived from the formal model OR estimated from 
# SOCSIM outputs. For SOCSIM, you ideally want to compare thi to estimates obtained from Method B (below) to
# double check accuracy of estimates. 

# Given S(a) a set of age-specific sandwichness rates (or probabilities of being sandwiched at any given age
# if working with formal models), transform this to population-level prevalence of sandwichness in a similar
# way to how you would go from age-specific mortality to crude death rates: by multiplying each age-specific value
# by a standardized value representing the share of the population containted in the interval a to a+1. 
# In a period perspective, this is simply the share of the total population represented by the age-specific population
# aged a. In a cohort perspective, the formulate for the Crude Sandwichness Rate is:

# CSR = sum(s_a * w(a))

# over ages a = [0;100] where w(a) are the person-years lived between ages a and a+1 by women born in cohort c 
# (life table column 1_L_a) as a share of the sum of all person years lived by members of cohort c (life table column
# T_a where a = 0):

# w(a) = 1_L_a/T_0

# 1. Get life table columns 
# We can use the Cohort life table columns estimated in a previous script (child death)
# This already has all the columns we need, so we only need to transform it:

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

size_both <- bind_rows(
  size %>% mutate(variable = "sand")
  , gsize %>% mutate(variable = "gsand")
) %>% 
  mutate(variable = factor(variable, levels = types))


# 1.2. Shared parameters ===============

cohort_map <- 1970

country_line_size <- 0.0005
title_size <- 9
viridis_direction <- -1
bar_lim <- c(0.5, 7.6)/100
bar_br <- c(1:7)/100
bar_name <- paste0("Crude\nIndex")

# 1.3. Plot ==============

# I have to do it separately for sand and gsand to avoid some countries from disappearing

# SANDWICH

# Get value :
values_sand <-
  size_both %>%
  rename(type = variable) %>%
  filter(type == "sand") %>%
  filter(cohort %in% cohort_map) %>% 
  select(country, type, value)

# Join with map
w_sand <- left_join(
  world
  , values_sand
  , by = "country"
) %>%
  # filter(! ID %in% "Antarctica") %>% 
  mutate(type = "sand")

# GRANDSANDWICH

# Get value :
values_gsand <-
  size_both %>%
  rename(type = variable) %>%
  filter(type == "gsand") %>%
  filter(cohort %in% cohort_map) %>% 
  select(country, type, value)

# Join with map
w_gsand <- 
  left_join(
    # recode Greenland
    world 
    , values_gsand
    , by = "country"
  ) %>%
  # filter(! ID %in% "Antarctica") %>% 
  mutate(type = "gsand")

w <- 
  rbind(w_sand, w_gsand) %>% 
  mutate(
    type = recode(type,
                  sand = "(a) Parental sandwich"
                  , gsand = "(b) Grandparental sandwich"
    )
    , type = factor(type, levels = c("(a) Parental sandwich", "(b) Grandparental sandwich"))
  )

# PLOT

tit <- "FIGURE 2   Crude Sandwich and Grandsandwich Index (1970 cohort)."
cap <- "NOTE: The Crude Index is the population-weighted sum of age-specific probabilities of (grand)sandwichness. \nThe maps show mean values for men and women combined."

no_title <- w %>%
  ggplot() +
  geom_sf(
    aes(geometry = geometry, fill = value)
    , colour = alpha("white", 1 / 2)
    , size = country_line_size
  ) +
  scale_fill_viridis(
    name = bar_name
    , option="plasma"
    , breaks = bar_br
    , limits = bar_lim
    , direction = viridis_direction
    , labels = function(br) br*100
  ) +
  # labs(title = tit, caption = cap) +
  coord_sf(crs = "+proj=robin") +
  facet_wrap(~type, ncol = 1, strip.position = "top") +
  theme_minimal(base_size = 8) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    , plot.title = element_text(size = title_text_size)
    , legend.position = "right"
    , strip.text.x = element_text(size = axis_text_size)
    , text = element_text(family = text_family, size = axis_text_size)
    , plot.caption = element_text(hjust = 0)
  ) +
  guides(
    fill = guide_colourbar(barwidth = 0.5, barheight = 16)
  )


# ggsave(paste0("../../Output/map_size",  "_", "both", "_",cohort_map, "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 14, width = 16, units = "cm")
ggsave("../../Output/alburez Figure 2_no_title.pdf", no_title, height = 14, width = 16, units = "cm")

no_title + labs(title = tit, caption = cap) 

ggsave("../../Output/alburez Figure 2.pdf", height = 14, width = 16, units = "cm")
# ggsave("../../Output/alburez Figure 2.eps", height = 14, width = 16, units = "cm")
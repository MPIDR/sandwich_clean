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

sand_model <- sand_model_stable

# 0. Shared parameters -------------------

# Cohorts that are both present in the cohort life tables and sandwich estimates
cohorts_in_common <- 1950:2000
x_lim <- c(1970, 2040)

# A. Crude Sandwich Rate ~~~~~~~~~~~~~~~~~ -------------------------

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

# 1. Get life table columns =========
# We can use the Cohort life table columns estimated in a previous script (child death)
# This already has all the columns we need, so we only need to transform it:

# One alternative is to get them from UNWPP, but this only works for cohorts 1950-2000

# lt_df <- 
#   LTCF_all %>% 
#   rename(country = Country, cohort = Cohort, age = Age) %>% 
  # mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F))

# Prefered for model - use WPP LT

lt_df <- LTCF_all

wx <- 
  lt_df %>% 
  select(country = Country, cohort = Cohort, age = Age, nLx, Tx) %>% 
  group_by(country, cohort) %>% 
  arrange(country, cohort, age) %>% 
  mutate(wx = nLx / first(Tx)) %>% 
  ungroup %>% 
  select(country, cohort, age, wx) %>% 
  fix_socsim_countries()

# 2. Estimate size of sandwich generation =======================

# Weight S(a)
# Note that we need all ages and not just over 15

sa_wx <- 
  full_join(
    sand_model %>% 
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


# Crude Sandwich Rate:
  # Given a set of age-specific sandwichness rates S(a), 
  # I transformed this to population-level measure in the way '
  # in which you would go from age-specific mortality to crude 
  # death rates: by multiplying each age-specific value by the 
  # population distribution and then summing over all ages. 
  # Something like:  \sum_{a=0}^{100}{S(a)*w(a)}, where w(a) 
  # are the person-years lived between ages a and a+1 by women 
  # born in cohort c (life table column 1_L_a) as a share of the 
  # sum of all person years lived by members of cohort c 
  # (life table column T_0). 
size <- 
  sa_wx %>% 
  group_by(country, cohort) %>% 
  summarise(
    value = sum( sx * wx )
  ) %>% 
  ungroup() 
  
#  1. Plot: All country-cohort in one plot =============

# Get country-level estimates

# Life expectancy by region

ex_country <- 
  LTCF_all %>% 
  filter(Age == 0) %>% 
  select(country = Country, cohort = Cohort, ex) %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)) %>% 
  filter(!is.na(country)) 

CTFR_country <-
  ASFRC_all %>% 
  filter(between(Cohort, x_lim[1], x_lim[2])) %>% 
  group_by(country, Cohort) %>% 
  summarise(TFR = sum(ASFR)) %>% 
  ungroup() %>% 
  select(country, cohort = Cohort, TFR) %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)) %>% 
  filter(!is.na(country)) 

size_extended <- 
  size %>% 
  left_join(CTFR_country, by = c("country", "cohort")) %>% 
  left_join(ex_country, by = c("country", "cohort"))

# Total duration

tit <- "FIGURE 1   Crude Sandwich Index in 10,200 stable populations with different \nlevels of fertility and mortality."
cap <- "NOTE: Estimates produced from Eq. 1 and Eq. 3 using country-level rates from the 2019 Revision of the United \nNations World Population Prospects (historical and medium-scenario projections)."

no_title <- 
  size_extended  %>% 
  mutate(cohort = as.character(cohort)) %>% 
  mutate(value = value*100) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_point(
    aes(colour = value)
    , shape = "circle"
    , size = 1, alpha = 1
  ) +
  scale_color_viridis_c(
    "Crude sandwich index"
    , direction = -1
    , limits = c(0,5)
    ) +
  coord_cartesian(xlim = c(37, 93)) +
  labs(x = "Life expectancy", y = "Total fertility rate") +
  # labs(title = tit, caption = cap) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    , text = element_text(family = text_family, size = axis_text_size)
    , plot.caption = element_text(hjust = 0)
  )

ggsave("../../Output/alburez Figure 1_no_title.png", no_title, height = 12, width = 15, units = "cm")

no_title + labs(title = tit, caption = cap) 

ggsave(paste0("../../Output/alburez Figure 1.pdf"), width = 15, height = 12, units = "cm")
ggsave(paste0("../../Output/alburez Figure 1.eps"), width = 15, height = 12, units = "cm")

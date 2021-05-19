# 20200625
# Sandwich estimates from math model in a stable population

# The idea is to create something like a heatmap with the expected size of the sandwich generation
# given all observed combinations of mortality and fertility in the UN WPP but removing the time (cohort)
# dimension (ie plotting all countries and cohorts in one graph without differentiating them)
# A more advanced analysis would increase or reduce TFR and ex to create all *possible* combinations
# even if they are not observed in the empirical data.
# Having a stable population means that ASFR and ASMR do not change over time!

# Probability that a randomly selected woman who is alive at 
# age “a” in year “t” gave birth during the previous 5 years 
# and has a living mother who will die within 5 years
# (without considering mortality amongst children).

# Follows the formula:

# \begin{equation}
# S(a) = (\underbrace{1 - \prod_{x=1}^{\kappa} 1 - f(a-x)}_{\substack{\text{fertility risk in the}\\ \text{$\kappa$ years preceding age a}}}) \times \underbrace{M_1(a)}_{\substack{\text{Prob. that mother of ego}\\ \text{is alive when ego is a years old}}} \times  \underbrace{(1-  \frac{M_1(a+\tau)}{M_1(a)})}_{\substack{\text{Prob. that mother of ego}\\ \text{would die within $\tau$ years}}}    
# \label{eq:sand}
# \end{equation}
# 
# where $f(a-x)$ is the fertility of women at age $a-x$ and $M_1(a)$ is the probability of having a living mother at age $a$ in a stable population. 
# Conditional on Ego's survival, $M_1{(a)}$ can be thought of as a survival probability in a life table: it has to be equal to 1 when $a$ is equal to zero (the mother is alive when she gives birth) and goes monotonically to zero. 
# It is the survival probability for the mother of Ego, conditional on her being alive at Ego's birth.


# 1. Prob of sandwich by age ---------------------

# These cannot change by definition
sex_keep <- "female"
in_law_lab <- "blood"


# cohorts_to_keep <- "every_5"
cohorts_to_keep <- "all"
cos_model <- switch (cohorts_to_keep,
  every_5 = seq(1950, 2000, 5)
  , all = 1950:2000
)

cohorts_labs <- paste0("cohort_", cohorts_to_keep)

tau <- 5

# FOR NOW, just run the code below multiple times with different values for kappa and 
# tau to get the differnet 'types of sandwich':
# Min age of child
# kappa <- 5
# kappa <- 15
kappa <- 18


model_2 <- lapply(
  X = country_keep
  , FUN = model_sandwich_estimates_stable
  # Child's age
  , kappa = kappa
  # years before parents death
  , tau = tau
  , ASFRC_all = ASFRC_all
  , LTCF_all = LTCF_all
  , cos_model = cos_model
  , min_age_model = min_age_model
  , max_age_model = max_age_model
  , age_range_model = age_range_model
  , rep_age = rep_age
)

s_a <- do.call(rbind, model_2) %>% 
  data.frame() 

path_stable <- paste0(sex_keep, "_", cohorts_labs, "_", in_law_lab, "_", kappa, "_", tau)

write.csv(s_a, paste0("../../Data/estimates/model_sandwich_proportion_stable_", path_stable, ".csv"), row.names = F)

print(paste0("Exporte: ", path_stable))

# PLOT TEMP ===========================

s_a %>% 
  filter(cohort %in% c(1950, 2000)) %>% 
  filter(country %in% c("japan", "austria", "niger", "guatemala")) %>% 
  ggplot() +
  # Plot line means of model
  geom_line(
    aes(x = age, y = value, colour = cohort)
    , size = 1.5
  ) +
  facet_wrap(~country)

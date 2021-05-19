
# 20200425
# Sandwich estimates from math model

# Probability that a randomly selected woman who is alive at 
# age “a” in year “t” gave birth during the previous 5 years 
# and has a living mother who will die within 5 years
# (without considering mortality amongst children).

# It follows the formula:

# \begin{equation}
# S(a,c) = \underbrace{  (1 -  \prod_{x=1}^{\kappa} 1 - {}_{1}F_{a-x,c} )      }_{\substack{   \text{Prob. of having given}\\ \text{birth in $\kappa$ preceding years}}   } \times \underbrace{M_{a, c}}_{\substack{\text{Prob. that mother of ego}\\ \text{is alive when ego is a years old}}} \times  \underbrace{(1-  \frac{M_{a+\tau, c}}{M_{a, c}})}_{\substack{\text{Prob. that mother of ego}\\ \text{would die within $\tau$ years}}} 
# \label{eq:discrete}
# \end{equation}

# 
# where $M_{a,c}$ is the probability of having a living mother at age $a$ 
# for an average woman born in cohort $c$:
#   
# \begin{equation}
# M_{a,c}=\sum_{x=\alpha}^{\beta} \left({\underbrace{\frac{_1F_{x,c-x}K_{x,c-x}}{\sum_{x=\alpha}^{\beta}{_1F_{x,c-x}K_{x,c-x}}}}_{\text{distribution of mothers}}\times \underbrace{\frac{l_{x+a,c-x}}{l_{x,c-x}}}_{\text{mother's survival}}} \right)
# \label{eq:ma}
# \end{equation}

# We consider an ego sandwiched if they have at least one child 
# - aged $\kappa$ or younger and 
# - a mother within $\tau$ years of death



# Get age-specific probability of sanwdiwchness for an ego
# having a child aged kappa or less and a parent within tau years
# of death:

# 2. kappa = 5; tau = 5 --------------------------------------------------

# Min age of child
kappa <- 5
# Years in which parents of sandwiched ego will die
tau <- 5

model_1 <- lapply(
  X = country_keep
  , FUN = model_sandwich_estimates
  # Child's age
  , kappa = kappa
  # years before parents death
  , tau = tau
  , ASFRC_all = ASFRC_all
  , LTCF_all = LTCF_all
  , female_births_all = female_births_all
  , lx_df_full = lx_df_full
  , cos_model = cos_model
  , min_age_model = min_age_model
  , max_age_model = max_age_model
  , age_range_model = age_range_model
  , rep_age = rep_age
  # If F, returns results, otherwise, 's' element to plot components separately
  , return_components = F
)

sand_model1 <- data.frame(do.call(rbind, model_1)) %>% 
  # Add this to plot later
  mutate(
    simulation = as.character("1")
    , source = factor("Model", levels = c("SOCSIM", "Model"))
    , kappa_tau = paste0(kappa, "_", tau)
  )

# 3. kappa = 18; tau = 5 --------------------------------------------------

# Min age of child
kappa <- 18
# Years in which parents of sandwiched ego will die
tau <- 5

model_2 <- lapply(
  X = country_keep
  , FUN = model_sandwich_estimates
  # Child's age
  , kappa = kappa
  # years before parents death
  , tau = tau
  , ASFRC_all = ASFRC_all
  , LTCF_all = LTCF_all
  , female_births_all = female_births_all
  , lx_df_full = lx_df_full
  , cos_model = cos_model
  , min_age_model = min_age_model
  , max_age_model = max_age_model
  , age_range_model = age_range_model
  , rep_age = rep_age
  # If F, returns results, otherwise, 's' element to plot components separately
  , return_components = F
)

sand_model2 <- data.frame(do.call(rbind, model_2)) %>% 
  # Add this to plot later
  mutate(
    simulation = as.character("1")
    , source = factor("Model", levels = c("SOCSIM", "Model"))
    , kappa_tau = paste0(kappa, "_", tau)
  )

# 3. kappa = 18; tau = 10 --------------------------------------------------

kappa <- 18 # Min age of child
tau <- 10 # Years in which parents of sandwiched ego will die

model_3 <- lapply(
  X = country_keep
  , FUN = model_sandwich_estimates
  # Child's age
  , kappa = kappa
  # years before parents death
  , tau = tau
  , ASFRC_all = ASFRC_all
  , LTCF_all = LTCF_all
  , female_births_all = female_births_all
  , lx_df_full = lx_df_full
  , cos_model = cos_model
  , min_age_model = min_age_model
  , max_age_model = max_age_model
  , age_range_model = age_range_model
  , rep_age = rep_age
  # If F, returns results, otherwise, 's' element to plot components separately
  , return_components = F
)

sand_model3 <- data.frame(do.call(rbind, model_3)) %>% 
  # Add this to plot later
  mutate(
    simulation = as.character("1")
    , source = factor("Model", levels = c("SOCSIM", "Model"))
    , kappa_tau = paste0(kappa, "_", tau)
  )

# 4. Consolidate ----

sand_model <- 
  bind_rows(sand_model1, sand_model2, sand_model3) %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  fix_socsim_countries(keep_regions = F) %>% 
  select(-simulation)

write.csv(sand_model, "../../Data/estimates/model_sandwich_proportion_all.csv")

print("Model estimates complete.")

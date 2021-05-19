
# STATUS 20200911: Incompelte
# ISSUE: dependency rates are period measures and sandwichness cohort ones!
# SOLUTION: re-estimate period sandwich rates from SOCSIM directly... :/

# From EZ: -	is dependency ratio negatively correlated with sandwichness? 
# In other words, do dependency ratios fail to account for the likelihood 
# of simultaneous responsibilities?

# Data needs ~~~~~~~~~~~~~~~~~~~~~~~~
# dependency (loaded from disk)
# tab (created in 13_table_paper, has data on timimng and size for all countries and selected years)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Understand data ~~~~~~~~~~~~~~~~~~~~
# The dependency df has the following information
# 1. Dependency ratio. Numerator:people younger than 15 and older that 64; denominator: between 15 and 64 years old. 
# 2. Young dependency ratio. Numerator: younger than 15 years old. 
# 3. Old dependency ratio. Numerator older than 64 years.  
# 4. Prospective old-age dependency ratio. Numerator: people expected to die within a certain number of years.

# 1. Paramters --------------

dependency_type <- c("old")
type_keep <- c("sand")
measure_keep <- c("size")
# dput(unique(tab$measure))

# dependency_type <- c("old", "prospective")
# dep_labels <- c("Old age", "Prospective old age")

# 2. Prepare data -------------------

head(dependency)
head(tab)

tab %>% 
  filter(type %in% type_keep) %>% 
  filter(measure %in% measure_keep) %>% 
  select(region, country, type, cohort, sandwich = value) %>% 
  left_join(
    dependency %>% 
      filter(variable %in% dependency_type) %>% 
      select(country, year, dependency = mean)
    , by = c("country, year")
  )

# Error makes sense: dependency is period and sandwichness is cohort - how to merge?

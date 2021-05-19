
# 1. Old and prospective --------------------

measure_keep <- c("old", "prospective")
dep_labels <- c("Old age", "Prospective old age")
# country_keep <- c("Sweden", "Guatemala")

dep_reg %>% 
  filter(between(year, min(x_lim), max(x_lim))) %>% 
  filter(variable %in% measure_keep) %>% 
  # filter(country %in% country_keep) %>% 
  filter(!region %in% regions_to_ignore) %>% 
  ggplot(aes(x = year, y = value, colour = region, linetype = variable)) +
  geom_line(size = 1) +
  # Labells for regions
  geom_label(
    aes(x = year, y = value, colour = region, label = region)
    , size = 2.5
    , data = . %>% filter(year == 2035, variable == measure_keep[1])
    # , colour = "black"
    , show.legend = F
    # , nudge_y = - 0.02
  ) +
  scale_y_continuous("Dependency ratio %", labels = function(br) br*100) +
  scale_x_continuous("") +
  scale_color_discrete("", guide = F) +
  # scale_color_manual("", values = c("#990099", "#009900", "#4C4C4C")
  #                    # , labels = function(br) str_to_title(br)
  #                    , labels = c("Old_age dependency:\n65+ / working_age", "Prospective dependency:\ndie_within_5_years / working_age")
  # ) +
  scale_linetype("", labels = dep_labels) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
  )

ggsave("../../Output/dependency_rates_old.pdf", width = 14, height = 11, units = "cm")


# 2.1. All rates 

# dependency %>% 
#   filter(country %in% c("ARM")) %>% 
#   ggplot(aes(x = year, y = mean, colour = country)) +
#   geom_line() +
#   scale_y_continuous("Rate", labels = function(br) br*100) +
#   scale_x_continuous("") +
#   scale_color_discrete("") +
#   facet_wrap(~variable, scales = "free_x") +
#   theme_bw() +
#   theme(
#     legend.position = "bottom"
#   )


# Check against world back data: 
# https://data.worldbank.org/indicator/SP.POP.DPND.OL?locations=SE-GT-US-NE

# ~~~~~~~~~~~~~~~~~ -------------------------
# B. Correlation dependency ~ size of sandwich generation ----------------------

# TODO 
# - Get cohort estiamtes of dependency from SOCSIM outputs


# Dispel the myth that dependency ratios are a good proxy for sandwichness

# For this, both measure have to be cohort
# Currently dependency is period and 'size' (% sandwiched) is period...

# Mix period and cohort for now as a quick approximation
# FOr a more accurate analysis estimate dependency this from the SOCSIM outputs directly

warning("These estaimtes are incorrect - they conflate period and cohort!")

# Estimate cohort values
dep_coh <- 
  dep %>% filter(variable == "prospective") %>% 
  select(country, cohort = year, dependency = value)

dep_size <- 
  left_join(
    dep_coh
    , size %>% select(region, country, cohort, size = value)
    , by = c("country", "cohort")
    ) %>% 
  filter(between(cohort, min(x_lim), max(x_lim))) 
  # pivot_longer(dependency:size, names_to = "variable")

# Plot

dep_size %>% 
  filter(!is.na(region)) %>% 
  # filter(cohort %in% c(2000)) %>% 
  ggplot(aes(y = size, x = dependency, colour = cohort)) +
  geom_point() +
  # geom_path(aes(group = country)) +
  # Region lines
  # geom_smooth(
  #   aes(group = region), method = lm, formula = y ~ splines::bs(x, 4)
  #   , se = F, size = 2, colour = "black"
  # ) +
  geom_abline() +
  coord_cartesian(xlim = c(0, 0.85)) +
  facet_wrap(~region) +
  theme_bw()

ggsave("../../Output/dependency_x_size.pdf")


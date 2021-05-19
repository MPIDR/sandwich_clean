# Data needs ~~~~~~~~~~~~~~~~~~~~~~~~
# tab (created in 13_table_paper, has data on timimng and size for all countries and selected years)
# tab_mean, same but already has SD
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

measure_levels <- c("size", "total duration (years)", "share of lifespan (%)")
type_levels <- c("sandwich", "grandsandwich")
types <- c("sand", "gsand")
# type_names <- setNames(type_levels, types)

# 1. Plot SD by region ============


tab_mean %>%
  filter(!region %in% regions_to_ignore) %>% 
  mutate(
    type = factor(type, levels = types)
    , measure = factor(measure, levels = measure_levels)
    # , size = as.numeric(ifelse(region == "World", 1, 0.4))
    # , size = ifelse(region == "World", "dashed", "solid")
    ) %>% 
  ggplot(aes(x = cohort, y = sd, colour = region)) +
  geom_line(data = . %>% filter(region != "World")) +
  geom_line(data = . %>% filter(region == "World"), size = 0.8, colour = "black", linetype = "dashed") +
  scale_x_continuous("Cohort") +
  scale_y_continuous("Within-region SD") +
  scale_color_discrete("") +
  # scale_size_continuous("", guide = F, range = c(0.4,1)) +
  facet_grid(measure ~ type) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # Legend labels
    , legend.text=element_text(size=(8))
    # Legend title
    , legend.title = element_text(size = (8))
    # , axis.text.x = element_text(angle = 45)
    , panel.spacing = unit(1.2, "lines")
    # Move y axis closer to the plot
    , plot.margin = unit(c(t=0.2, r=0, b=0.1, l=0), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # , strip.text = element_text(hjust = 0)
  )


ggsave(paste0("../../Output/ineq_sd", "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 14, width = 14, units = "cm")

# 2. COefficient of variation ---------------

tab_mean %>%
  filter(!region %in% regions_to_ignore) %>% 
  mutate(
    cv = sd/ mean
    ,type = factor(type, levels = types)
    , measure = factor(measure, levels = measure_levels)
    # , size = as.numeric(ifelse(region == "World", 1, 0.4))
    # , size = ifelse(region == "World", "dashed", "solid")
  ) %>% 
  ggplot(aes(x = cohort, y = cv, colour = region)) +
  geom_line(data = . %>% filter(region != "World")) +
  geom_line(data = . %>% filter(region == "World"), size = 0.8, colour = "black", linetype = "dashed") +
  scale_x_continuous("Cohort") +
  scale_y_continuous("Within-region coefficient of variation") +
  # , labels = function(br) paste0(br*100, "%")
  scale_color_discrete("") +
  # scale_size_continuous("", guide = F, range = c(0.4,1)) +
  facet_grid(measure ~ type) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # Legend labels
    , legend.text=element_text(size=(8))
    # Legend title
    , legend.title = element_text(size = (8))
    # , axis.text.x = element_text(angle = 45)
    , panel.spacing = unit(1.2, "lines")
    # Move y axis closer to the plot
    , plot.margin = unit(c(t=0.2, r=0, b=0.1, l=0), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # , strip.text = element_text(hjust = 0)
  )


ggsave(paste0("../../Output/ineq_coef_var", "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 14, width = 14, units = "cm")

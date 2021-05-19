
# To do
# 1. Add line showing min age at sandwich
# 2. Define 'middle age' lines prospectively also and not using fixed values over time

# A lexis field with period on x axis and age in y axis (cohorts are diagonals)
# Color of parallelogram (or square) represents the probability of being sandwiched
# Working with SOCSIM data, this is the share of the period-age square that is sandwiched
# If I want the x axis to be period, I need to transform the data a bit to approximate period
# from cohorts

# DATA NEEDED
# I need something like sand_socsim_aggregated_simulations:

# country   cohort   age source kappa_tau   value
# <chr>     <chr>  <dbl> <fct>  <chr>       <dbl>
#   1 guatemala 1965      15 SOCSIM 18_10     0.00204
# 2 guatemala 1965      15 SOCSIM 18_5      0.00102
# 3 guatemala 1965      15 SOCSIM 5_5       0.00102
# 4 guatemala 1965      16 SOCSIM 18_10     0.00721
# 5 guatemala 1965      16 SOCSIM 18_5      0.00409
# 6 guatemala 1965      16 SOCSIM 5_5       0.00409

# But for all birth cohorts and for only one value of kappa_tau
# Maybe I already createed this somewhere else??

# 0. PArameters ----------------

# width <- 25
# height <- 20
width <- 14
height <- 12

# COhort choice
# According to EZ:
# I would avoid 1950 because those rates had been used to get to a stable population 
# with the 1950 rates. Segments with changing rates start from 1950 and it may take 
# some time for the effect of the "burning period" to disappear. The later we start, 
# e.g. 1970 or so, the safer. 

# A few years ago we did non lack courage, and we included women born in 2100. I feel 
# like I would be more conservative now and would not dare go beyond the cohort of 
# 2040 or so, unless we want to make a point about the implications of current projections
# in the long run.


x_lim <- c(1970, 2040)
x_br <- c(1970, 1990, 2020, 2040)
# x_labs <- c("1950", "2000", "50", "100")

y_lim <- c(15, 85)

# gradient_label <- "Probability"
gradient_label <- "Women\nsqueezed %"

# Kappa and tau values
# k_t_all <- sort(unique(grandsand_socsim_aggregated_simulations$kappa_tau))
# # (k_t <- k_t_all[1])
# (k_t <- k_t_all[2])
# # (k_t <- k_t_all[3])
# 
# kappa <- gsub("_[0-9]+$", "", k_t)
# tau <- gsub("^[0-9]+_", "", k_t)

# ~~~~~~~~~~~~~~~~~~~--------------------------
# 1. Sandwiched ~~~~ -----------------

# For heatmap
bar_br_both <- c(0, 0.04, 0.10, 0.14)
bar_lim_both <- c(0, 0.14)

# bar_br_sand <- c(0, 0.05, 0.1)
# bar_lim_sand <- c(0, 0.1) 

# bar_br_grandsand <- c(0, 0.02, 0.04, 0.06)
# bar_lim_grandsand <- c(0, 0.06)

bar_br_grandsand <- bar_br_sand <- bar_br_both
bar_lim_grandsand <- bar_lim_sand <- bar_lim_both

# 1.1. Format data ====

df_coh_sand <- 
  sand_reg %>% 
  filter(between(cohort, x_lim[1], x_lim[2])) %>% 
  filter(!region %in% regions_to_ignore) %>% 
  select(region, cohort, age, value) %>% 
  mutate(
    age = as.numeric(age)
    , cohort = as.numeric(cohort)
    , id = paste(region, age, cohort, sep = "_")
  ) 

# 1.2. Get coordinates ====

coords_coh_sand <- data.frame(do.call(rbind, apply(select(df_coh_sand, - value), 1, lexis_coord_cohort_sand) ))

datapoly_coh_sand <- 
  df_coh_sand %>% 
  select(id, value, region) %>% 
  left_join(coords_coh_sand, by = "id")
  
# 1.3. Plot =====

p_sand <- 
  datapoly_coh_sand %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  labs(x ="Cohort of women", y = "Woman's age 'a'") +
  scale_x_continuous(br = x_br, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_gradient(gradient_label, low = "white", high = "black") +
  scale_fill_viridis_c(
    gradient_label,option = "magma", direction = -1
    # , breaks = pretty_breaks(3), labels = function(br) br*100
    , breaks = bar_br_sand, labels = function(br) br*100
    , limits = bar_lim_sand
    ) +
  facet_wrap( ~ region) +
  coord_fixed(ylim = y_lim, xlim = x_lim) +
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    , legend.text=element_text(size=(base_size-2))
    , panel.spacing = unit(1.4, "lines")
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = - 3, b = 0, l = -3))
    , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    , strip.text = element_text(hjust = 0)
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
  ) 

# p_sand

ggsave(paste0("../../Output/heat_sand_region1.pdf"),p_sand, width = width, height = height, units = "cm")

# 1.3.2 With max age =======================

lines_max_df <- 
  datapoly_coh_sand %>% 
  group_by(region, cohort) %>% 
  filter(value == max(value, na.rm = T)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(region, cohort) %>% 
  select(-value)

p_sand +
  geom_line(aes(x = cohort, y = age), data = lines_max_df, size = 1, colour = "blue")

ggsave(paste0("../../Output/heat_sand_region2.pdf"), width = width, height = height, units = "cm")

# 1.3.3 Plot with middle-age lines ======================

p_sand +
  geom_hline(yintercept = c(40, 65), linetype = "dashed", size = 1)

ggsave(paste0("../../Output/heat_sand_region3.pdf"), width = width, height = height, units = "cm")

# ~~~~~~~~~~~~~~~~~~~--------------------------
# 2. Grand-sandwiched ~~~~ -----------------


# 1.1. Format data ====

df_coh_grandsand <- 
  grandsand_reg %>% 
  filter(between(cohort, x_lim[1], x_lim[2])) %>% 
  filter(!region %in% regions_to_ignore) %>% 
  select(region, cohort, age, value) %>% 
  mutate(
    age = as.numeric(age)
    , cohort = as.numeric(cohort)
    , id = paste(region, age, cohort, sep = "_")
    , region = as.character(region)
  ) 

# 1.2. Get coordinates ====

coords_coh_grandsand <- data.frame(do.call(rbind, apply(select(df_coh_grandsand, - value), 1, lexis_coord_cohort_sand) ))

datapoly_coh_grandsand <- 
  df_coh_grandsand %>% 
  select(id, value, region) %>% 
  left_join(coords_coh_grandsand, by = "id")

# 1.3. Plot =====

p_grand <- 
  datapoly_coh_grandsand %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  labs(x ="Cohort of women", y = "Woman's age 'a'") +
  scale_x_continuous(br = x_br, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_gradient(gradient_label, low = "white", high = "black") +
  scale_fill_viridis_c(
    gradient_label,option = "magma", direction = -1
    , breaks = bar_br_grandsand, labels = function(br) br*100
    , limits = bar_lim_grandsand
  ) +
  facet_wrap( ~ region) +
  coord_fixed(ylim = y_lim, xlim = x_lim) +
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    , legend.text=element_text(size=(base_size-2))
    , panel.spacing = unit(1.4, "lines")
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = - 3, b = 0, l = -3))
    , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    , strip.text = element_text(hjust = 0)
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
  ) 

# p_grand

ggsave(paste0("../../Output/heat_grandsand_region1.pdf"), p_grand, width = width, height = height, units = "cm")

# 2.3.2 With max age =======================

lines_max_df <- 
  datapoly_coh_grandsand %>% 
  group_by(region, cohort) %>% 
  filter(value == max(value, na.rm = T)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(region, cohort) %>% 
  select(-value)

p_grand +
  geom_line(aes(x = cohort, y = age), data = lines_max_df, size = 1, colour = "blue")

ggsave(paste0("../../Output/heat_grandsand_region2.pdf"), width = width, height = height, units = "cm")

# 2.3.3 Plot with middle-age lines ======================

p_grand +
  geom_hline(yintercept = c(40, 65), linetype = "dashed", size = 1)

ggsave(paste0("../../Output/heat_grandsand_region3.pdf"), width = width, height = height, units = "cm")

# ~~~~~~~~~~~~~~~~~~~--------------------------
# 3. TOGETHER ~~~~ -----------------

# Assuming for a second that the prob of being either sandwiched or grandsanwiched at any
# given age is the sum of the prob of either of the two.

df_coh_both <-
  df_coh_sand %>% 
  mutate(merge_id = paste0(region, "_",cohort, "_",age)) %>%
  select(merge_id, value) %>% 
  rename(sand = value) %>% 
  left_join(
    df_coh_grandsand %>% 
      mutate(merge_id = paste0(region, "_",cohort, "_",age)) %>%
      rename(gsand = value)
    , by = "merge_id"
    ) %>%
  mutate(value = sand + gsand) %>% 
  arrange(region, cohort, age) %>% 
  # select(id, sand, gsand, value) %>% View
  select(-merge_id, - sand, -gsand)

coords_coh_both <- data.frame(do.call(rbind, apply(select(df_coh_both, - value), 1, lexis_coord_cohort_sand) ))

datapoly_coh_both <- 
  df_coh_both %>% 
  select(id, value, region) %>% 
  left_join(coords_coh_both, by = "id")

# 3.1.PLot ===================

p_both <-
  datapoly_coh_both %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  labs(x ="Cohort of women", y = "Woman's age 'a'") +
  scale_x_continuous(br = x_br, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(
    gradient_label,option = "magma", direction = -1
    , breaks = bar_br_both, labels = function(br) br*100
    , limits = bar_lim_both
  ) +
  facet_wrap( ~ region) +
  coord_fixed(ylim = y_lim, xlim = x_lim) +
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    , legend.text=element_text(size=(base_size-2))
    , panel.spacing = unit(1.4, "lines")
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = - 3, b = 0, l = -3))
    , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    , strip.text = element_text(hjust = 0)
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
  ) 

# p_both

ggsave(paste0("../../Output/heat_both_regions1.pdf"), p_both, width = width, height = height, units = "cm")

# 1.3.3 Plot with max age ======================


lines_max_df <- 
  datapoly_coh_both %>% 
  group_by(region, cohort) %>% 
  filter(value == max(value, na.rm = T)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(region, cohort) %>% 
  select(-value)

p_both +
  geom_line(aes(x = cohort, y = age), data = lines_max_df, size = 1, colour = "blue")

ggsave(paste0("../../Output/heat_both_regions2.pdf"), width = width, height = height, units = "cm")
  
# 3.3.3 Plot with middle-age lines ======================

p_both +
  geom_hline(yintercept = c(40, 65), linetype = "dashed", size = 1)

ggsave(paste0("../../Output/heat_both_regions3.pdf"), width = width, height = height, units = "cm")

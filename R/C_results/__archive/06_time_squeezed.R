
# 20200620

# Disable if getting only estimates as 'share of life' not as 'share of adult life'
start_of_adult_life <- 15

height <- 9
width <- 12

# ~~~~~~~~~~~~~~ ------------------------------------
# A. Cohort-level estimates  ------------------------------------

# 4. MAP: Duration  -----------------------------

# ISSUE! 20200803
# sd436
# Some countries are missing fro the duration df (loaded from csv):
# sort(unique(ex_country$country)[!unique(ex_country$country) %in% unique(dur$country)])
# [1] "ABW" "AFG" "AGO" "ALB" "ARG" "ARM" "ATG" "COG" "CRI" "CUB" "CUW" "CYP" "CZE" "DZA" "FSM" "HRV" "MKD" "MYT" "PRK" "TZA"

cohort_map <- 1970
# cohort_map <- 2000
# cohort_map <- 2040

type_keep <- "sandwiched"
# type_keep <- "gsandwiched"

# Use this for stacked maps
country_line_size <- 0.0005
title_size <- 9
viridis_direction <- -1

type_lab <- switch(type_keep,
                   sandwiched = "sandwiched"
                   , gsandwiched = "grandsandwiched")


bar_name <- paste0("Duration\nin years\n", type_lab)

p_title <- paste0("Time spent ", type_lab, " (", cohort_map," cohort)")


bar_lim <- switch (type_keep,
                   sandwiched = c(1.5, 8)
                   , gsandwiched = c(1.8, 7.9)
)

bar_br <- switch (type_keep,
                  sandwiched = c(2,4,6,8)
                  , gsandwiched = c(3,5,7)
)

# Get country-level estimates

# Life expectancy by region

ex_country <- 
  LTCF_all %>% 
  filter(Age == 0) %>% 
  select(country = Country, cohort = Cohort, ex) %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)) %>% 
  filter(!is.na(country)) 

# Country-level sandwich estimate

years_squeezed <-
  dur %>% 
  # years_squeezed as share of adult life expectancy
  left_join(ex_country, by = c("country", "cohort")) %>% 
  mutate(
    share = (value / ex)*100
    # , share_adult = (value / (ex - start_of_adult_life))*100
  )

# 0. Create world map 
# Can be reused for all maps

world <- 
  sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country))

# Get value :
values <-
  years_squeezed %>%
  filter(type == type_keep) %>% 
  filter(cohort %in% cohort_map) %>% 
  select(country, value = value)

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
  # , labels = function(br) paste0(round(br*100), "%")
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


ggsave(paste0("../../Output/map_duration", "_", type_keep, "_",cohort_map, "_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf"), height = 7, width = 16, units = "cm")
ggsave(paste0("../../Output/map_duration", "_", type_keep, "_",cohort_map, "_",kappa_sand, "_",kappa_gsand, "_",tau, ".png"))
# ~~~~~~~~~~~~~~~~~~~~--------------------
# B. Time Sandwiched per TFR and e0 ------------------------------


# Plot with x = TFR, y = e0 and values =expected time sandwiched for each country
# This would allow to see conttributino of changes in mort and fert to outcome

# Get cohort TFR

CTFR_country <-
  ASFRC_all %>% 
  filter(between(Cohort, x_lim[1], x_lim[2])) %>% 
  group_by(country, Cohort) %>% 
  summarise(TFR = sum(ASFR)) %>% 
  ungroup() %>% 
  select(country, cohort = Cohort, TFR) %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)) %>% 
  filter(!is.na(country)) 

squeeze <- 
  dur %>% 
  left_join(CTFR_country, by = c("country", "cohort")) %>% 
  left_join(ex_country, by = c("country", "cohort"))

# All countries in facets ============

squeeze  %>% 
  filter(type == "sandwiched") %>% 
  filter(!region %in% regions_to_ignore) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_path(
    aes(group = country), size = 0.2, alpha = 0.3
  ) +
  geom_point(
    aes(colour = value, shape = cohort), size = 2.5
    # aes(colour = share, shape = cohort), size = 2.5
    , data = . %>% filter(cohort %in% c(1970, 2000))
  ) +
  scale_color_viridis_c("Years spent\nsandwiched",direction = -1) +
  scale_shape_manual("", breaks = c(1970, 2000), values = c(16,17)) +
  facet_wrap(~region) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/squeezed_sand_tfr_ex.pdf"), height = height, width = width, unit = "cm")

# Plot regions ==================

squeeze_r <- 
  squeeze %>% 
  # mutate(value = share_adult) %>%
  group_by(region, cohort) %>% 
  summarise_at(c("ex", "TFR", "value"), mean, na.rm = T) %>% 
  ungroup() %>% 
  mutate(cohort = as.character(cohort)) 

squeeze_r %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_path(
    aes(group = region, colour = value)
    , size = 1
  ) +
  geom_point(
    aes(colour = value, shape = cohort), size = 4
    # aes(colour = share, shape = cohort), size = 2.5
    , data = . %>% filter(cohort %in% c(1970, 2000))
  ) +
  geom_label_repel(
    aes(label = region)
    , size = 3
    , data = . %>% filter(cohort %in% 1970)
    , show.legend = F
  ) +
  scale_color_viridis_c("Years spent\nsandwiched",direction = -1, breaks = scales::pretty_breaks(3)) +
  scale_shape_manual("", breaks = c(1970, 2000), values = c(16,17)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/squeezed_tfr_ex_reg.pdf"))

# Regions + countries ==============

p_sq <-
  ggplot() +
  # Countries ~~~~~~~~~~~
  geom_path(
    aes(x = ex, y = TFR, group = country, colour = value), size = 0.2, alpha = 0.3
    , squeeze
  ) +
  # Regions ~~~~~~~~~~~
  geom_path(
    aes(x = ex, y = TFR, group = region, colour = value)
    , size = 2
    , data = squeeze_r
  ) +
  geom_point(
    aes(x = ex, y = TFR, colour = value, shape = cohort), size = 5
    # aes(colour = share, shape = cohort), size = 2.5
    , data = squeeze_r %>% filter(cohort %in% c(1970, 2000))
  ) +
  geom_label_repel(
    aes(x = ex, y = TFR, label = region)
    , size = 3
    , data = squeeze_r %>% filter(cohort %in% 1970)
    , show.legend = F
  ) +
  scale_color_viridis_c(
    "Years spent\nsandwiched",direction = -1, breaks = scales::pretty_breaks(3)
    # , limits = c(1.5, 4)
  ) +
  scale_shape_manual("", breaks = c(1970, 2000), values = c(16,17)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

# Takes long to plot
# p_sq

ggsave(paste0("../../Output/squeezed_tfr_ex_reg_con.pdf"), p_sq)

# 4. All country-cohort in one plot ===========================================

# Crazy plot tat plots all the duration data in one single space

# 4.1.  Total duration ===================

squeeze  %>% 
  mutate(cohort = as.character(cohort)) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_point(
    aes(colour = value), size = 4, shape = "square"
  ) +
  scale_color_viridis_c("Years spent\nsandwiched",direction = -1) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/squeezed_tfr_ex_crazy.pdf"))

# 4.1.  As share of life ===================

squeeze  %>% 
  mutate(
    share = (value / ex)*100
    , share_adult = (value / (ex - start_of_adult_life))*100
  ) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_point(
    aes(colour = share)
    , shape = "square"
    , size = 6, alpha = 1
    ) +
  scale_color_viridis_c("Share of life\nsandwiched",direction = -1, labels = function(br) c(br[-length(br)], paste0(br[length(br)], "%")) ) +
  # scale_color_viridis_c("Share of adult\nlife sandwiched",direction = -1, labels = function(br) c(br[-length(br)], paste0(br[length(br)], "%"))) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/squeezed_tfr_ex_crazy_share.pdf"))

# B.3 Time Grandsandwiched per TFR and e0 ------------------------------


# Plot with x = TFR, y = e0 and values =expected time sandwiched for each country
# This would allow to see conttributino of changes in mort and fert to outcome

# Get cohort TFR

CTFR_country <-
  ASFRC_all %>% 
  filter(between(Cohort, x_lim[1], x_lim[2])) %>% 
  group_by(country, Cohort) %>% 
  summarise(TFR = sum(ASFR)) %>% 
  ungroup() %>% 
  select(country, cohort = Cohort, TFR) %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)) %>% 
  filter(!is.na(country)) 

squeeze <- 
  dur %>% 
  left_join(CTFR_country, by = c("country", "cohort")) %>% 
  left_join(ex_country, by = c("country", "cohort"))

# All countries in facets ============

squeeze  %>% 
  filter(type == "gsandwiched") %>% 
  filter(!region %in% regions_to_ignore) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  ggplot(aes(y = TFR, x = ex)) +
  geom_path(
    aes(group = country), size = 0.2, alpha = 0.3
  ) +
  geom_point(
    aes(colour = value, shape = cohort), size = 2.5
    # aes(colour = share, shape = cohort), size = 2.5
    , data = . %>% filter(cohort %in% c(1970, 2000))
  ) +
  scale_color_viridis_c("Years spent\ngrandsandwiched",direction = -1) +
  scale_shape_manual("", breaks = c(1970, 2000), values = c(16,17)) +
  facet_wrap(~region) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0("../../Output/squeezed_gsand_tfr_ex.pdf"), height = height, width = width, unit = "cm")

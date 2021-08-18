# 0. Preamble --------------

ci <- 0.95

cohort_map <- 1970

# Use this for stacked maps
country_line_size <- 0.0005
title_size <- 9
viridis_direction <- -1

bar_name <- paste0("Years\n", "sandwiched")
bar_lim <- c(0.5, 7.6) # have same scale for rate and duration
bar_br <- c(1:8) 

# Create world map 

world <-
  sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>%
  select(ID = ADMIN, country = ADM0_A3, geometry) %>%
  mutate(ID = as.character(ID), country = as.character(country)) %>% 
# recode Greenland as denmakr
  assign_contested_iso3_countries(.)

# 2.1. Get objects needed ==================

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
  )

# 2.3. Plot =================

# SANDWICH

# Get value :
values_sand <-
  years_squeezed %>%
  filter(type == "sandwiched") %>%
  filter(cohort %in% cohort_map) %>% 
  select(country, type, value)

# Join with map
w_sand <- 
  left_join(
  world
  , values_sand
  , by = "country"
) %>%
  mutate(type = "sand")

# GRANDSANDWICH

# Get value :
values_gsand <-
  years_squeezed %>%
  filter(type == "gsandwiched") %>%
  filter(cohort %in% cohort_map) %>% 
  select(country, type, value)

# Join with map
w_gsand <- 
  left_join(
  world
  , values_gsand
  , by = "country"
) %>%
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

# Plot

tit <- "FIGURE 3   Expected number of years in a sandwich or grandsandwich state (1970 cohort)."
cap <- "NOTE: A person is (grand)sandwiched if they simultaneously have a frail parent within five years of death and a \npotentially dependent child or grandchild aged 15 or younger. The maps show mean values for men and women combined."

no_title <- 
  ggplot(data = w) +
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
  ) +
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

ggsave("../../Output/alburez Figure 3_no_title.pdf", no_title, height = 14, width = 16, units = "cm")

no_title + labs(title = tit, caption = cap) 

ggsave("../../Output/alburez Figure 3.pdf", height = 14, width = 16, units = "cm")

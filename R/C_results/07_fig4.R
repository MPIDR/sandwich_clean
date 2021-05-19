
# For plot
x_lim <- c(15, 80)
w_p <- 5
h_p <- 4

br_comb <- c("sand","gsand", "combined")
labs_comb <- c("Sandwich", "Grandsandwich", "Sandwich or grandsandwich")

# 4. Sand OR gsand  ~~~~~~~ ----------------------

# Probability of being sandwiched or grandsandwiched over age
# Including one line to show the 'adjusted probability of being sandwiched OR grandsand"
# This is a weighted version of the P(sand|gsand)*w(a), where w(a) is a vector of the
# share of people (by country, cohort, age) 
# The weighting has to be done at the country level!

cohort_keep <- 1970
ages_smooth <- seq(15, 100, 5)
ages_points <- seq(15, 100, 15)
key_countries <- c("ETH", "IRN", "CHN","GTM", "DEU", "USA")

# Get dfs 

doomed_con <- 
  doomed_pop %>% 
  group_by(country, cohort, age) %>% 
  summarise(doomed = mean(share, na.rm = T)) %>% 
  ungroup() 

doomed_reg <- 
  doomed_con %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>%
  group_by(region, cohort, age) %>%
  summarise(doomed = mean(doomed, na.rm = T)) %>% 
  ungroup() 

# Sand df

sand_gsand_con <-
  sand_socsim_aggregated_simulations %>%
  select(country, cohort, age, sand = value) %>%
  left_join(
    grandsand_socsim_aggregated_simulations %>%
      select(country, cohort, age, gsand = value)
    , by = c("country", "cohort", "age")
  ) %>%
  left_join(doomed_con, by = c("country", "cohort", "age")) %>%
  mutate(combined = sand + gsand) %>%
  pivot_longer(c("combined", "sand", "gsand"), names_to = "variable")


sand_gsand_reg <- 
  sand_gsand_con %>% 
  find_regions_code(ignore_regions = F, pretty_names = T) %>% 
  group_by(region, cohort, age, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% 
  ungroup() 
  
# Change region labels

rename_region <- function(df) {
  lookup <- c( 
    "Sub-Sah Africa" = "Sub-Saharan Africa"
    , "N Africa & W Asia" = "North Africa & West Asia"
    , "C & S Asia" = "Central & South Asia"
    , "E & SE Asia" = "East & Southeast Asia"
    , "LATAM & Caribbean" = "Latin America & Caribbean"
    , "AUS & NZ" = "Australia & New Zealand"
    , "Oceania (other)" = "Oceania (other)"
    , "Europe & N America" = "Europe & North America"
    )
  
  df$region <- lookup[df$region]
  df$region <- factor(df$region, levels = lookup)
  df
}

# (Fig 4) 4.1. By region =================

tit <- "FIGURE 4   Distribution of parental and grandparental sandwichness \nover the life course (1970 cohort). "
cap <- "NOTE: Higher values for the black lines indicate that a larger share of a given age group has \nsimultaneous care responsibilities for older parents and young children or grandchildren (regional \nmeans for male and female populations). The red line shows the share of people in a given age \ngroup who, in the microsimulations, will die in the next five years."

no_title <- 
  ggplot() +
  # Plot line means of model
  geom_line(
    aes(x = age, y = value, linetype = variable)
    , data = sand_gsand_reg %>% 
      filter(cohort %in% c(cohort_keep)) %>%
      filter(!region %in% regions_to_ignore) %>%
      rename_region() %>% 
      mutate(value = value*100)
  ) +
  # Expected to die line
  geom_line(
    aes(x = age, y = doomed)
    , size = 0.5
    , colour = "red"
    , data = doomed_reg %>% 
      mutate(doomed = doomed*100) %>% 
      filter(cohort %in% c(cohort_keep)) %>%
      filter(!region %in% regions_to_ignore) %>% 
      rename_region() %>% 
      filter(age %in% ages_smooth) 
  ) +
  # # Expected to die points
  geom_point(
    aes(x = age, y = doomed)
    , size = 2
    , colour = "red"
    , shape = 17
    , data = doomed_reg %>%
      mutate(doomed = doomed*100) %>%
      filter(cohort %in% c(cohort_keep)) %>%
      filter(!region %in% regions_to_ignore) %>%
      rename_region() %>%
      filter(age %in% ages_points)
  ) +
  facet_wrap(~region) +
  # Formatting
  scale_y_continuous(
    "Share of age group sandwiched"
    , breaks = scales::pretty_breaks(4)
    , sec.axis = sec_axis(~ . , name = "Share expected to die within 5 years (triangles)")
  ) +
  scale_x_continuous(paste0("Age (", cohort_keep, " cohort)" )) +
  scale_linetype_manual("", breaks = br_comb, labels = labs_comb, values = c("solid", "dashed", "dotted")) +
  scale_color_discrete("") +
  # labs(title = tit, caption = cap) +
  coord_cartesian(ylim = c(0, 22), xlim = x_lim) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # REmove space between acis and label
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    , axis.title.y.right = element_text(colour = "red")
    , axis.text.y.right = element_text(colour = "red")
    , axis.ticks.y.right = element_line(colour = "red")
    , strip.text.x = element_text(size = 8)
    , text = element_text(family = text_family, size = axis_text_size)
    , plot.title = element_text(size = title_text_size)
    , plot.caption = element_text(hjust = 0)
  )


# ggsave(
#   paste0("../../Output/sandwich_over_age_doomed_",cohort_keep,"_",kappa_sand, "_",kappa_gsand, "_",tau, ".pdf")
#        , width = w_p, height = h_p
# )
  
ggsave("../../Output/alburez Figure 4_no_title.png", no_title, height = 12, width = 16, units = "cm")
  
no_title + labs(title = tit, caption = cap) 

ggsave("../../Output/alburez Figure 4.pdf", width = 5, height = 5.5)
ggsave("../../Output/alburez Figure 4.eps", width = w_p, height = h_p)




source("../functions.R")

# Data wrangling
wrangling <- 
  c("tidyverse", "scales", "patchwork", "data.table","parallel", "countrycode"
    , "ggrepel", "sf", "knitr", "viridis", "showtext")

library2(wrangling)

# For plotting

windowsFonts(Times = windowsFont("Times New Roman"))

font_add("times", regular = "times.ttf", bold = "timesbd.ttf")
showtext_auto() 

text_family <- "times"
title_text_size <- 11
axis_text_size <- 10

regions_to_ignore <- c("AUS & NZ", "Oceania (other)")

?
  font_add

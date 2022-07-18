### Week 6 Assignment: Data Visualization ###

### Graph 1: Floral biomass vs. Treatment
rm(list=ls())
setwd("T:/Agronomy/7.Hemp/HempExperiements/NutritionStudy_2021/EO_block10/Completed_data_entry")

library(tidyverse)

harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  filter(!is.na(FlowerDryWeight_g)) %>%
  select(c(Unit, Treatment, FlowerDryWeight_g, TotalFreshWeight_g)) %>%
  mutate(FlowerDryWeight_kg = FlowerDryWeight_g / 1000)

ggplot(harvest_data) +
  geom_point(mapping = aes(x = Treatment, y = FlowerDryWeight_kg))



###Graph 2: Yield Mass vs. Treatment ###

rm(list=ls())
setwd("T:/Agronomy/7.Hemp/HempExperiements/NutritionStudy_2021/EO_block10/Completed_data_entry")

library(tidyverse)

harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  select( Unit, Treatment, Rep, TotalDryWeight_g) %>%
  group_by(Unit,Treatment) %>%
  mutate(TotalDryWeight_kg = TotalDryWeight_g / 1000) %>%
  summarize (avg_yield = mean (TotalDryWeight_kg, na.rm = TRUE), 
             sd_yield = sd (TotalDryWeight_kg, na.rm = TRUE))

ggplot(harvest_data) +
  geom_bar(mapping = aes (x = Treatment, y = avg_yield), stat = "identity")

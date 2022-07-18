rm(list=ls())
setwd("T:/Agronomy/7.Hemp/HempExperiements/NutritionStudy_2021/EO_block10/Completed_data_entry")

library(tidyverse)

harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  filter(!is.na(FlowerDryWeight_g)) %>%
  select(c(Unit, Treatment, FlowerDryWeight_g, TotalFreshWeight_g)) %>%
  mutate(FlowerDryWeight_kg = FlowerDryWeight_g / 1000)

View(harvest_data)

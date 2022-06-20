rm(list=ls())
setwd("T:/Agronomy/7.Hemp/HempExperiements/NutritionStudy_2021/EO_block10/Completed_data_entry")


library(tidyverse)

harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  select( Unit, Treatment, Rep, TotalDryWeight_g) %>%
  group_by(Unit,Treatment) %>%
  summarize (avg_yield = mean (TotalDryWeight_g, na.rm = TRUE), SD = sd (TotalDryWeight_g, na.rm = TRUE))


View(harvest_data)

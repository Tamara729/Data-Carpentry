### Week 8 Assignment: Statistics ###

### Part 1: ANOVA Test

#Clear the console and set working directory
rm(list=ls())
setwd("C:/Users/t.serrano/Documents/Courses/Summer 2022/AGR6905 Data Carpentry for Agroecologists")

#Load packages
library(tidyverse)
library (agricolae)

#Import, select, mutate, group, and summarize harvest data 
harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  select (Unit, Treatment, TotalDryWeight_g, FlowerDryWeight_g) %>%
  mutate(Treatment_kgha = Treatment*1.12,
         TotalDryWeight_kg = TotalDryWeight_g /1000,
         FlowerDryWeight_Kg = FlowerDryWeight_g/1000) %>%
  group_by(Unit, Treatment_kgha) %>%
  summarize(avg_yield = mean (TotalDryWeight_kg),
            ci_yield = 2*sd (TotalDryWeight_kg)/ sqrt (n()))

#Import & mutate canopy data
canopy_data <- read_csv("EO_Canopy_2021.csv") %>%
  select(ID, NDVI, GNDVI, CanopyArea) %>%
  mutate(unit = str_extract(ID, "[0-9][0-9][0-9]")) %>%
  group_by(unit) %>%
  summarize(avg_ndvi = mean(NDVI),
            ci_ndvi = 2*sd (NDVI)/sqrt(n()))

#Convert unit into a numeric factor
canopy_data$unit <- as.numeric (canopy_data$unit)

#Join wife_data and canopy_data together
combined_data <- left_join(canopy_data, harvest_data, by = c("unit" = "Unit")) 


#ANOVA & Tukey's HSD Test

yield_aov = aov (avg_yield ~ factor(Treatment_kgha), data = combined_data)
TukeyHSD(yield_aov)

#ANOVA Table
summary (yield_aov)


### Part 2: Linear Regression Model ###

test <- lm(log10(avg_yield)~avg_ndvi, data = combined_data)
summary(test)$r.squared #0.850


### Week 10 Assignment: Advanced Programming ###

#Clear the console and set working directory
rm(list=ls())
setwd("C:/Users/t.serrano/Documents/Courses/Summer 2022/AGR6905 Data Carpentry for Agroecologists")

#Load packages
library(tidyverse)
library (agricolae)

#Import data 
harvest<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  mutate(Treatment_kgha = Treatment*1.12) %>%
  rename(unit = Unit)
         
#Convert all variables containing a weight to SI units
harvest_data <- harvest 
for(i in 1:ncol(harvest_data)) {                              # Head of for-loop
  
  if(grepl("Weight", colnames(harvest_data)[i])) {            # Logical if condition
    
    harvest_data[ , i] <- harvest_data[ , i] / 1000           # Block code
  }
}

head(harvest_data)

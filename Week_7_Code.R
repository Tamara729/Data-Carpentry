### Week 7 Assignment: Data Visualization ###

### Graph 1: Floral biomass vs. N application
rm(list=ls())
setwd("T:/Agronomy/7.Hemp/HempExperiements/NutritionStudy_2021/EO_block10/Completed_data_entry")

library(tidyverse)


harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  filter(!is.na(FlowerDryWeight_g)) %>%
  select(c(Unit, Treatment, FlowerDryWeight_g, TotalFreshWeight_g)) %>%
  mutate(FlowerDryWeight_kg = FlowerDryWeight_g / 1000) %>%
  mutate(Treatment_kgha = Treatment * 1.12)

ggplot(harvest_data, aes(x = as.factor (Treatment_kgha), y = FlowerDryWeight_kg)) + 
  geom_point() +
  labs(x="N application [kg/ha]", y="Floral Mass [kg]") +
  theme_classic(base_size = 12, base_family = "TT Arial") +
  theme (axis.title = element_text(face = "bold")) +
  theme (axis.title.y = element_text (angle = 0, vjust = 1)) 


###Graph 2: Yield Mass vs. N application (with facet layer)

rm(list=ls())
setwd("C:/Users/t.serrano/Documents/Courses/Summer 2022/AGR6905 Data Carpentry for Agroecologists")

library(tidyverse)

harvest_data<-read.csv("EO_Harvest_Wife_2021.csv") %>%
  select( Unit, Treatment, Rep, TotalDryWeight_g) %>%
  mutate(Treatment_kgha = Treatment*1.12,
         TotalDryWeight_kg = TotalDryWeight_g /1000) %>%
  group_by(Unit,Treatment_kgha) %>%
  summarize (avg_yield = mean (TotalDryWeight_kg, na.rm = TRUE), 
             ci_yield = 2*sd (TotalDryWeight_kg/sqrt(n()), na.rm = TRUE))

ggplot(harvest_data, aes(x=as.factor(Treatment_kgha), y =avg_yield)) +
  geom_bar (stat = "identity") +
  labs(x="N Application [kg/ha]", y="Yield Mass [kg]") +
  theme_classic(base_size = 12, base_family = "TT Arial") +
  theme (axis.title = element_text(face = "bold")) +
  theme (axis.title.y = element_text (angle = 0, vjust = 1)) +
  facet_wrap(~Treatment_kgha, nrow=1)

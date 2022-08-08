# Week 11 & 12 Formatted Project Report --------------------------------------------------------------
# Analysis featuring yield mass, canopy area, GNDVI, and NDVI across six N applications

# Clear the console and set working directory ----------------------------------------------------

rm(list=ls())
setwd("C:/Users/t.serrano/Documents/Courses/Summer 2022/AGR6905 Data Carpentry for Agroecologists")

# Load packages ----------------------------------------------------------------------------------

library(tidyverse)
library(agricolae)

# Import harvest data, rename, mutate, and select variables --------------------------------------

harvest_data <- read.csv("EO_Harvest_Wife_2021.csv") %>%
  rename(unit = Unit) %>%
  mutate(Treatment_kgha = Treatment * 1.12,
                          TotalDryWeight_kg = TotalDryWeight_g / 1000) %>%
  select(unit, Treatment_kgha, Variety, TotalDryWeight_kg)

# Import canopy data, mutate, select, and extract variables --------------------------------------

canopy_data <- read_csv("EO_Canopy_2021_v2.csv") %>%
  select(ID, NDVI, GNDVI, CanopyArea) %>%
  mutate(unit = str_extract(ID, "[0-9][0-9][0-9]")) %>%
  select(unit, NDVI, GNDVI, CanopyArea )

# Convert unit into a numeric factor, combine data frames ----------------------------------------

canopy_data$unit <- as.numeric(canopy_data$unit)
combined_data <- cbind(harvest_data, canopy_data) %>%
  select(unique(colnames(.))) 
  
# Filter data by variety -------------------------------------------------------------------------

wife <- filter(combined_data, Variety == "Wife")

# Group by unit and fertility treatment, summarize mean and CI values for each plot --------------

wife_plot <- wife %>%
  group_by(unit, Variety, Treatment_kgha) %>%
  summarize(avg_yield_p = mean(TotalDryWeight_kg),
            ci_yield_p = 2*sd(TotalDryWeight_kg) / sqrt (n()),
            avg_canopy_p = mean(CanopyArea),
            ci_canopy_p = 2*sd(CanopyArea) / sqrt(n()),
            avg_ndvi_p = mean(NDVI),
            ci_ndvi_p = 2*sd(NDVI) / sqrt(n()),
            avg_gndvi_p = mean (GNDVI),
            ci_gndvi_p = 2*sd(GNDVI) / sqrt(n()))

# Group by treatment, summarize mean and CI values for six fertility treatments ------------------

wife_rate <- wife_plot %>%
  group_by(Treatment_kgha) %>%
  summarize(avg_yield = mean(avg_yield_p),
            ci_yield = 2*sd(avg_yield_p) / sqrt(n()),
            avg_canopy = mean(avg_canopy_p),
            ci_canopy = 2*sd(avg_canopy_p) / sqrt(n()),
            avg_ndvi = mean(avg_ndvi_p),
            ci_ndvi = 2*sd(avg_ndvi_p) / sqrt(n()),
            avg_gndvi = mean(avg_gndvi_p),
            ci_gndvi = 2*sd(avg_gndvi_p) / sqrt(n()))

# Plot yield versus N application =================================================================================

ggplot(wife_plot, aes(x = as.factor (Treatment_kgha), y = avg_yield_p)) + 
  geom_point() +
  labs(x = "N application [kg/ha]", y = "Yield Mass [kg]") +
  theme_classic(base_size = 12, base_family = "TT Arial") 

# Yield ANOVA model and Duncan.test, assign groupings --------------------------------------------------------------

yield_aov <- aov(avg_yield_p ~ Treatment_kgha, data = wife_plot)
print(summary(yield_aov))

print(duncan.test(yield_aov,"Treatment_kgha")$groups)

groupings_yield <-data.frame (Treatment_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("d","c","bc", "bc", "a", "ab"))

# Combine data frame with yield groupings, select unique variables --------------------------------------------------

wife_rate_yield <- cbind(wife_rate, groupings_yield) %>%
  select(unique(colnames(.)))

# Create bar graph with confidence intervals and letter denotation  -------------------------------------------------

ggplot(wife_rate_yield, aes(x = as.factor(Treatment_kgha), y = avg_yield)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_yield - ci_yield, 
                    ymax = avg_yield + ci_yield), width = 0.2) +
  geom_text(aes(label = letter, y = avg_yield + ci_yield + 0.05),
            position = position_dodge(0.9), vjust = 0) +
  labs(x = "N Application [kg/ha]", y = "Yield Mass [kg]") +
  theme_classic(base_size = 12, base_family = "TT Arial") +
  theme (axis.title.y = element_text (angle = 0, vjust = 1))

# Canopy area vs. N application figure =============================================================================

# Yield canopy ANOVA model and Duncan.test, assign groupings -------------------------------------------------------

canopy_aov <- aov(avg_canopy_p ~ Treatment_kgha, data = wife_plot)
print(summary(canopy_aov))

print(duncan.test(canopy_aov, "Treatment_kgha")$groups)

groupings_canopyarea <- data.frame(Treatment_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("a", "a", "a", "a", "a", "a"))

# Combine data frame with yield groupings, select unique variables -------------------------------------------------

wife_rate_canopy <- cbind(wife_rate, groupings_canopyarea) %>%
  select(unique(colnames(.)))

# Create yield bar graph with confidence intervals and letter denotation -------------------------------------------------

ggplot(wife_rate_canopy, aes(x = as.factor(Treatment_kgha), y = avg_canopy)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_canopy - ci_canopy, 
                    ymax = avg_canopy + ci_canopy), width = 0.2) +
  geom_text(aes(label = letter, y = avg_canopy + ci_canopy + 0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x = "N Application [kg/ha]", y = "Canopy Area [m2]") +
  theme_classic(base_size = 12, base_family = "TT Arial") +
  theme (axis.title.y = element_text (angle = 0, vjust = 1))

# NDVI reflectance values versus N application =====================================================================
# NDVI ANOVA model and Duncan.test, assign groupings ---------------------------------------------------------------

ndvi_aov <- aov(avg_ndvi_p ~ Treatment_kgha, data = wife_plot)
print(summary(ndvi_aov))

print(duncan.test(ndvi_aov, "Treatment_kgha")$groups)

groupings_ndvi <- data.frame(Treatment_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("a", "a", "a", "a", "a", "a"))

# Combine data frame with NDVI groupings, select unique variables -------------------------------------------------

wife_rate_ndvi <- cbind(wife_rate, groupings_ndvi) %>%
  select(unique(colnames(.)))

# Create NDVI bar graph with confidence intervals and letter denotation --------------------------------------------

ggplot(wife_rate_ndvi, aes(x = as.factor(Treatment_kgha), y = avg_ndvi)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_ndvi - ci_ndvi, 
                    ymax = avg_ndvi + ci_ndvi), width = 0.2) +
  geom_text(aes(label = letter, y = avg_ndvi + ci_ndvi + 0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x = "N Application [kg/ha]", y = "NDVI from Reflectances") +
  theme_classic(base_size = 12, base_family = "TT Arrial")+
  theme (axis.title.y = element_text (angle = 0, vjust = 1))

# GNDVI reflectance values vs. N application figure ================================================================

# GNDVI ANOVA model and Duncan.test, assign groupings --------------------------------------------------------------
gndvi_aov <- aov(avg_gndvi_p ~ Treatment_kgha, data = wife_plot)
print(summary(gndvi_aov))

print(duncan.test(gndvi_aov, "Treatment_kgha")$groups)

groupings_gndvi <- data.frame(Treatment_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("a", "a", "a", "a", "a", "a"))

# Combine data frame with GNDVI groupings, select unique variables -------------------------------------------------
wife_rate_gndvi <- cbind(wife_rate, groupings_gndvi) %>%
  select(unique(colnames(.)))

# Create GNDVI bar graph with confidence intervals and letter denotation -------------------------------------------

ggplot(wife_rate_gndvi, aes(x = as.factor(Treatment_kgha), y = avg_gndvi)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_gndvi - ci_gndvi, 
                    ymax = avg_gndvi + ci_gndvi), width = 0.2) +
  geom_text(aes(label = letter, y = avg_gndvi + ci_gndvi + 0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x = "N Application [kg/ha]", y = "GNDVI from Reflectances") +
  theme_classic(base_size = 12, base_family = "TT Arial") +
  theme (axis.title.y = element_text (angle = 0, vjust = 1))

# Yield ~ NDVI =====================================================================================================

# Test for homogenity of variances ---------------------------------------------------------------------------------
logreg = lm(log(wife_plot$avg_yield_p)~wife_plot$avg_ndvi_p)
predicted = predict.lm(logreg)
residuals = log(wife_plot$avg_yield_p) - predicted
plot(predicted, residuals)
abline(a = 0, b = 0, col = "red", lwd = 3, lty = "dashed")

# Test for normality -----------------------------------------------------------------------------------------------
reg = lm(wife_plot$avg_yield_p~wife_plot$avg_ndvi_p)
stdRes= rstandard(reg)
qqnorm(stdRes, ylab = "Standardized Results", xlab = "Theoretical Quantiles")
qqline(stdRes, col = 2, lwd = 2)

# Log transformation of yield data, fitting regression model -------------------------------------------------------
test_ndvi <- lm(log10(avg_yield_p)~avg_ndvi_p, data = wife_plot)
summary(test_ndvi)$r.squared # 0.200
coefficients(test_ndvi)

# Plot log transformed yield and NDVI values -----------------------------------------------------------------------
ggplot(wife_plot, aes(x = avg_ndvi_p, y = log10(avg_yield_p))) +
  geom_point() +
  geom_abline(intercept = summary(test_ndvi)$coeff[1,1],
              slope = summary(test_ndvi)$coeff[2,1]) +
  labs(x = "NDVI", y = "Log10( Plant Mass [kg] )") +
  theme_classic(base_size = 12, base_family = "TT Arial")

# Yield ~ GNDVI ====================================================================================================

# Log transformation of yield data, fitting regression model
test_gndvi <- lm(log10(avg_yield_p)~avg_gndvi_p, data = wife_plot)
summary(test_gndvi) #r.squared 0.183
coefficients(test_gndvi)

# Plot log transformed yield and GNDVI values ----------------------------------------------------------------------

ggplot(wife_plot, aes(x = avg_gndvi_p, y = log10(avg_yield_p))) +
  geom_point() +
  geom_abline(intercept = summary(test_gndvi)$coeff[1,1],
              slope = summary(test_gndvi)$coeff[2,1]) +
  labs(x = "GNDVI", y = "Log10( Plant Mass [kg] )") +
  theme_classic(base_size = 12, base_family = "TT Arial")

# Yield ~ Canopy ===================================================================================================

test_canopy <- lm(avg_yield_p~avg_canopy_p, data = wife_plot)
summary(test_canopy)$r.squared  # 0.16
coefficients(test_canopy)

# Plot log transformed yield and canopy area values ----------------------------------------------------------------

ggplot(wife_plot, aes(x = avg_canopy_p, y = avg_yield_p)) +
  geom_point() +
  geom_abline(intercept = summary(test_canopy)$coeff[1,1],
              slope = summary(test_canopy)$coeff[2,1]) +
  labs(x = "Canopy Area [m2]", y = "Plant Mass [kg]") +
  theme_classic(base_size = 12, base_family = "TT Arial")
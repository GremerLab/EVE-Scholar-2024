library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

germ_wk1 <- read.csv("../Input/Week1_Germ.csv") ## Read data for all weeks
germ_wk2 <- read_csv
germ_wk3 <- read_csv

# Merge all weeks into one dataset
dat <- 

# Check structure of data
str(germ_wk1)

# Remove first two rows
germ_wk1 <-  germ_wk1[-c(1,2),]

head(germ_wk1)

# Include first 13 columns
germ_wk1 <- data[, 1:13]

# Create column for data, germination percentage, and total count

dat_long <-  germ_wk1 %>%  
  pivot_longer(
    cols = starts_with("07"),
    names_to = "date", 
    values_to = "germination_count"
  )
dat_long$Date <- as.Date(dat_long$Date, format = "%Y-%m-%d")

head(dat_long)

# Summary statistics

summary(germ_wk1)

# Summary statistics by treatment and population

sum_treatment <-
  germ_wk1 %>% 
  group_by(Treatment) %>% 
  summarize(
    avg_germ_percentage = mean(Germ_percentage),
    SD_germ_percentage = mean(Germ_percentage),
    N = n()
  )

sum_population <-
  germ_wk1 %>% 
  group_by(Population) %>% 
  summarize(
    avg_germ_percentage = mean(Germ_percentage),
    SD_germ_percentage = mean(Germ_percentage),
    N = n()
  )
  
# Germination rate calculation by rep

# Calculate germination time, then reciprocal
  

## Visualizations

# Germination counts over time

# Germination percentage by treatment
ggplot(germ_wk1, aes(x = Treatment, y = Germ_percentage, fill = Treatment)) + 
  geom_boxplot() +
  labs(title = "Germination Percentage by Treatment", x = 
         "Treatment", y = "Germination Percentage")

# Germination percentage by population
ggplot(germ_wk1, aes(x = Population, y = Germ_percentage, fill = Population)) +
  geom_boxplot() +
  labs(title = "Germination Percentage by Population", x = 
         "Population", y = "Germination Percentage")

# Interaction of population and treatment
ggplot(germ_wk1, aes(x = Treatment, y = Germ_percentage, fill = Population)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  labs(title = "Combined Effects of Treatment and Population on Germination Percentage",
       x = "Treatment", y = "Germination Percentage")

## Statistical analysis

# ANOVA
model <- aov(Germ_percentage ~ Treatment * Population, data = germ_wk1)
summary(model)

# Regression
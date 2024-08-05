library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)

germ_wk1 <- read_csv("../Input/Week1_Germ.csv") ## Read data for all weeks
germ_wk2 <- read_csv
germ_wk3 <- read_csv

# Merge all weeks into one dataset
dat <- 

# Check structure of data
str(germ_wk1)

# Create column for data, germination percentage, and total count

# Create date and germ_count columns
dat_long <-  germ_wk1 %>%  
  pivot_longer(
    cols = starts_with("7"),
    names_to = "date", 
    values_to = "germination_count"
  )

# Create daily + weekly total count and germ_percent column
dat_long <-  dat_long %>% 
  group_by(Cell) %>% 
  mutate(total_germ = cumsum(germination_count)) 

dat_long <- dat_long %>%  
  mutate(germ_percent = (total_germ/Actual_count) * 100)

# Column for cumulative germination

dat_long <- dat_long %>% group_by(date, Pop, Treatment) %>% 
  mutate(avg_total = mean(total_germ))

head(dat_long)

str(dat_long)

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

# Week 1 Germination percentage by treatment

weekly_data <- dat_long %>% filter(date > "7/29/24")
head(weekly_data)

ggplot(weekly_data, aes(x = Treatment, y = germ_percent, fill = Treatment)) + 
  geom_boxplot() +
  labs(title = "Germination Percentage by Treatment", x = 
         "Treatment", y = "Germination Percentage")

# Germination percentage by population
ggplot(weekly_data, aes(x = Pop, y = germ_percent, fill = Pop)) +
  geom_boxplot() +
  labs(title = "Germination Percentage by Population", x = 
         "Population", y = "Germination Percentage")

# Interaction of population and treatment
ggplot(weekly_data, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Combined Effects of Treatment and Population on Germination Percentage",
       x = "Population", y = "Germination Percentage")

ggplot(weekly_data, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                  group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Germination Percentage")


ggplot(dat_long, mapping = aes(x = date, y = avg_total, color = as.factor(Treatment),
                                  group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 7) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Cumulative Germination")

## Statistical analysis

# ANOVA
model <- aov(Germ_percentage ~ Treatment * Population, data = germ_wk1)
summary(model)

# Regression
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)
library(germinationmetrics)

germ_wk1 <- read_csv("../Input/Week1_Germ.csv") ## Read data for all weeks
germ_wk2 <- read_csv("../Input/week2_germ.csv")
germ_wk3 <- read_csv("../Input/Week3_Germ.csv")

# Merge all weeks into one dataset
dat <- 

# Check structure of data
str(germ_wk1)
str(germ_wk2)
str(germ_wk3)

# Create column for data, germination percentage, and total count

# Create date and germ_count columns
dat_long <-  germ_wk1 %>%  
  pivot_longer(
    cols = starts_with("7"),
    names_to = "date", 
    values_to = "germination_count"
  )
dat_long2 <-  germ_wk2 %>%  
  pivot_longer(
    cols = matches("^(7|8)"),
    names_to = "date", 
    values_to = "germination_count"
  )
dat_long3 <-  germ_wk3 %>%  
  pivot_longer(
    cols = starts_with("8"),
    names_to = "date", 
    values_to = "germination_count"
  )
# Create daily + weekly total count and germ_percent column
dat_long <-  dat_long %>% 
  group_by(Cell) %>% 
  mutate(total_germ = cumsum(germination_count)) 

dat_long <- dat_long %>%  
  mutate(germ_percent = (total_germ/Actual_count) * 100)

dat_long3 <-  dat_long3 %>% 
  group_by(Cell) %>% 
  mutate(total_germ = cumsum(germination_count)) 

dat_long3 <- dat_long3 %>%  
  mutate(germ_percent = (total_germ/Actual_count) * 100)

# Add Actual_count to dat_long2
dat_long2$Actual_count <- dat_long$Actual_count

dat_long2 <-  dat_long2 %>% 
  group_by(Cell) %>% 
  mutate(total_germ = cumsum(germination_count)) 

dat_long2 <- dat_long2 %>%  
  mutate(germ_percent = (total_germ/Actual_count) * 100)

# Column for cumulative germination

dat_long <- dat_long %>% group_by(date, Pop, Treatment) %>% 
  mutate(avg_total = mean(total_germ))

dat_long2 <- dat_long2 %>% group_by(date, Pop, Treatment) %>% 
  mutate(avg_total = mean(total_germ))

dat_long3 <- dat_long3 %>% group_by(date, Pop, Treatment) %>% 
  mutate(avg_total = mean(total_germ))


# Summary statistics

summary(germ_wk1)

summary(germ_wk2)

summary(germ_wk3)

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
  
## Visualizations

# Germination counts over time

# Week 1 and 2 Germination percentage by treatment

weekly_data <- dat_long %>% filter(date > "7/29/24")
head(weekly_data)

weekly_data2 <- dat_long2 %>% filter(date > "8/5/24")
head(weekly_data2)

ggplot(weekly_data, aes(x = Treatment, y = germ_percent, fill = Treatment)) + 
  geom_boxplot() +
  labs(title = "Week 1 Germination Percentage by Treatment", x = 
         "Treatment", y = "Germination Percentage")

ggplot(weekly_data2, aes(x = Treatment, y = germ_percent, fill = Treatment)) + 
  geom_boxplot() +
  labs(title = "Week 2 Germination Percentage by Treatment", x = 
         "Treatment", y = "Germination Percentage")

# Germination percentage by population
ggplot(weekly_data, aes(x = Pop, y = germ_percent, fill = Pop)) +
  geom_boxplot() +
  labs(title = "Week 1 Germination Percentage by Population", x = 
         "Population", y = "Germination Percentage")

ggplot(weekly_data2, aes(x = Pop, y = germ_percent, fill = Pop)) +
  geom_boxplot() +
  labs(title = "Week 2 Germination Percentage by Population", x = 
         "Population", y = "Germination Percentage")

# Interaction of population and treatment
ggplot(weekly_data, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Week 1: Combined Effects of Treatment and Population on Germination Percentage",
       x = "Population", y = "Germination Percentage")

ggplot(weekly_data, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Week 2: Combined Effects of Treatment and Population on Germination Percentage",
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

# Calculate days to germinate, using days since start date
dat_long$date <- as.Date(dat_long$date, format = "%m/%d/%Y")

start_date <- as.Date("2024-07-23")

dat_long$days_to_germ <- as.numeric(dat_long$date - start_date) 

# Germination rate calculation by cell

# Calculate germination rate
dat_long <- dat_long %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

ggplot(dat_long, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Germination Rate")
  

ggplot(dat_long, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                                  group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Cumulative Germination")

dat_long2$date <- as.Date(dat_long$date, format = "%m/%d/%Y")

start_date2 <- as.Date("2024-07-31")

dat_long2$days_to_germ <- as.numeric(dat_long$date - start_date) 

ggplot(dat_long2, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 2 Cumulative Germination")

# Joined graphs for week 1 and 2



## Statistical analysis

# ANOVA
model <- aov(Germ_percentage ~ Treatment * Population, data = germ_wk1)
summary(model)

# Regression
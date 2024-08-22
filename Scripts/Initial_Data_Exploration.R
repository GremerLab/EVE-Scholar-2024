library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)
library(purrr)

# Add separate datasets with cumulative values after each amount weeks

germ_wk1 <- read_csv("./Input/Week1_Germ.csv") ## Read data for all weeks
germ_wk2 <- read_csv("./Input/week2_germ.csv")
germ_wk3 <- read_csv("./Input/Week3_Germ.csv")

# Check structure of data
str(germ_wk1)
str(germ_wk2)
str(germ_wk3)

# Add Actual_count to germ_wk2

germ_wk2$Actual_count <- germ_wk1$Actual_count

# Create column for data, germination percentage, and total count

# Create date and germ_count columns
dat_list <- list(germ_wk1, germ_wk2, germ_wk3)

prefix_list <- list("7", c("7", "8"), "8")

dat_long_list <- map2(dat_list, prefix_list, ~ .x %>% 
                        pivot_longer(
                          cols = starts_with(.y),
                          names_to = "date", 
                          values_to = "germination_count"
                        ))


# Create daily + weekly total count and germ_percent column

dat_long_list <- dat_long_list %>% 
  map(~ .x %>% 
        group_by(Cell) %>% 
        mutate(total_germ = cumsum(germination_count)) %>%
        ungroup() %>%
        mutate(germ_percent = (total_germ / Actual_count) * 100)
  )


# Column for cumulative germination

dat_long_list <- dat_long_list %>% 
  map(~ .x %>% 
        group_by(date, Pop, Treatment) %>% 
        mutate(avg_total = mean(total_germ)))

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

# Germination percentage by treatment

# Extract each dataset
germ_wk1 <- dat_long_list[[1]]
germ_wk2 <- dat_long_list[[2]]
germ_wk3 <- dat_long_list[[3]]

### Temporal analysis of germination checks

# Apply filtering by end of week date
germ_wk1_weekly <- germ_wk1 %>% filter(date > "7/29/24")
germ_wk2_weekly <- germ_wk2 %>% filter(date > "8/5/24")
germ_wk3_weekly <- germ_wk3 %>% filter(date > "8/12/24")

germ_filtered_list <- list(germ_wk1_weekly, germ_wk2_weekly, germ_wk3_weekly)

week_labels <- c("Week 1", "Week 2", "Week 3")

germ_filtered_list <- map2(germ_filtered_list, week_labels, ~ .x %>%
                             mutate(week = .y))

germ_filtered_list %>% 
  map(~ ggplot(.x, aes(x = Treatment, y = germ_percent, group = Treatment, color = Treatment)) +
        geom_boxplot() +
        labs(title = paste("Germination Percentage By Treatment", unique(.x$week)), x = "Treatment", y = "Germination Percentage"))

# Germination percentage by population

germ_filtered_list %>% 
  map(~ ggplot(.x, aes(x = Pop, y = germ_percent, group = Pop, color = Pop)) +
        geom_boxplot() +
        labs(title = paste("Germination Percentage By Population", unique(.x$week)), x = "Population", y = "Germination Percentage"))

# Interaction of population and treatment
ggplot(germ_wk1_weekly, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Week 1: Combined Effects of Treatment and Population on Germination Percentage",
       x = "Population", y = "Germination Percentage")

ggplot(germ_wk2_weekly, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Week 2: Combined Effects of Treatment and Population on Germination Percentage",
       x = "Population", y = "Germination Percentage")

ggplot(germ_wk3_weekly, aes(x = Pop, y = germ_percent, color = Treatment)) +
  geom_boxplot() +
  labs(title = "Week 3: Combined Effects of Treatment and Population on Germination Percentage",
       x = "Population", y = "Germination Percentage")


ggplot(germ_wk1_weekly, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                  group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Germination Percentage")

ggplot(germ_wk2_weekly, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                      group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Week 2 Germination Percentage")

ggplot(germ_wk3_weekly, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                      group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Week 3 Germination Percentage")

# Calculate days to germinate, using days since start date
germ_wk1$date <- as.Date(germ_wk1$date, format = "%m/%d/%Y")

start_date1 <- as.Date("2024-07-23")

germ_wk1$days_to_germ <- as.numeric(germ_wk1$date - start_date1) 

germ_wk2$date <- as.Date(germ_wk2$date, format = "%m/%d/%Y")

start_date2 <- as.Date("2024-07-31")

germ_wk2$days_to_germ <- as.numeric(germ_wk2$date - start_date2) 

germ_wk3$date <- as.Date(germ_wk3$date, format = "%m/%d/%Y")

start_date3 <- as.Date("2024-08-10")

germ_wk3$days_to_germ <- as.numeric(germ_wk3$date - start_date3) 

# Germination rate calculation by cell

# Calculate germination rate
germ_wk1 <- germ_wk1 %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

germ_wk2 <- germ_wk2 %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

germ_wk3 <- germ_wk3 %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

# Temporal germination rate and cumulative germination graphs

# Week 1

ggplot(germ_wk1, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Germination Rate")

ggplot(germ_wk1, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                                  group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 1 Cumulative Germination")

# Week 2

ggplot(germ_wk2, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                               group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Week 2 Germination Rate")

ggplot(germ_wk2, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 2 Cumulative Germination")

# Week 3

ggplot(germ_wk3, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                               group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Week 3 Germination Rate")

ggplot(germ_wk3, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 3 Cumulative Germination")

### Cumulative analysis on germination checks

# Merge weeks

merged_week_list <- list(germ_wk1, germ_wk2, germ_wk3)
combined_merged_data <- bind_rows(merged_week_list)

wk1_end_date <- "2024-07-30"
wk2_end_date <- "2024-08-06"
wk3_end_date <- "2024-08-14"

# Cumulative germination counts, rate, total germ after merging after weeks

cumulative_2_weeks <- combined_merged_data %>%
  filter(date <= wk2_end_date) %>%
  group_by(Pop, Treatment, Cell, Tray) %>%  # Grouping by relevant columns
  mutate(cumulative_germ_count = cumsum(germination_count)) %>%
  mutate(cumulative_germ_rate = cumsum(avg_total)) %>% 
  mutate(germ_percent = (cumulative_germ_count / Actual_count) * 100) %>% 
  ungroup()

cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(days_to_germ = as.numeric(date - start_date1))

cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(week = case_when(
    date <= wk1_end_date ~ "Week 1",
    date > wk1_end_date & date <= wk2_end_date ~ "Week 2"
  ))

cumulative_2_weeks <- cumulative_2_weeks %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

cumulative_3_weeks <- combined_merged_data %>%
  filter(date <= wk3_end_date) %>%
  group_by(Pop, Treatment, Cell, Tray) %>%  # Grouping by relevant columns
  mutate(cumulative_germ_count = cumsum(germination_count)) %>%
  mutate(cumulative_germ_rate = cumsum(avg_total)) %>% 
  mutate(germ_percent = (cumulative_germ_count / Actual_count) * 100) %>% 
  ungroup()

cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(days_to_germ = as.numeric(date - start_date1))

cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(week = case_when(
    date <= wk1_end_date ~ "Week 1",
    date > wk1_end_date & date <= wk2_end_date ~ "Week 2",
    date > wk2_end_date & date <= wk3_end_date ~ "Week 3"
  ))

cumulative_3_weeks <- cumulative_3_weeks %>% 
  group_by(Cell) %>% 
  mutate(
    germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)
  )

# Graphs after 2 and 3 weeks

ggplot(cumulative_2_weeks, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                         group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 2 Weeks")

ggplot(cumulative_2_weeks, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                                         group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 2 Weeks")

ggplot(cumulative_2_weeks, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                         group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Percentage After 2 Weeks")



ggplot(cumulative_3_weeks, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                               group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 3 Weeks")

ggplot(cumulative_3_weeks, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 3 Weeks")

ggplot(cumulative_3_weeks, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                      group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Percentage After 3 Weeks")

## Statistical analysis

# Regression

# Create column for germination_success, with binary values
germ_wk1 <- germ_wk1 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

germ_wk2 <- germ_wk2 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

germ_wk3 <- germ_wk3 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

glm_wk1 <- glm(germination_success ~ Pop * Treatment,
                   data = germ_wk1, 
                   family = binomial())

summary(glm_wk1)


glm_wk2 <- glm(germination_success ~ Pop * Treatment,
    data = germ_wk2, 
    family = binomial())

summary(glm_wk2)


glm_wk3 <- glm(germination_success ~ Pop * Treatment,
    data = germ_wk3, 
    family = binomial())

summary(glm_wk3)


glm_combined_wk2 <- glm(germination_success ~ Pop * Treatment,
    data = cumulative_2_weeks, 
    family = binomial())

summary(glm_combined)

glm_combined_wk3 <- glm(germination_success ~ Pop * Treatment,
      data = cumulative_3_weeks, 
      family = binomial())

summary(glm_combined)

# GLMs

glm_germ_rate <- glm(germination_rate ~ Pop * Treatment, 
                     data = cumulative_3_weeks, 
                     family = gaussian())

summary(glm_germ_rate)

glm_germ_percent <- glm(germ_percent ~ Pop * Treatment, 
                        data = cumulative_3_weeks, 
                        family = gaussian())

summary(glm_germ_percent)

glm_germ_count <- glm(germination_count ~ Pop * Treatment, 
                      data = cumulative_3_weeks, 
                      family = poisson())

summary(glm_germ_count)

# ANOVA

# Shapiro test

# Levene test

# Transformation

anova(glm_wk1, test = "Chisq")

anova(glm_wk2, test = "Chisq")

anova(glm_wk3, test = "Chisq")

anova(glm_combined_wk2, test = "Chisq")

anova(glm_combined_wk3, test = "Chisq")


# Predicted values for GLM and logistic

cumulative_3_weeks$predicted_germ_rate <- predict(glm_germ_rate, type = "response")

cumulative_3_weeks$predicted_germ_success <- predict(glm_combined, type = "response")

# Plots



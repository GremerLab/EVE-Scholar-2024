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
germ_wk4 <- read_csv("./Input/week4_germ.csv")

# Check structure of data
str(germ_wk1)
str(germ_wk2)
str(germ_wk3)
str(germ_wk4)

# Add Actual_count to germ_wk2

germ_wk2$Actual_count <- germ_wk1$Actual_count

# Create column for data, germination percentage, and total count

# Create date and germ_count columns
dat_list <- list(germ_wk1, germ_wk2, germ_wk3, germ_wk4)

prefix_list <- list("7", c("7", "8"), "8", c("8", "9"))

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

summary(germ_wk4)

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
germ_wk4 <- dat_long_list[[4]]

### Temporal analysis of germination checks

# Apply filtering by end of week date
germ_wk1_weekly <- germ_wk1 %>% filter(date > "7/29/24")
germ_wk2_weekly <- germ_wk2 %>% filter(date > "8/5/24")
germ_wk3_weekly <- germ_wk3 %>% filter(date > "8/12/24")
germ_wk4_weekly <- germ_wk4 %>% filter(date > "8/30/24")

germ_filtered_list <- list(germ_wk1_weekly, germ_wk2_weekly, germ_wk3_weekly, germ_wk4_weekly)

week_labels <- c("Week 1", "Week 2", "Week 3", "Week 4")

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

germ_wk4$date <- as.Date(germ_wk4$date, format = "%m/%d/%Y")

start_date4 <- as.Date("2024-08-17")

germ_wk4$days_to_germ <- as.numeric(germ_wk4$date - start_date4) 

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

germ_wk4 <- germ_wk4 %>% 
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

# Week 4

ggplot(germ_wk4, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                               group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Population") +
  labs(color = "Treatment") +
  ggtitle("Week 4 Germination Rate")

ggplot(germ_wk3, mapping = aes(x = days_to_germ, y = avg_total, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Week 4 Cumulative Germination")

### Cumulative analysis on germination checks

# Merge weeks

merged_week_list <- list(germ_wk1, germ_wk2, germ_wk3, germ_wk4)
combined_merged_data <- bind_rows(merged_week_list)

wk1_end_date <- "2024-07-30"
wk2_end_date <- "2024-08-06"
wk3_end_date <- "2024-08-14"
wk4_end_date <- "2024-09-03"

# Cumulative germination counts, rate, total germ after merging after weeks
# Transform proportions on logit scale, take mean and standard dev/error, retransform for graphing

# Calculate variables for each row
cumulative_2_weeks <- combined_merged_data %>%
  filter(date <= wk2_end_date) %>%
  mutate(days_to_germ = as.numeric(date - start_date1)) %>% 
  mutate(cumulative_germ_count = cumsum(germination_count)) %>%
  mutate(germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
    germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                              0, germination_rate)) %>% 
  mutate(cumulative_germ_percent = (cumulative_germ_count / Actual_count) * 100) %>% 
  ungroup()

cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(week = case_when(
    date <= wk1_end_date ~ "Week 1",
    date > wk1_end_date & date <= wk2_end_date ~ "Week 2"
  ))

# Summaries for cumulative germination group by day, pop, treatment

cumulative_2wk_avg_germ <- cumulative_2_weeks %>% 
  group_by(Pop, Treatment, date) %>%  # Grouping by relevant columns
  summarise(cumulative_germ_avg = mean(cumulative_germ_count)) %>% 
  ungroup()

# Summaries for average germination rate for every pop/treatment
cumulative_2wk_avg_germ_rate <- cumulative_2_weeks %>% 
  group_by(Pop, Treatment) %>%
  summarise(germination_rate_avg = mean(germination_rate)) %>% 
           ungroup()

cumulative_3_weeks <- combined_merged_data %>%
  filter(date <= wk3_end_date) %>%
  mutate(days_to_germ = as.numeric(date - start_date1)) %>% 
  mutate(cumulative_germ_count = cumsum(germination_count)) %>%
  mutate(germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
         germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                                   0, germination_rate)) %>% 
  mutate(cumulative_germ_percent = (cumulative_germ_count / Actual_count) * 100) %>% 
  ungroup()


cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(week = case_when(
    date <= wk1_end_date ~ "Week 1",
    date > wk1_end_date & date <= wk2_end_date ~ "Week 2",
    date > wk2_end_date & date <= wk3_end_date ~ "Week 3"
  ))

cumulative_3wk_avg_germ <- cumulative_3_weeks %>% 
  group_by(Pop, Treatment, date) %>%  # Grouping by relevant columns
  summarise(cumulative_germ_avg = mean(cumulative_germ_count)) %>% 
  ungroup()

cumulative_3wk_avg_germ_rate <- cumulative_3_weeks %>% 
  group_by(Pop, Treatment) %>%
  summarise(germination_rate_avg = mean(germination_rate)) %>% 
  ungroup()

cumulative_4_weeks <- combined_merged_data %>%
  filter(date <= wk4_end_date) %>%
  mutate(days_to_germ = as.numeric(date - start_date1)) %>% 
  mutate(cumulative_germ_count = cumsum(germination_count)) %>%
  mutate(germination_rate = sum(germination_count)/sum(germination_count * days_to_germ),
         germination_rate = ifelse(is.nan(germination_rate) | is.infinite(germination_rate), 
                                   0, germination_rate)) %>% 
  mutate(cumulative_germ_percent = (cumulative_germ_count / Actual_count) * 100) %>% 
  ungroup()

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(week = case_when(
    date <= wk1_end_date ~ "Week 1",
    date > wk1_end_date & date <= wk2_end_date ~ "Week 2",
    date > wk2_end_date & date <= wk3_end_date ~ "Week 3",
    date > wk3_end_date & date <= wk4_end_date ~ "Week 4"
  ))

cumulative_4wk_avg_germ <- cumulative_4_weeks %>% 
  group_by(Pop, Treatment, date) %>%  # Grouping by relevant columns
  summarise(cumulative_germ_avg = mean(cumulative_germ_count)) %>% 
  ungroup()

cumulative_4wk_avg_germ_rate <- cumulative_4_weeks %>% 
  group_by(Pop, Treatment) %>%
  summarise(germination_rate_avg = mean(germination_rate)) %>% 
  ungroup()

# Add elevation data, organize graphs that way
elevation_data <- read.csv("./Input/elevation_data.csv")

# Add index column to keep original order before merging
cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(Index = row_number())

cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(Index = row_number())

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Index = row_number())

cumulative_2_weeks <- merge(cumulative_2_weeks, elevation_data, by = "Pop")

cumulative_2_weeks <- cumulative_2_weeks %>% 
  arrange(Index) 

cumulative_3_weeks <- merge(cumulative_3_weeks, elevation_data, by = "Pop")

cumulative_3_weeks <- cumulative_3_weeks %>% 
  arrange(Index) 

cumulative_4_weeks <- merge(cumulative_4_weeks, elevation_data, by = "Pop")

cumulative_4_weeks <- cumulative_4_weeks %>% 
  arrange(Index) 

# Add fire history data
# Visualization of sites through GIS, identify if sites with more fire experience higher germination under fire cues
# Potentially overlay distribution of germinants of each pop/site with fire history? 

fire_data <-

stto_distribution <- 


# Graphs after 2 and 3 weeks

ggplot(cumulative_2_weeks, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                         group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 2 Weeks")

ggplot(cumulative_2wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
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
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 3 Weeks")

ggplot(cumulative_3wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
                               group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 3 Weeks")

# Can try facet_wrap with elevation, can try out sort_by method

ggplot(cumulative_3_weeks, mapping = aes(x = Treatment, y = germ_percent, color = as.factor(Treatment),
                                      group = as.factor(Treatment))) +
  geom_boxplot() +
  ylim(0, 100) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination percentage") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Percentage After 3 Weeks")

ggplot(cumulative_4_weeks, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                         group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Germination rate") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 4 Weeks")

ggplot(cumulative_4wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
                                              group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 4 Weeks")

## Statistical analysis

## Grouped logistic regression
# Germ_count = event, Actual_count = trials

#cbind(total_germ, Actual_count) ~ Pop * Treatment

# Create dataframe with a total column to use for logistic regression, group by Pop, Treatment, Elev, Cell
# Two ways:
# Use summarise function of germination_count, not mutate, for total column
# Summarise max of cumulative germ count
# Check if they have same values
# Keep actual count for each cell
# Data points should be reduced 

reg_2wk_cumulative <- cumulative_2_weeks %>% 
  group_by(Actual_count, Pop, Treatment, Elev, Cell) %>%  # Grouping by relevant columns
  summarise(total_cumulative_germ = max(cumulative_germ_count)) %>% 
  ungroup()

reg_3wk_cumulative <- cumulative_3_weeks %>% 
  group_by(Actual_count, Pop, Treatment, Elev, Cell) %>%  # Grouping by relevant columns
  summarise(total_cumulative_germ = max(cumulative_germ_count)) %>% 
  ungroup()

reg_4wk_cumulative <- cumulative_3_weeks %>% 
  group_by(Actual_count, Pop, Treatment, Elev, Cell) %>%  # Grouping by relevant columns
  summarise(total_cumulative_germ = max(cumulative_germ_count)) %>% 
  ungroup()

# Create column for germination_success, with binary values
germ_wk1 <- germ_wk1 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

germ_wk2 <- germ_wk2 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

germ_wk3 <- germ_wk3 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

germ_wk4 <- germ_wk4 %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

cumulative_2_weeks <- cumulative_2_weeks %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

cumulative_3_weeks <- cumulative_3_weeks %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(germination_success = ifelse(germination_count > 0, 1, 0))

# Regression analysis 
# These variables are used for ANOVA, which will do statistical analysis
glm_wk1 <- glm(cbind(total_germ, Actual_count) ~ Pop * Treatment,
                   data = germ_wk1, 
                   family = binomial())

summary(glm_wk1)


glm_wk2 <- glm(cbind(total_germ, Actual_count) ~ Pop * Treatment,
    data = germ_wk2, 
    family = binomial())

summary(glm_wk2)


glm_wk3 <- glm(cbind(total_germ, Actual_count) ~ Pop * Treatment,
    data = germ_wk3, 
    family = binomial())

summary(glm_wk3)

glm_wk4<- glm(cbind(total_germ, Actual_count) ~ Pop * Treatment,
               data = germ_wk4, 
               family = binomial())

summary(glm_wk4)


glm_combined_wk2 <- glm(cbind(total_cumulative_germ, Actual_count) ~ Pop * Treatment,
    data = reg_2wk_cumulative, 
    family = binomial())

glm_combined_wk2_elev <- glm(cbind(total_cumulative_germ, Actual_count) ~ Elev * Treatment,
                        data = reg_2wk_cumulative, 
                        family = binomial())

summary(glm_combined_wk2)
summary(glm_combined_wk2_elev)

glm_combined_wk3 <- glm(cbind(total_cumulative_germ, Actual_count) ~ Pop * Treatment,
      data = reg_3wk_cumulative, 
      family = binomial())

glm_combined_wk3_elev <- glm(cbind(total_cumulative_germ, Actual_count) ~ Elev * Treatment,
                        data = reg_3wk_cumulative, 
                        family = binomial())

summary(glm_combined_wk3)
summary(glm_combined_wk3_elev)


glm_combined_wk4 <- glm(cbind(total_cumulative_germ, Actual_count) ~ Pop * Treatment,
                        data = reg_4wk_cumulative, 
                        family = binomial())

glm_combined_wk4_elev <- glm(cbind(total_cumulative_germ, Actual_count) ~ Elev * Treatment,
                        data = reg_4wk_cumulative, 
                        family = binomial())

summary(glm_combined_wk4)
summary(glm_combined_wk4_elev)

# GLMs

# Reduce dataframe so that each cell, on the last day of the third week, has one value

glm_germ_rate3_elev <- glm(germination_rate ~ Elev * Treatment, 
                      data = cumulative_3_weeks, 
                      family = gaussian())

summary(glm_germ_rate3_elev)

glm_germ_rate3 <- glm(germination_rate ~ Pop * Treatment, 
                     data = cumulative_3_weeks, 
                     family = gaussian())

summary(glm_germ_rate3)



glm_germ_rate4 <- glm(germination_rate ~ Pop * Treatment, 
                     data = cumulative_4_weeks, 
                     family = gaussian())

summary(glm_germ_rate4)

glm_germ_percent <- glm(germ_percent ~ Pop * Treatment, 
                        data = cumulative_4_weeks, 
                        family = gaussian())

summary(glm_germ_percent)

glm_germ_count <- glm(germination_count ~ Pop * Treatment, 
                      data = cumulative_4_weeks, 
                      family = poisson())

summary(glm_germ_count)

# ANOVA

# Shapiro test

# Levene test

anova(glm_wk1, test = "Chisq")

anova(glm_wk2, test = "Chisq")

anova(glm_wk3, test = "Chisq")

anova(glm_combined_wk2, test = "Chisq")

anova(glm_combined_wk3, test = "Chisq")

anova(glm_combined_wk4, test = "Chisq")

anova(glm_combined_wk2_elev, test = "Chisq")

anova(glm_combined_wk3_elev, test = "Chisq")

anova(glm_combined_wk4_elev, test = "Chisq")

anova(glm_germ_rate3, test = "Chisq")

anova(glm_germ_rate3_elev, test = "Chisq")

anova(glm_germ_rate4, test = "Chisq")


# Add elevation to replace pop, group pops by elevation

ggplot(cumulative_2_weeks, mapping = aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                                         group = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(.~Elev, ncol = 4) +
  ylab("Germination rate") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 2 Weeks")

ggplot(cumulative_2wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
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
  xlab("Treatment") +
  labs(color = "Treatment") +
  ggtitle("Germination Rate After 3 Weeks")

ggplot(cumulative_3wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
                                              group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 3 Weeks")

# Can try facet_wrap with elevation, can try out sort_by method
# Round elev, add additional title for each box

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Elev = round(Elev, 0))  # Round to 0 decimal places, adjust if you want decimals

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Elev_Pop = paste0("Elevation: ", Elev, "\nPop: ", Pop)) %>%
  arrange(Elev)  # Ensure data is arranged by the rounded Elev value

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Elev_Pop = factor(Elev_Pop, levels = unique(Elev_Pop)))

ggplot(cumulative_4_weeks, aes(x = Treatment, y = germination_rate, color = as.factor(Treatment), 
                               group = as.factor(Treatment))) + 
  geom_boxplot() + 
  facet_wrap(.~Elev_Pop, ncol = 4, scales = "free") +  
  ylab("Germination rate") + 
  xlab("Treatment") + 
  labs(color = "Treatment") + 
  ggtitle("Germination Rate After 4 Weeks")

cumulative_4wk_avg_germ <- cumulative_4wk_avg_germ %>%
  left_join(cumulative_4_weeks %>% select(Pop, Elev), by = "Pop")

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Elev = round(Elev, 0))  

cumulative_4wk_avg_germ  <- cumulative_4wk_avg_germ  %>%
  mutate(Elev_Pop = paste0("Elevation: ", Elev, "\nPop: ", Pop)) %>%
  arrange(Elev)  

cumulative_4wk_avg_germ  <- cumulative_4wk_avg_germ  %>%
  mutate(Elev_Pop = factor(Elev_Pop, levels = unique(Elev_Pop)))

ggplot(cumulative_4wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment), 
                                         group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  facet_wrap(.~Elev_Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Date") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 4 Weeks")

# Elev vs. Prop of germination, shapes/colors would be different treatment - regression + inverse logit

ggplot(cumulative_4wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Treatment),
                                              group = as.factor(Treatment))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Pop, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Treatment") +
  ggtitle("Cumulative Germination After 4 Weeks")



ggplot(cumulative_4wk_avg_germ, mapping = aes(x = date, y = cumulative_germ_avg, color = as.factor(Pop),
                                              group = as.factor(Pop))) +
  geom_point() +
  geom_line() +
  ylim(0, 6) +
  facet_wrap(.~Treatment, ncol = 4) +
  ylab("Cumulative germination") +
  xlab("Time (days)") +
  labs(color = "Pop") +
  ggtitle("Cumulative Germination After 4 Weeks")

# Predicted values for GLM and logistic

cumulative_3_weeks$predicted_germ_rate <- predict(glm_germ_rate, type = "response")

cumulative_3_weeks$predicted_germ_success <- predict(glm_combined_wk3, type = "response")

cumulative_4_weeks$predicted_germ_rate <- predict(glm_germ_rate4, type = "response")

cumulative_4_weeks$predicted_germ_success <- predict(glm_combined_wk4, type = "response")

# Can try facet_wrap with elevation, can try out sort_by method

cumulative_4_weeks$Elev_Level <- cut(cumulative_4_weeks$Elev,
                                     breaks = c(-Inf, 786, 1612, 2800),  # Your specified range limits
                                     labels = c("Low", "Medium", "High"))


# Elev vs Total proportion of germination at the end of week 4, colored by treatment using regression model + geom_point of original data  
cumulative_4_weeks_new <- cumulative_4_weeks %>% 
  filter(date == wk4_end_date) %>% 
  mutate(total_germ_prop = total_germ / Actual_count)

reg_total_prop_wk4 <- lm(total_germ_prop ~ Elev + Treatment, data = cumulative_4_weeks_new)

cumulative_4_weeks_new$predicted_total_germ_prop <- predict(reg_total_prop_wk4)
cumulative_4_weeks_new$predicted_total_germ_prob <- plogis(cumulative_4_weeks_new$predicted_total_germ_prop)

ggplot(cumulative_4_weeks_new, aes(x = Elev, y = total_germ_prop, color = Treatment)) +
  geom_point() +
  geom_line(aes(y = predicted_total_germ_prop), linetype = "dashed") +
  ggtitle("Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion")

avg_germination <- cumulative_4_weeks_new %>%
  filter(date == wk4_end_date) %>%  # Make sure to filter for the end of week 4
  group_by(Elev, Treatment, Pop) %>%
  summarize(avg_total_germ_prop = mean(total_germ_prop, na.rm = TRUE), .groups = 'drop')  # Calculate average

reg_total_prop_wk4_avg <- lm(avg_total_germ_prop ~ Elev + Treatment, data = avg_germination)

avg_germination$predicted_total_germ_prop <- predict(reg_total_prop_wk4_avg)
avg_germination$predicted_total_germ_prop <- pmax(0, pmin(1, avg_germination$predicted_total_germ_prop))


ggplot(avg_germination, aes(x = Elev, y = avg_total_germ_prop, color = Treatment)) +
  geom_point() +
  geom_line(aes(y = predicted_total_germ_prop, color = Treatment)) +
  ggtitle("Average Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion") 

ggplot(avg_germination, aes(x = Elev, y = avg_total_germ_prop, color = Treatment)) +
  geom_point(size = 2) +  # Scatter plot of averages
  geom_line(aes(y = predicted_total_germ_prop, color = Treatment)) +  
  xlab("Elevation") + 
  ggtitle("Average Total Germination Proportion by Elevation at Week 4") +
  theme_minimal()
  
str(reg_total_prop_wk4)

## Reverse logit transformation

reg_total_prop_wk4_avg <- glm(avg_total_germ_prop ~ Elev * Treatment, family = binomial(link = "logit"), data = avg_germination)

predictions <- predict(reg_total_prop_wk4_avg, newdata = avg_germination, type = "link", se.fit = TRUE)

# Back-transform predictions and SEs
avg_germination$predicted_total_germ_prop <- plogis(predictions$fit)  # back-transform mean predictions
avg_germination$se <- predictions$se.fit  # SE on the logit scale
avg_germination$lower <- plogis(predictions$fit - predictions$se.fit)  # back-transform lower bound
avg_germination$upper <- plogis(predictions$fit + predictions$se.fit)  # back-transform upper bound

ggplot(avg_germination, aes(x = Elev, y = avg_total_germ_prop, color = Treatment)) +
  geom_point(size = 2.5) +
  geom_line(aes(y = predicted_total_germ_prop, color = Treatment)) +
  ggtitle("Average Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion") +
  scale_y_continuous(limits = c(0, 0.45))


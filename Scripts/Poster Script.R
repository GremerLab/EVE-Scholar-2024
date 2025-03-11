library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)
library(purrr)
library(car)

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

### Cumulative analysis on germination checks

# Merge weeks

merged_week_list <- list(germ_wk1, germ_wk2, germ_wk3, germ_wk4)
combined_merged_data <- bind_rows(merged_week_list)

wk1_end_date <- "2024-07-30"
wk2_end_date <- "2024-08-06"
wk3_end_date <- "2024-08-14"
wk4_end_date <- "2024-09-03"

# Only use this dataframe
# Start script just for poster
cumulative_4_weeks <- combined_merged_data %>%
  group_by(Cell) %>%
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

# Add fire data
fire_data <- read.csv("./Input/fire_history.csv")

cumulative_4_weeks <- merge(cumulative_4_weeks, elevation_data, by = "Pop")

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(Index = row_number())

cumulative_4_weeks <- cumulative_4_weeks %>% 
  arrange(Index) 

View(cumulative_4_weeks)

cumulative_4_weeks <- cumulative_4_weeks %>% 
  arrange(FireNum) 

cumulative_4_weeks <- cumulative_4_weeks %>% 
  mutate(total_germ_prop = total_germ / Actual_count)

# Elevation breaks
cumulative_4_weeks$Elev_Level <- cut(cumulative_4_weeks$Elev,
                                     breaks = c(-Inf, 786, 1612, 2800),  # Your specified range limits
                                     labels = c("Low", "Medium", "High"))

# Fire history data merge

cumulative_4_weeks <- merge(cumulative_4_weeks, fire_data, by = "Pop")

colnames(cumulative_4_weeks)

## Grouped logistic regression


cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(total_germ_prop = total_germ / Actual_count) %>%
  mutate(max_germ_prop = max(total_germ_prop))

cumulative_4_weeks$max_germ_prop <- pmin(cumulative_4_weeks$max_germ_prop, 1)

View(cumulative_4_weeks)

# GLM
# Use max of cumulative germ
# Plot: coefficients for glm on a logit scale, plot germination proportions without transformation

glm_combined_wk4 <- glm(cbind(cumulative_germ_count, Actual_count) ~ Pop * Treatment,
                        data = cumulative_4_weeks, 
                        family = binomial())

glm_combined_wk4_elev <- glm(cbind(cumulative_germ_count, Actual_count) ~ Elev * Treatment,
                             data = cumulative_4_weeks, 
                             family = binomial())

summary(glm_combined_wk4)
summary(glm_combined_wk4_elev)

# ANOVA

anova(glm_combined_wk4, test = "Chisq")
anova(glm_combined_wk4_elev, test = "Chisq")

# Statistical test

cumulative_4_weeks$FireNum <- as.numeric(cumulative_4_weeks$FireNum)

glm_combined_fire <- glm(cbind(cumulative_germ_count, Actual_count) ~ FireNum * Treatment,
                        data = cumulative_4_weeks, 
                        family = binomial())

summary(glm_combined_fire)
anova(glm_combined_fire, test = "Chisq")


# Pop ordered by fire number for new graphs
# Send scatterplots
# Pairwise comparisons

library(emmeans)

# Pairwise comparisons for Treatment within each Population
emmeans(glm_combined_wk4, pairwise ~ Treatment | Pop, type = "response")

# Pairwise comparisons for Population within each Treatment
emmeans(glm_combined_wk4, pairwise ~ Pop | Treatment, type = "response")

# Pairwise comparisons for Population within each Treatment
emmeans(glm_combined_fire, pairwise ~ Treatment | FireNum, type = "response")

emmeans(glm_combined_fire, pairwise ~ FireNum | Treatment, type = "response")

# Graphs

library(ggplot2)
library(ggpubr)

# Subset dataframe to create mean proportion for each pop/treatment combination

summary_df <- cumulative_4_weeks %>%
  group_by(FireNum, Treatment, Pop) %>%
  summarise(mean_germ = max(total_germ_prop, na.rm = TRUE))

# Mean proportion vs. fire

ggplot(summary_df, aes(x = as.factor(FireNum), y = mean_germ, color = Pop)) +
  geom_point() +
  facet_wrap(. ~ Treatment) +
  labs(title = "Interaction of Fire History, Population, and Treatment",
       x = "Fire Number",
       y = "Mean Germination Proportion")

# Total proportion vs. fire

cumulative_4_weeks$Pop <- factor(cumulative_4_weeks$Pop, levels = unique(cumulative_4_weeks$Pop[order(cumulative_4_weeks$FireNum)]))

cumulative_4_weeks$FireNum <- factor(cumulative_4_weeks$FireNum, levels = sort(unique(cumulative_4_weeks$FireNum)))

# Ordered by Pop and FireNum
ggplot(cumulative_4_weeks, aes(x = reorder(Pop, FireNum), y = total_germ_prop, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge bars for better visibility
  labs(title = "Germination Proportion by Fire Number and Treatment",
       x = "Fire Number",
       y = "Germination Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Other FireNum vs. Prop plots
ggplot(cumulative_4_weeks, aes(x = as.factor(FireNum), y = total_germ_prop, color = Treatment)) +
  geom_point() +
  facet_wrap(. ~ Pop) +
  labs(title = "Interaction of Fire History, Population, and Treatment",
       x = "Fire Number",
       y = "Total Germination Proportion")

ggplot(cumulative_4_weeks, aes(x = as.factor(FireNum), y = total_germ_prop, fill = Treatment)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2) +
  stat_compare_means(aes(group = Treatment), label = "p.signif", method = "kruskal.test") +  # Add p-values
  facet_wrap(. ~ Pop) +
  labs(title = "Interaction of Fire History, Population, and Treatment",
       x = "Fire Number",
       y = "Total Germination Proportion") +
  theme_minimal()

install.packages('ggpubr')
library(ggpubr)

dunn_results <- dunn.test(
  x = cumulative_4_weeks$total_germ_prop,  # Numeric response variable
  g = interaction(cumulative_4_weeks$Pop, cumulative_4_weeks$Treatment),  # Grouping variable
  method = "bonferroni"
)

print(dunn_results)

# Extract significance groups and add them to the plot
ggplot(cumulative_4_weeks, aes(x = FireNum, y = total_germ_prop, fill = Treatment)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ Pop, ncol = 4) +
  stat_compare_means(aes(group = Treatment), label = "p.signif", method = "anova") +  # Add p-values
  labs(title = "Fire History vs. Germination Proportion with Statistical Significance",
       x = "Fire Number",
       y = "Germination Proportion") +
  theme_minimal()


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



### Old code for regression plot ###
# Predicted values for GLM

reg_4wk_cumulative$predicted_germ_success <- predict(glm_combined_wk4, newdata = reg_4wk_cumulative, type = "response")

p <- ggplot(reg_4wk_cumulative, aes(x = Elev, y = max_germ_prop, color = Treatment)) +
  geom_point() +
  ggtitle("Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion")

Treat = unique(reg_4wk_cumulative$Treatment)

elev = seq(min(reg_4wk_cumulative$Elev), max(reg_4wk_cumulative$Elev), by = 1)

pop = unique(reg_4wk_cumulative$Pop)

glmpred = expand_grid(Treatment = Treat, Elev = elev, Pop = pop) 

glmpred$predicted_germ_success <- predict(glm_combined_wk4_elev, newdata = glmpred, type = "response")

p + geom_line(aes(y = predicted_germ_success), data = glmpred)

# pop

p_2 <- ggplot(reg_4wk_cumulative, aes(x = Elev_Level, y = max_germ_prop, color = Treatment)) +
  geom_point() +
  ggtitle("Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion")

Treat = unique(reg_4wk_cumulative$Treatment)

elev = seq(min(reg_4wk_cumulative$Elev), max(reg_4wk_cumulative$Elev), by = 1)

pop = unique(reg_4wk_cumulative$Pop)

glmpred_2 = expand_grid(Treatment = Treat, Elev = elev, Pop = pop) 

glmpred_2$predicted_germ_success <- predict(glm_combined_wk4_elev, newdata = glmpred_2, type = "response")

p_2 + geom_line(aes(y = predicted_germ_success), data = glmpred_2)

# Can try facet_wrap with elevation, can try out sort_by method

# Reverse transformation from predicted line

# Linear model with proportions for each cell

cumulative_4_weeks$Elev_Level <- cut(cumulative_4_weeks$Elev,
                                     breaks = c(-Inf, 786, 1612, 2800),  # Your specified range limits
                                     labels = c("Low", "Medium", "High"))
# Calculate proportions

cumulative_4_weeks <- cumulative_4_weeks %>%
  mutate(total_germ_prop = total_germ / Actual_count)

# Plot

reg_total_prop_wk4 <- lm(total_germ_prop ~ Elev * Treatment, family = binomial(link = "logit"), data = cumulative_4_weeks)
cumulative_4_weeks$predicted_total_germ_prop <- predict(reg_total_prop_wk4)

predictions <- predict(reg_total_prop_wk4, newdata = cumulative_4_weeks, type = "response", se.fit = TRUE)

# Back-transform predictions and SEs
cumulative_4_weeks$predicted_total_germ_prop <- plogis(predictions$fit)
cumulative_4_weeks$se <- predictions$se.fit
cumulative_4_weeks$lower <- plogis(predictions$fit - predictions$se.fit) 
cumulative_4_weeks$upper <- plogis(predictions$fit + predictions$se.fit)

p <- ggplot(cumulative_4_weeks, aes(x = Elev, y = total_germ_prop, color = Treatment)) +
  geom_point() +
  ggtitle("Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion")

ggplot(reg_4wk_cumulative, aes(x = Elev, y = max_germ_prop, color = Treatment)) +
  geom_point() +
  geom_line(aes(y = predicted_germ_success)) +
  ggtitle("Total Germination Proportion After 4 Weeks") +
  labs(x = "Elevation", y = "Total Germination Proportion")




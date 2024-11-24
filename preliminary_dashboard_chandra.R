library(ggplot2)
library(dplyr)
library(usmap)
library(shiny)

crime_data <- read.csv('/Users/crazygay/Desktop/Classes/Stat 451/project01/crime_data_w_population_and_crime_rate.csv')
crime_data <- crime_data %>%
  rename(population = population, crime_rate = crime_rate_per_100000)

crime_data <- crime_data %>%
  mutate(population_group = case_when(
    population < 50000 ~ "Under 50k",
    population >= 50000 & population < 200000 ~ "50k - 200k",
    population >= 200000 & population < 500000 ~ "200k - 500k",
    population >= 500000 ~ "Above 500k"
  ))

ggplot(crime_data, aes(x = population_group, y = crime_rate, fill = population_group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  scale_fill_brewer(palette = "YlOrRd") + 
  labs(
    title = "Crime Rate by Population Size in all US Counties",
    x = "Population Size Group",
    y = "Crime Rate per 100,000 People"
  ) +
  theme_linedraw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


ggplot(crime_data, aes(x = population, y = crime_rate)) +
  geom_point(alpha = 0.3, color = "blue") +           
  scale_x_log10(labels = scales::comma) +        
  labs(
    title = "Population vs. Crime Rate in all US Counties",
    x = "Population (Log Scale)",
    y = "Crime Rate per 100,000 People"
  ) +
  theme_linedraw() +                         
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")  




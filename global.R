library(dplyr)

### import unemployment data ###
unemployment_crime_data <- read.csv("/Users/edisonlu/Downloads/crimebystatecombinedwithunemployment.csv")

### Process the unemployment data ###
state_means <- 
  unemployment_crime_data %>%
  group_by(state) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(overall_mean = rowMeans(select(., where(is.numeric)), na.rm = TRUE))

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(usmap)
library(stringr)
library(tidyverse)
library(maps)
#Final
################################################################################
# Datasets
################################################################################
unemployment_crime_data <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crimebystatecombinedwithunemployment.csv")
crime_data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\US_violent_crime.csv")
crime_data_population <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crime_data_w_population_and_crime_rate.csv")
crime_pop_data <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crime_data_w_population_and_crime_rate.csv")
data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\2014-2022 Medicare FFS Geographic Variation Public Use File.csv")
crimedata_economics <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crimedata.csv")
crime_data_education <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\US Violent Crime Dataset (3).csv")



################################################################################
# Unemployment data manipulation
################################################################################
state_means <- 
  unemployment_crime_data %>%
  group_by(state) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(overall_mean = rowMeans(select(., where(is.numeric)), na.rm = TRUE))

### Process the crime rate with population data ###
us_counties <- maps::county.fips
us_counties <- us_counties %>%
  separate(polyname, into = c("state", "county"), sep = ",") 

crime_pop_data <- crime_pop_data %>%
  separate(county_name, into = c("county", "state"), sep = ",\\s*", remove = FALSE) %>%
  mutate(
    county = str_to_lower(county) %>%
      str_replace_all("\\.", "") %>% 
      str_replace("\\scounty$", ""),
    state = str_to_lower(state)  # State abbreviation in lowercase for consistency
  )

crime_pop_data <- crime_pop_data %>%
  mutate(county = str_replace(county, "\\scity$", " city"))

crime_pop_data <- crime_pop_data %>%
  select(-county_name)

crime_pop_data <- crime_pop_data %>%
  left_join(us_counties, by = "county", relationship = "many-to-many")
################################################################################
# Population
################################################################################
crime_data_population <- crime_data_population %>%
  rename(population = population, crime_rate = crime_rate_per_100000)


crime_data_population <- crime_data_population %>%
  mutate(population_group = case_when(
    population < 50000 ~ "Under 50k",
    population >= 50000 & population < 200000 ~ "50k - 200k",
    population >= 200000 & population < 500000 ~ "200k - 500k",
    population >= 500000 ~ "Above 500k"
  ))
crime_data_population <- crime_data_population %>%
  mutate(
    population_group = factor(population_group, levels = c("Under 50k", "50k - 200k", "200k - 500k", "Above 500k"))
  )
################################################################################
# Medicare data manipulation
################################################################################


crime_data$X <- state.abb[match(crime_data$X, state.name)]
data = data[,c(1:3, 5:6, 12:17)]
colnames(data)[c(2,3,4,5,6,7,8,9,10,11)] = c("Level","Level.County","Age","Total","Female.PCT","Male.PCT","White.PCT",
                                             "Black.PCT","Hisp.PCT","Other.PCT")
data_filtered <- data[grepl("^[A-Za-z]{2}$", data$Level.County), ]
data_filtered <- data_filtered[data_filtered$Age == "All",] 
data_filtered <- data_filtered[!data_filtered$Level.County %in% c("ZZ", "PR", "VI", "DC"), ]
data_filtered <- data_filtered[data_filtered$YEAR == "2022",] 

combined_data <- merge(data_filtered, crime_data, by.x = "Level.County", by.y = "X")

combined_data <- combined_data[, !names(combined_data) %in% c("YEAR", "Level","Age","Total","Male.PCT", "Female.PCT", "UrbanPop")]
combined_data <- combined_data %>%
  rename(White = White.PCT, Black = Black.PCT, Hispanic = Hisp.PCT, Other = Other.PCT)

################################################################################
# Medicare plot generation
################################################################################
generate_heatmap <- function(data, crime_metric, title) {
  
  
  # Ensure numeric types for percentages
  percentage_cols <- c("White", "Black", "Hispanic", "Other")
  data[percentage_cols] <- lapply(data[percentage_cols], as.numeric)
  
  # Select relevant columns
  heatmap_data <- data[, c("Level.County", crime_metric, "White", "Black", "Hispanic", "Other")]
  
  # Convert to long format
  heatmap_long <- tidyr::pivot_longer(
    heatmap_data,
    cols = all_of(c(crime_metric, "White", "Black", "Hispanic", "Other")),
    names_to = "Metric",
    values_to = "Value"
  )
  
  # Add a new column for distinguishing between Crime and Race
  heatmap_long <- heatmap_long %>%
    mutate(DataType = ifelse(Metric == crime_metric, "Crime", "Race"))
  
  # Reorder states based on the crime metric
  crime_order <- heatmap_long %>%
    filter(Metric == crime_metric) %>%
    arrange(desc(Value)) %>%
    pull(Level.County)
  
  # Reorder the race metrics
  race_order <- c("White", "Black", "Hispanic", "Other")
  
  # Separate data for crime and race
  crime_data <- heatmap_long %>% filter(DataType == "Crime")
  race_data <- heatmap_long %>% 
    filter(DataType == "Race") %>%
    mutate(Metric = factor(Metric, levels = race_order))
  
  # Crime plot
  crime_plot <- ggplot(crime_data, aes(
    x = Metric, 
    y = factor(Level.County, levels = crime_order), 
    fill = Value
  )) +
    geom_tile(color = "white", size = 0.5) + # Add white borders to tiles
    scale_fill_gradient(
      low = "yellow", high = "red", 
      name = "Crime Rate"
    ) +
    labs(
      x = NULL, 
      y = "State", 
      title = title, 
      subtitle = "Rate = per 100,000 people \nPercent = of those enrolled in Medicare"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 15),
      title = element_text(size = 24),
      plot.subtitle = element_text(size = 16), 
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_colorbar(
        title = "Crime Rate",
        title.position = "top",
        barwidth = 1,
        barheight = 12  # Adjust bar height for vertical scaling
      )
    )
  
  # Race plot
  race_plot <- ggplot(race_data, aes(
    x = Metric, 
    y = factor(Level.County, levels = crime_order), 
    fill = Value
  )) +
    geom_tile(color = "white", size = 0.5) + # Add white borders to tiles
    scale_fill_gradient(
      low = "lightblue", high = "blue", 
      name = "Race Percentage"
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_colorbar(
        title = "Race Percentage",
        title.position = "top",
        barwidth = 1,
        barheight = 12  # Adjust bar height for vertical scaling
      )
    )
  
  crime_legend <- cowplot::get_legend(crime_plot)
  race_legend <- cowplot::get_legend(race_plot)
  
  crime_plot <- crime_plot + theme(legend.position = "none")
  race_plot <- race_plot + theme(legend.position = "none")
  
  combined_plot <- cowplot::plot_grid(
    crime_plot, race_plot, 
    ncol = 2, align = "h", rel_widths = c(1, 1)
  )
  
  combined_legend <- cowplot::plot_grid(crime_legend, race_legend, nrow = 1, align = "v", axis = "lr")
  
  final_plot <- cowplot::plot_grid(combined_plot, combined_legend, rel_widths = c(4, 1), align = "h")
  
  return(final_plot)
}
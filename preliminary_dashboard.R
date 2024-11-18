library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

unemployment_crime_data <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crimebystatecombinedwithunemployment.csv")


################################################################################
######################### Start of Medicare Code ###############################
################################################################################

crime_data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\US_violent_crime.csv")
crime_data$X <- state.abb[match(crime_data$X, state.name)]
data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\Medicare Demographics.csv")
data_filtered <- data[grepl("^[A-Za-z]{2}$", data$Level.County), ]
data_filtered <- data_filtered[data_filtered$Age == "All",] 
data_filtered <- data_filtered[!data_filtered$Level.County %in% c("ZZ", "PR", "VI", "DC"), ]
data_filtered <- data_filtered[data_filtered$YEAR == "2022",] 

combined_data <- merge(data_filtered, crime_data, by.x = "Level.County", by.y = "X")

combined_data <- combined_data[, !names(combined_data) %in% c("YEAR", "Level","Age","Total","Male.PCT", "Female.PCT", "UrbanPop")]
combined_data <- combined_data %>%
  rename(White = White.PCT, Black = Black.PCT, Hispanic = Hisp.PCT, Other = Other.PCT)
generate_heatmap <- function(data, crime_metric, title) {
  # Check if required columns exist
  required_cols <- c("Level.County", crime_metric, "White", "Black", "Hispanic", "Other")
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("Missing columns:", paste(setdiff(required_cols, colnames(data)), collapse = ", ")))
  }
  
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
  
  # Plot for crime data
  crime_plot <- ggplot(crime_data, aes(
    x = Metric, 
    y = factor(Level.County, levels = crime_order), 
    fill = Value
  )) +
    geom_tile() +
    scale_fill_gradient(
      low = "white", high = "red", 
      name = "Crime Rate"
    ) +
    labs(
      x = NULL, 
      y = "State", 
      title = title, 
      subtitle = "Rate = per 100,000 people"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 15),
          title = element_text(size = 24),
          subtitle = element_text(size = 16), 
          legend.title = element_text(size = 14), 
          legend.text = element_text(size = 12))     
  
  # Plot for race data
  race_plot <- ggplot(race_data, aes(
    x = Metric, 
    y = factor(Level.County, levels = crime_order), 
    fill = Value
  )) +
    geom_tile() +
    scale_fill_gradient(
      low = "white", high = "blue", 
      name = "Race Percentage"
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  
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
################################################################################
########################## End of Medicare Code ################################
################################################################################


# UI
ui <- page_navbar(
  nav_panel("Crime and Unemployment Analysis",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "state",
                  label = "Select a state:",
                  choices = c("All", unique(unemployment_crime_data$state)),
                  selected = "All"
                )
              ),
              mainPanel(
                plotOutput(outputId = "crime_unemployment_plot", height = "400px")
              )
            )
  ),
  
  # Medicare Implementation
  nav_panel("Crime Rate by Type and Medicare Enrollement by Race",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "crime_type",
                  label = "Select a Crime Rate:",
                  choices = c("Murder Rate", "Assault Rate", "Rape Rate"),
                  selected = "Murder Rate"
                )
              ),
              mainPanel(
                plotOutput(outputId = "crime_rate_plot", height = "800px")
              )
            )
  ),

  #BAR PLOTS
  nav_panel("Crime Rates by State",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "crime_type_bar",
                  label = "Select a Crime Rate:",
                  choices = c("Murder", "Assault", "Rape"),
                  selected = "Murder"
                )
              ),
              mainPanel(
                plotOutput(outputId = "crime_rate_bar_plot", height = "600px")
              )
            )
  ),
  nav_panel("Yet another visualization"),
  nav_panel("Still more visualization!"),
  
  title = "Visualization of Crime Data in the U.S.",
  id = "page"
)

# Server
server <- function(input, output) {
  output$crime_unemployment_plot <- renderPlot({
    state_means <- unemployment_crime_data %>%
      group_by(state) %>%
      summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      mutate(overall_mean = rowMeans(select(., where(is.numeric)), na.rm = TRUE))
    
    ggplot(state_means, aes(x = unemployment, y = violent.total, color = state)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_viridis_d(option = "D") +
      labs(title = "Average number of violent crime vs. \nunemployment rate across states",
           x = "Unemployment rate",
           y = "Average number of violent crimes")
  })
  
  # Render Medicare Visualization Based on Selection
  output$crime_rate_plot <- renderPlot({
    crime_metric <- switch(input$crime_type,
                           "Murder Rate" = "Murder",
                           "Assault Rate" = "Assault",
                           "Rape Rate" = "Rape")
    
  
    generate_heatmap(combined_data, crime_metric, paste(crime_metric, "Rate and Medicare Enrollment by Race Percentages"))
  })

  #BAR PLOTS FOR SELECTED CRIME TYPES
  output$crime_rate_bar_plot <- renderPlot({
    crime_type <- input$crime_type_bar
    crime_col <- switch(crime_type,
                        "Murder" = "Murder",
                        "Assault" = "Assault",
                        "Rape" = "Rape")
    fill_color <- switch(crime_type,
                         "Murder" = "steelblue",
                         "Assault" = "darkorange",
                         "Rape" = "purple")
    
    ggplot(crime_data, aes(x = reorder(State, -get(crime_col)), y = get(crime_col))) +
      geom_bar(stat = "identity", fill = fill_color) +
      labs(title = paste(crime_type, "Rates by State"), x = "State", y = paste(crime_type, "Rate")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

shinyApp(ui, server)

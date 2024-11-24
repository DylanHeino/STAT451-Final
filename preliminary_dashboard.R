library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(usmap)

################################################################################
# Datasets
################################################################################
unemployment_crime_data <- read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\crimebystatecombinedwithunemployment.csv")
crime_data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\US_violent_crime.csv")


state_means <- 
  unemployment_crime_data %>%
  group_by(state) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(overall_mean = rowMeans(select(., where(is.numeric)), na.rm = TRUE))

################################################################################
######################### Start of Medicare Code ###############################
################################################################################


crime_data$X <- state.abb[match(crime_data$X, state.name)]
data = read.csv("C:\\Users\\Dylan H\\Documents\\STAT 451\\Final\\2014-2022 Medicare FFS Geographic Variation Public Use File.csv")
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
                ),
                sliderInput(
                  inputId = "population",
                  label = "Select population range:",
                  min = min(state_means$Population)-10000,
                  max = max(state_means$Population)+10000,
                  value = c(min(state_means$Population)-10000, max(state_means$Population)+10000),
                  step = 10000
                ),
                sliderInput(
                  inputId = "Unemployment Rate",
                  label = "Select range for unemployment rate:",
                  min = min(state_means$unemployment)-0.5,
                  max = max(state_means$unemployment)+0.5,
                  value = c(min(state_means$unemployment)-0.5, max(state_means$unemployment)+0.5),
                  step = 0.1
                )
              ),
              
              mainPanel(
                plotOutput(outputId = "crime_unemployment_plot", height = "400px",
                           hover = hoverOpts(id = "plot_hover",
                                             delayType = "throttle")),
                verbatimTextOutput(outputId = "hover_info"),
                plotOutput(outputId = "detail_violent_crime_plot", height = "300px")
              )
              
            )
  ),
  nav_panel("Mean Violent crimes map",
            
            sidebarLayout(
              sidebarPanel(
                h4("State Details"),
                verbatimTextOutput(outputId = "State_info")
              ),
              
              mainPanel(
                plotOutput(outputId = "mean_violent_crime_map", height = "600px",
                           hover = hoverOpts(id = "map_hover", delayType = "throttle"))
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
  
  # Medicare Implementation
  nav_panel("Crime Type and Medicare",
            sidebarLayout(
              sidebarPanel(
                textInput("state_search", "Enter State Abbreviation:"),
                selectInput("crime_type", "Select Crime Type", choices = c("Murder Rate", "Assault Rate", "Rape Rate"))
              ),
              mainPanel(
                # Conditionally show the crime rate value text only when a state is entered
                conditionalPanel(
                  condition = "input.state_search != ''",
                  textOutput("crime_rate_value")
                ),
                
                # Conditionally show the bar plot for race percentages only when a state is entered
                conditionalPanel(
                  condition = "input.state_search != ''",
                  plotOutput("race_percentage_bar_plot", height = "800px")
                ),
                
                # Conditionally show the heatmap only when no state is selected
                conditionalPanel(
                  condition = "input.state_search == ''",
                  plotOutput("crime_rate_plot", height = "800px")
                )
              )
            )
  ),
  
  nav_panel("Still more visualization!"),
  
  title = "Visualization of Crime Data in the U.S.",
  id = "page"
)
################################################################################
# Server
server <- function(input, output) {
  
  
################################################################################
  # Unemployment plot and US map
################################################################################
  filtered_data <- reactive({
    
    # Filter by state
    if (input$state != "All") {
      state_means <- state_means %>% filter(state == input$state)
    }
    
    # Filter by population range
    state_means <- state_means %>% filter(Population >= input$population[1], Population <= input$population[2])
    
    # Filter by unemployment range
    state_means <- state_means %>% filter(unemployment >= input$`Unemployment Rate`[1], 
                                          unemployment <= input$`Unemployment Rate`[2])
    
    state_means
  })
  
  output$detail_violent_crime_plot <- renderPlot({
    
    state_long <- state_means %>%
      select(state, Murder, rape, Robbery, Aggravated.assault) %>%
      pivot_longer(cols = c(Murder, rape, Robbery, Aggravated.assault),
                   names_to = "Crime_Type",
                   values_to = "Count")
    
    if (input$state != "All") {
      state_long_new <- state_long %>% filter(state == input$state)
      ggplot(state_long_new, aes(x = Crime_Type, y = Count, fill = Crime_Type)) +
        geom_bar(stat = "identity") +
        labs(title = ifelse(input$state != "All",
                            paste("Violent Crime Counts in", input$state),
                            "Violent Crime Counts Across All States"),
             x = "Crime Type",
             y = "Count") +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
    } else {
      state_data <- state_means
      ggplot(state_long, aes(x = Crime_Type, y = Count, fill = Crime_Type)) +
        geom_bar(stat = "identity") +
        labs(title = ifelse(input$state != "All",
                            paste("Violent Crime Counts in", input$state),
                            "Violent Crime Counts Across All States"),
             x = "Crime Type",
             y = "Count") +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
    }
    
  })
  
  
  output$crime_unemployment_plot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = unemployment, y = violent.total, color = state)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_viridis_d(option = "D") +
      labs(title = "Average number of violent crime vs. unemployment rate across states",
           x = "Unemployment rate",
           y = "Average number of violent crimes") +
      theme(
        plot.title <- element_text(size = 20, face = "bold"),
        axis.title.x <- element_text(size = 18),
        axis.title.y <- element_text(size = 18),
        axis.text.x <- element_text(size = 14),
        axis.text.y <- element_text(size = 14),
        legend.text <- element_text(size = 14),
        legend.title <- element_text(size = 18)
      )
    
  })
  
  
  output$hover_info <- renderPrint({
    hover <- input$plot_hover  # Get hover details
    if (!is.null(hover)) {
      # Find the nearest point
      nearest_point <- state_means %>%
        filter(
          abs(unemployment - hover$x) < 0.1 & abs(violent.total - hover$y) < 20
        ) %>%
        slice(1)
      
      if (nrow(nearest_point) > 0) {
        # Format the nearest_point details for display
        paste(
          "State:", nearest_point$state,
          "Unemployment Rate:", round(nearest_point$unemployment, 2),
          "Violent Crimes (Avg):", round(nearest_point$violent.total, 2)
        )
      } else {
        "Hover over a point to see details."
      } 
    } else {
      "Hover over a point to see details."
    }
  })
  
  output$state_info <- renderPrint({
    
    state_centers <- usmap::us_map("states") %>%
      group_by(full) %>%
      summarize(
        x = mean(range(x)),
        y = mean(range(y)),
        state = unique(full)
      )
    
    hover_map <- input$map_hover
    if (!is.null(hover_map)) {
      state_data <- usmap::us_map() %>%
        mutate(hover_x = hover_map$x, hover_y = hover_map$y) %>%
        filter(abs(x - hover_x) < 5, abs(y - hover_y) < 5) %>%
        Slice(1) %>%
        select(state)
      
      if (nrow(state_data) > 0) {
        matched_state <- state_means %>%
          filter(state == state_data$state)
        
        if (nrow(matched_state) > 0) {
          paste(
            "State: ", matched_state$state,
            "\nMean Population:", round(matched_state$Population, 2),
            "\nMean Unemployment Rate:", round(matched_state$unemployment, 2),
            "\nMean Violent Crimes:", round(matched_state$violent.total, 2)
          )
        } else {
          "Hover over a valid state to see more information"
        }
      } else {
        "No state found, try again"
      }
    } else {
      "Hover over the map to begin exploring"
    }
  })
  
  output$mean_violent_crime_map <- renderPlot({
    plot_usmap(data = state_means, values = "violent.total", labels = TRUE) + 
      scale_fill_gradient2(
        low = "purple", 
        high = "gold", 
        mid = "white", 
        midpoint = median(state_means$violent.total), 
        limits = c(0, max(state_means$violent.total)), 
        breaks = seq(0, max(state_means$violent.total), by = 200), 
        name = "Violent crimes", 
        labels = scales::comma
      ) +
      theme(
        legend.position = "right", 
        legend.key.size = unit(1.75, "cm"), 
        legend.text = element_text(size = 11, face = 'bold'), 
        legend.title = element_text(size = 12, face = 'bold')
      )
  })
################################################################################
  # Render Medicare Visualization Based on Selection
################################################################################
  output$crime_rate_value <- renderText({
    crime_metric <- switch(input$crime_type,
                           "Murder Rate" = "Murder",
                           "Assault Rate" = "Assault",
                           "Rape Rate" = "Rape")
    
    # Filter data based on state abbreviation input
    filtered_data_medicare <- combined_data
    if (input$state_search != "") {
      filtered_data_medicare <- combined_data %>%
        filter(Level.County == input$state_search)
    }
    
    if (input$state_search != "" && nrow(filtered_data_medicare) > 0) {
      # Fetch the crime value for the selected state and metric
      crime_value <- filtered_data_medicare[[crime_metric]]
      
      # Ensure crime_value is numeric and valid
      crime_value <- as.numeric(crime_value)
      
      # Generate the text output
      trimws(paste(crime_metric, "Rate for", input$state_search, ":", crime_value))
    } else {
      # Return an empty string if no valid state or data is found
      ""
    }
  })
  
  # Render bar plot for race percentages when a state is selected
  output$race_percentage_bar_plot <- renderPlot({
    crime_metric <- switch(input$crime_type,
                           "Murder Rate" = "Murder",
                           "Assault Rate" = "Assault",
                           "Rape Rate" = "Rape")
    
    # Filter data based on state abbreviation input
    filtered_data_medicare <- combined_data
    if (input$state_search != "") {
      filtered_data_medicare <- combined_data %>%
        filter(Level.County == input$state_search)
    }
    
    if (input$state_search != "" && nrow(filtered_data_medicare) > 0) {
      # Create a data frame for race percentages
      race_data <- filtered_data_medicare %>%
        select(White, Black, Hispanic, Other) %>%
        gather(key = "Race", value = "Percentage")
      
      # Ensure Percentage is numeric
      race_data$Percentage <- as.numeric(race_data$Percentage)
      
      # Fetch the crime value for the selected crime type
      crime_value <- filtered_data_medicare[[crime_metric]]
      
      # Ensure crime_value is numeric and valid
      crime_value <- as.numeric(crime_value)
      
      # Set the factor levels to control the order of bars
      race_data$Race <- factor(race_data$Race, levels = c("White", "Black", "Hispanic", "Other"))
      
      # Title showing race percentages and crime rate value
      plot_title <- paste("Race Percentages of Medicare Enrollment for", input$state_search)
      
      # Create the bar plot
      ggplot(race_data, aes(x = Race, y = Percentage, fill = Race)) +
        geom_bar(stat = "identity") +
        labs(title = plot_title,
             y = "Percentage (%)") +
        scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
        scale_fill_manual(values = c("White" = "lightblue", "Black" = "darkorange", "Hispanic" = "green", "Other" = "purple")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              plot.title = element_text(hjust = 0.5, size = 24))  # Centers the title
    } else {
      # Return an empty plot if no valid state is selected
      ggplot() + theme_void()
    }
  })
  
  
  # Render the heatmap when no state is selected
  output$crime_rate_plot <- renderPlot({
    crime_metric <- switch(input$crime_type,
                           "Murder Rate" = "Murder",
                           "Assault Rate" = "Assault",
                           "Rape Rate" = "Rape")
    
    # Filter data based on state abbreviation input
    filtered_data_medicare <- combined_data
    if (input$state_search != "") {
      filtered_data_medicare <- combined_data %>%
        filter(Level.County == input$state_search)
    }
    
    # Show heatmap only when no state is selected
    if (input$state_search == "") {
      generate_heatmap(combined_data, crime_metric, paste(crime_metric, "Rate and Medicare Enrollment by Race Percentages"))
    } else {
      # Return an empty plot when a state is selected (no heatmap)
      ggplot() + theme_void()
    }
  })


################################################################################
  # BAR PLOTS FOR SELECTED CRIME TYPES
################################################################################
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
    
    ggplot(crime_data, aes(x = reorder(X, -get(crime_col)), y = get(crime_col))) +
      geom_bar(stat = "identity", fill = fill_color) +
      labs(title = paste(crime_type, "Rates by State"), x = "State", y = paste(crime_type, "Rate")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
################################################################################  

}

shinyApp(ui, server)

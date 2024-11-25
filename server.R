library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)

server <- function(input, output) {
  
  ### Filter the state_means data frame ### 
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
  
  ### Plotting the Bar plot of mean count of different type of violent crimes ###
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
          plot.title = element_text(size = 30, face = "bold"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
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
          plot.title = element_text(size = 30, face = "bold"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          legend.position = "none"
        )
    }
    
  })
  
  ### Plotting the unemployment rate vs. mean violent crime count scatterplot ###
  output$crime_unemployment_plot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = unemployment, y = violent.total, color = state)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_viridis_d(option = "D") +
      labs(title = "Average number of violent crime vs. unemployment rate across states",
           x = "Unemployment rate",
           y = "Average number of violent crimes") +
      theme(
        plot.title <- element_text(size = 30, face = "bold"),
        axis.title.x <- element_text(size = 24),
        axis.title.y <- element_text(size = 24),
        axis.text.x <- element_text(size = 20),
        axis.text.y <- element_text(size = 20),
        legend.text <- element_text(size = 18),
        legend.title <- element_text(size = 18)
      )
    
  })
  
  ### Function for hover feature of unemployment rate vs. mean violent crime count scatterplot ###
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
  
}
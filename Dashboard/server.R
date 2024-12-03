server <- function(input, output) {
  
  
  ################################################################################
  # Unemployment plot and US map
  ################################################################################
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
    library(stringr) # Ensure stringr is loaded for string manipulation
    
    state_long <- state_means %>%
      select(state, Murder, rape, Robbery, Aggravated.assault) %>%
      pivot_longer(
        cols = c(Murder, rape, Robbery, Aggravated.assault),
        names_to = "Crime_Type",
        values_to = "Count"
      ) %>%
      mutate(Crime_Type = str_replace_all(Crime_Type, "\\.", " ") %>% # Replace "." with " "
               str_to_title()) # Capitalize each word
    
    if (input$state != "All") {
      state_long_new <- state_long %>% filter(state == input$state)
      ggplot(state_long_new, aes(x = Crime_Type, y = Count, fill = Crime_Type)) +
        geom_bar(stat = "identity") +
        labs(
          title = paste("Violent Crime Counts in", input$state),
          x = "Crime Type",
          y = "Count"
        ) +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
    } else {
      ggplot(state_long, aes(x = Crime_Type, y = Count, fill = Crime_Type)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Violent Crime Counts Across All States",
          x = "Crime Type",
          y = "Count"
        ) +
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
  
  output$mean_violent_crime_map <- renderPlot({
    plot_usmap(data = state_means, values = "violent.total", labels = TRUE) + 
      scale_fill_gradient2(
        low = "white", 
        high = "red", 
        mid = "white", 
        midpoint = median(state_means$violent.total), 
        limits = c(0, max(state_means$violent.total)), 
        breaks = seq(0, max(state_means$violent.total), by = 200), 
        name = "Violent crimes", 
        labels = scales::comma
      ) +
      labs(title = "Mean Violent Crimes by State") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "right", 
        legend.key.size = unit(1.75, "cm"), 
        legend.text = element_text(size = 12, face = 'bold'), 
        legend.title = element_text(size = 18, face = 'bold')
      )
  })
  ### Plotting the unemployment rate vs. mean violent crime count scatterplot ###
  output$crime_unemployment_plot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = unemployment, y = violent.total, color = state)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_viridis_d(option = "D") +
      labs(title = "Average number of violent crime vs. unemployment rate across states",
           x = "Unemployment rate (%)",
           y = "Average number of violent crimes") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18)
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
  
  plot_region <- function(region_input) {
    plot_usmap("counties",
               include = region_input,
               data = crime_pop_data,
               values = "crime_rate_per_100000",
               color = "black") +
      scale_fill_continuous(low = "gold",
                            high = "purple",
                            name = "Crime rate per 100,000 people") +
      labs(
        title = "Crime rate per 100,000 people for Mountain Region"
      ) +
      theme(panel.background = element_rect(color = "lightgrey", fill = "white"),
            legend.position = "right",
            plot.title = element_text(size = 22, face = "bold"),  # Title font size
            plot.subtitle = element_text(size = 18),             # Subtitle font size
            legend.text = element_text(size = 14),               # Legend text font size
            legend.title = element_text(size = 16),              # Legend title font size
            axis.text = element_text(size = 12),                 # Axis text font size
            axis.title = element_text(size = 14) )
  }
  
  output$crime_rate_map_us <- renderPlot({
    
    if (input$region_selector == "All") {
      plot_usmap(regions = "counties",
                 data = crime_pop_data,
                 values = "crime_rate_per_100000",
                 size = 2,
                 color = "black") +
        scale_fill_continuous(low = "gold",
                              high = "purple",
                              name = "Crime rate per 100,000 people",
                              label = scales::comma) +
        labs(title = "Crime rate of counties in the U.S.",
             subtitle = "Crime rate per 100,000 people") +
        theme(panel.background = element_rect(color = "black", fill = "white"),
              legend.position = "right",
              plot.title = element_text(size = 22, face = "bold"),  # Title font size
              plot.subtitle = element_text(size = 18),             # Subtitle font size
              legend.text = element_text(size = 14),               # Legend text font size
              legend.title = element_text(size = 16),              # Legend title font size
              axis.text = element_text(size = 12),                 # Axis text font size
              axis.title = element_text(size = 14) )
    } else {
      regions_map <- list(
        "New England" = .new_england,
        "Mid Atlantic" = .mid_atlantic,
        "East North Central" = .east_north_central,
        "West North Central" = .west_north_central,
        "South Atlantic" = .south_atlantic,
        "East South Central" = .east_south_central,
        "West South Central" = .west_south_central,
        "Mountain" = .mountain,
        "Pacific" = .pacific)
      
      selected_region <- regions_map[[input$region_selector]]
      plot_region(selected_region)
      
    }
    
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
    filtered_label <- average_labels %>%
      filter(Metric == crime_metric)
    if (input$state_search != "" && nrow(filtered_data_medicare) > 0) {
      # Fetch the crime value for the selected state and metric
      crime_value <- filtered_data_medicare[[crime_metric]]
      
      # Ensure crime_value is numeric and valid
      crime_value <- as.numeric(crime_value)
      crime_label <- filtered_label$Average
      # Generate the text output
      trimws(paste(
        crime_metric, " Rate for ", input$state_search, ": ", crime_value,
        "\n", crime_metric, " Rate Average : ", crime_label,
        "\n","White Average : ",white_average$Average,"%",
        "\n","Black Average : ",black_average$Average,"%",
        "\n","Hispanic Average : ",hisp_average$Average,"%",
        "\n","Other Average : ",other_average$Average,"%",
        sep = ""
      ))
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
      plot_title <- paste("Race Percentages Of Medicare Enrollment for", input$state_search)
      
      # Create the bar plot
      ggplot(race_data, aes(x = Race, y = Percentage*100, fill = Race)) +
        geom_bar(stat = "identity") +
        labs(title = plot_title,
             y = "Percentage (%)") +
        scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))+
        scale_fill_manual(values = c("White" = "lightblue", "Black" = "darkorange", "Hispanic" = "green", "Other" = "purple")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
              legend.title = element_text(size = 18),  
              legend.text = element_text(size = 15))
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
      generate_heatmap(combined_data, crime_metric, paste("2022", crime_metric, "Rate and Medicare Enrollment by Race Percentages"))
    } else {
      # Return an empty plot when a state is selected (no heatmap)
      ggplot() + theme_void()
    }
  })
  
  ################################################################################
  # Population
  ################################################################################
  output$crime_rate_plots_population <- renderPlot({
    if (input$plot_choice == "Violin Plot") {
      ggplot(crime_data_population, aes(x = population_group, y = crime_rate, fill = population_group)) +
        geom_violin(trim = FALSE, alpha = 0.7) +
        scale_fill_brewer(palette = "YlOrRd") +
        labs(
          title = "Crime Rate by Population Size in all US Counties",
          x = "Population Size Group",
          y = "Crime Rate per 100,000 People",
          fill = "Population"  # Change legend title here
        ) +
        theme_linedraw() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)
        )
    } else {
      ggplot(crime_data_population, aes(x = population, y = crime_rate)) +
        geom_point(alpha = 0.3, color = "blue") +           
        scale_x_log10(labels = scales::comma) +        
        labs(
          title = "Population vs. Crime Rate in all US Counties",
          x = "Population (Log Scale)",
          y = "Crime Rate per 100,000 People"
        ) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Move this line before theme()
        theme_linedraw() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16)  # Adjust axis text size here
        )
    }
  })
  ################################################################################
  # Economics
  ################################################################################
  output$scatterPlot <- renderPlot({
    # Dynamic labels
    variable_label <- ifelse(input$variable == "medIncome", 
                             "Median Income ($)", 
                             "Percentage of Population Under Poverty (%)")
    crime_label <- ifelse(input$crime_type_economics == "ViolentCrimesPerPop",
                          "Violent Crimes Per Population",
                          "Non-Violent Crimes Per Population")
    
    # Choose color based on the selected crime type
    plot_color <- if (input$crime_type_economics == "ViolentCrimesPerPop") {
      "orange"
    } else {
      "blue"
    }
    
    # Generate scatter plot with updated font sizes and dynamic title
    ggplot(crimedata_economics, aes_string(x = input$variable, y = input$crime_type_economics)) +
      geom_point(alpha = 0.6, color = plot_color) +
      labs(
        x = variable_label,
        y = crime_label,
        title = paste("Relationship Between", variable_label, "and", crime_label)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 18,),
        axis.text = element_text(size=14)
      )
  })  

################################################################################
  # Education and Drugs
################################################################################
  # Calculate correlation coefficient
  selected_factor <- reactive({
    if (input$analysis_type == "education") {
      list(
        column = "MostEducatedStatesTotalBSDegreeOrHigher",
        label = "Population with Bachelor's Degree or Higher (%)",
        title_prefix = "Education Level",
        subtitle = "Higher education measured as percentage of population with bachelor's degree or higher",
        color = "darkblue",
        smooth_color = "red",
        smooth_fill = "pink"
      )
    } else if(input$factor_type == "alcohol") {
      list(
        column = "AlcoholConsumptionGallonsOfEthanolPerCapita",
        label = "Alcohol Consumption Gallons Of Ethanol Per Capita",
        title_prefix = "Alcohol Level",
        subtitle = "Alcohol level measured as Gallons Of Ethanol consumed Per Capita",
        color = "darkblue",
        smooth_color = "gold",
        smooth_fill = "yellow"
      )
    } else if(input$factor_type == "homelessness") {
      list(
        column = "HomelessPopulationPer10kResidents",
        label = "Homeless Population Per 10k Residents",
        title_prefix = "Homelessness Level",
        subtitle = "Homelessness measured as homeless population per 10,000 residents",
        color = "darkblue",
        smooth_color = "darkblue",
        smooth_fill = "lightblue"
      )
    } else {
      list(
        column = "DrugUse_18PlusIllicitDrugUsePastMonth_202122",
        label = "Illicit Drug Use Percentage",
        title_prefix = "Illicit Drug Usage",
        subtitle = "Illicit Drug Use Percentage is calculated by comparing the number of individuals exhibiting drug use to overall population totals",
        color = "darkblue",
        smooth_color = "darkgreen",
        smooth_fill = "lightgreen"
      )
    }
  })
  
  # Calculate correlation coefficient
  correlation_text <- reactive({
    correlation <- cor.test(merged_data_education[[selected_factor()$column]], 
                            merged_data_education[[input$crime_type_education]])
    paste0(
      "Strength of Relationship:\n",
      "Pearson's r: ", round(correlation$estimate, 3), "\n"
    )
  })
  
  output$correlation <- renderText({
    correlation_text()
  })
  
  output$crime_plot_education <- renderPlotly({
    factor_info <- selected_factor()
    
    cor_value <- round(cor(merged_data_education[[factor_info$column]], 
                           merged_data_education[[input$crime_type_education]]), 3)
    
    p <- ggplot(merged_data_education, aes_string(x = factor_info$column, 
                                                  y = input$crime_type_education)) +
      geom_point(aes(text = state), color = factor_info$color, alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", color = factor_info$smooth_color, 
                  fill = factor_info$smooth_fill, alpha = 0.2) +
      labs(
        title = paste("Relationship Between", factor_info$title_prefix, 
                      "and", input$crime_type_education, "Rate by State"),
        subtitle = paste0(
          factor_info$subtitle, "\n",
          "Correlation coefficient (r) = ", cor_value
        ),
        x = factor_info$label,
        y = paste(input$crime_type_education, "Rate (per 100,000 population)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray50"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        title = list(font = list(size = 14)),
        margin = list(b = 100)
      )
  })
}
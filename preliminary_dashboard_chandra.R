# UI
nav_panel("Population and Crime Visualizations",
          sidebarLayout(
            sidebarPanel(
              helpText("Visualizations showing relationships between population size and crime rate.")
            ),
            mainPanel(
              plotOutput(outputId = "crime_rate_population_violin", height = "400px"),
              plotOutput(outputId = "population_vs_crime_rate", height = "400px")
            )
          )
),

# Functions
output$crime_rate_population_violin <- renderPlot({
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
})

output$population_vs_crime_rate <- renderPlot({
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
    )
})


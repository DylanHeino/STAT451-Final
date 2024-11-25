# Load Shiny and ggplot2
library(shiny)
library(ggplot2)

# Load data
crimedata <- read.csv("crimedata.csv")

# UI function
ui <- fluidPage(
  titlePanel("Economic Factors and Crime Rates"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Choose an economic factor:",
        choices = c("Median Income" = "medIncome", 
                    "Percentage of Population Under Poverty" = "PctPopUnderPov"),
        selected = "medIncome"
      ),
      radioButtons(
        inputId = "crime_type",
        label = "Select type of crime rate to visualize:",
        choices = c("Violent Crimes Per Population" = "ViolentCrimesPerPop",
                    "Non-Violent Crimes Per Population" = "nonViolPerPop"),
        selected = "ViolentCrimesPerPop"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterPlot"),
      textOutput(outputId = "summaryText")
    )
  )
)

# Server function
server <- function(input, output) {
  
  # Generate scatter plot based on user input
  output$scatterPlot <- renderPlot({
    # Dynamic labels
    variable_label <- ifelse(input$variable == "medIncome", 
                             "Median Income", 
                             "Percentage of Population Under Poverty")
    crime_label <- ifelse(input$crime_type == "ViolentCrimesPerPop",
                          "Violent Crimes Per Population",
                          "Non-Violent Crimes Per Population")
    
    # Choose color based on the selected crime type
    plot_color <- if (input$crime_type == "ViolentCrimesPerPop") {
      "orange"
    } else {
      "blue"
    }
    
    # Generate scatter plot with updated font sizes and dynamic title
    ggplot(crimedata, aes_string(x = input$variable, y = input$crime_type)) +
      geom_point(alpha = 0.6, color = plot_color) +
      labs(
        x = variable_label,
        y = crime_label,
        title = paste("Relationship Between", variable_label, "and", crime_label)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
  })
  
  # Generate a summary of the selected data
  output$summaryText <- renderText({
    selected_var <- ifelse(input$variable == "medIncome", 
                           "Median Income", 
                           "Percentage of Population Under Poverty")
    crime_rate <- ifelse(input$crime_type == "ViolentCrimesPerPop",
                         "Violent Crimes Per Population",
                         "Non-Violent Crimes Per Population")
    paste("Displaying the relationship between", selected_var, "and", crime_rate, ".")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



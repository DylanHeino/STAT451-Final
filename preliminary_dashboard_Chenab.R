library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


crime_data <- read.csv("/Users/chenab/Downloads/US Violent Crime Dataset (3).csv")

# UI
ui <- fluidPage(
  titlePanel("Education Level and Crime Rate Analysis by State"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "crime_type",
        label = "Select Crime Type:",
        choices = c("Murder", "Assault", "Rape"),
        selected = "Murder"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "crime_plot", height = "600px"),
      verbatimTextOutput(outputId = "correlation")  # Added output for correlation
    )
  )
)

# Server
server <- function(input, output) {
  # Calculate correlation coefficient
  correlation_text <- reactive({
    correlation <- cor.test(crime_data$Education, crime_data[[input$crime_type]])
    paste0(
      "Strength of Relationship:\n",
      "Pearson's r: ", round(correlation$estimate, 3), "\n"
    )
  })
  
  output$correlation <- renderText({
    correlation_text()
  })
  
  output$crime_plot <- renderPlotly({

    cor_value <- round(cor(crime_data$Education, crime_data[[input$crime_type]]), 3)
    

    p <- ggplot(crime_data, aes_string(x = "Education", y = input$crime_type)) +
      geom_point(aes(text = State), color = "darkblue", alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.2) +
      labs(
        title = paste("Relationship Between Education Level and", input$crime_type, "Rate by State"),
        subtitle = paste0(
          "Higher education measured as percentage of population with bachelor's degree or higher\n",
          "Correlation coefficient (r) = ", cor_value
        ),
        x = "Population with Bachelor's Degree or Higher (%)",
        y = paste(input$crime_type, "Rate (per 100,000 population)")
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


shinyApp(ui, server)
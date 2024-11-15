library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

unemployment_crime_data <- read.csv("/Users/edisonlu/Downloads/crimebystatecombinedwithunemployment.csv")

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
  nav_panel("Another Visualization"),
  nav_panel("Yet another visualization"),
  nav_panel("Still more visualization!"),
  title = "Visualization of Crime Data in the U.S.",
  id = "page"
)

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
  
}

shinyApp(ui, server)
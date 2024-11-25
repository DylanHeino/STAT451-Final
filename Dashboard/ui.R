library(shiny)

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
                  min = round(min(state_means$Population)-10000),
                  max = round(max(state_means$Population)+10000),
                  value = c(round(min(state_means$Population)-10000), round(max(state_means$Population)+10000)),
                  step = 10000
                ),
                sliderInput(
                  inputId = "Unemployment Rate",
                  label = "Select range for unemployment rate:",
                  min = round(min(state_means$unemployment)-0.5),
                  max = round(max(state_means$unemployment)+0.5),
                  value = c(round(min(state_means$unemployment)-0.5), round(max(state_means$unemployment)+0.5)),
                  step = 0.1
                )
              ),
              
              mainPanel(
                plotOutput(outputId = "crime_unemployment_plot", height = "400px",
                           hover = hoverOpts(id = "plot_hover",
                                             delayType = "throttle")),
                verbatimTextOutput(outputId = "hover_info"),
                plotOutput(outputId = "detail_violent_crime_plot", height = "400px")
              )
              
            )
  )
  
)

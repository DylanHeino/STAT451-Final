ui <- page_navbar(
  nav_panel("Crime Rate Map",
            sidebarLayout(
              
              sidebarPanel(
                radioButtons(inputId = "region_selector",
                             label = "Choose a region to visualize crime rates",
                             choices = c("All", "New England", "Mid Atlantic", "East North Central",
                                         "West North Central", "South Atlantic", "East South Central",
                                         "West South Central", "Mountain", "Pacific"),
                             selected = "All")
              ),
              
              mainPanel(
                plotOutput(outputId = "crime_rate_map_us", height = "600px")
              )
              
            )
  ),
  nav_panel("Population Size and Crime",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "plot_choice",
                  label = "Select Plot:",
                  choices = c("Violin Plot", "Scatter Plot"),
                  selected = "Violin Plot"
                )
              ),
              mainPanel(
                plotOutput(outputId = "crime_rate_plots_population", height = "600px")
              )
            )
  ),
  nav_panel("Economic Factors and Crime",
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
                  inputId = "crime_type_economics",
                  label = "Select type of crime rate to visualize:",
                  choices = c("Violent Crimes Per Population" = "ViolentCrimesPerPop",
                              "Non-Violent Crimes Per Population" = "nonViolPerPop"),
                  selected = "ViolentCrimesPerPop"
                )
              ),
              
              mainPanel(
                plotOutput(outputId = "scatterPlot", height = "600px"),
                textOutput(outputId = "summaryText")
              )
            )
  ),
  nav_panel("Unemployment and Crime",
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
  ),
  

  nav_panel("Education and Crime Rate",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "crime_type_education",
                  label = "Select Crime Type:",
                  choices = c("Murder", "Assault", "Rape"),
                  selected = "Murder"
                )
              ),
              mainPanel(
                plotlyOutput(outputId = "crime_plot_education", height = "600px"),
                verbatimTextOutput(outputId = "correlation")  # Added output for correlation
              )
            )
            ),
  # Medicare Implementation
  nav_panel("Medicare Enrollement and Crime",
            sidebarLayout(
              sidebarPanel(
                # Move crime_type selectInput above state_search
                selectInput("crime_type", "Select Crime Type", 
                            choices = c("Murder Rate", "Assault Rate", "Rape Rate")),
                
                # Add placeholder text to state_search
                textInput("state_search", "Enter State Abbreviation:", 
                          placeholder = "Input State Abbreviation (ex. CA, NY, WA, etc.)")
              ),
              mainPanel(
                # Conditionally show the crime rate value text only when a state is entered
                conditionalPanel(
                  condition = "input.state_search != ''",
                  textOutput("crime_rate_value"),
                  style = "text-align: center; font-size: 24px; font-weight: bold;"
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

  
  
  title = "Factors Influencing Crime",
  id = "page"
)
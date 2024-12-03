library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Read and prepare data
crime_data <- read.csv("/Users/chenab/Downloads/US Violent Crime Dataset (2).csv")
alcohol_data <- read.csv("/Users/chenab/Downloads/Alcohol Consumption by State 2024.csv")
homelessness_data <- read.csv("/Users/chenab/Downloads/Homeless Population by State 2024.csv")
drug_use_data <- read.csv("/Users/chenab/Downloads/Drug Use by State 2024.csv")
education_data <- read.csv("/Users/chenab/Downloads/Most Educated States 2024 (1).csv")

# Filter out DC from all datasets
filtered_alcohol_data <- alcohol_data[alcohol_data$state != "District of Columbia", ]
filtered_homelessness_data <- homelessness_data[homelessness_data$state != "District of Columbia", ]
filtered_drug_use_data <- drug_use_data[drug_use_data$state != "District of Columbia", ]
filtered_education_data <- education_data[education_data$state != "District of Columbia", ]

# Rename state column in crime data
names(crime_data)[1] <- "state"

# Merge all datasets
merged_data <- crime_data %>%
  merge(filtered_alcohol_data, by = "state") %>%
  merge(filtered_homelessness_data, by = "state") %>%
  merge(filtered_drug_use_data, by = "state") %>%
  merge(filtered_education_data, by = "state")

# UI
ui <- fluidPage(
  titlePanel("Comprehensive Crime Rate Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_type",
        label = "Select Analysis Type:",
        choices = c("Social Factors" = "social", 
                    "Education Level" = "education"),
        selected = "social"
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'social'",
        selectInput(
          inputId = "factor_type",
          label = "Select Social Factor:",
          choices = c("Alcohol Consumption" = "alcohol", 
                      "Homelessness" = "homelessness",
                      "Drug Use" = "drug"),
          selected = "alcohol"
        )
      ),
      selectInput(
        inputId = "crime_type",
        label = "Select Crime Type:",
        choices = c("Murder", "Assault", "Rape"),
        selected = "Murder"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "crime_plot", height = "600px"),
      verbatimTextOutput(outputId = "correlation")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive values for factor selection
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
    correlation <- cor.test(merged_data[[selected_factor()$column]], 
                            merged_data[[input$crime_type]])
    paste0(
      "Strength of Relationship:\n",
      "Pearson's r: ", round(correlation$estimate, 3), "\n"
    )
  })
  
  output$correlation <- renderText({
    correlation_text()
  })
  
  output$crime_plot <- renderPlotly({
    factor_info <- selected_factor()
    
    cor_value <- round(cor(merged_data[[factor_info$column]], 
                           merged_data[[input$crime_type]]), 3)
    
    p <- ggplot(merged_data, aes_string(x = factor_info$column, 
                                        y = input$crime_type)) +
      geom_point(aes(text = state), color = factor_info$color, alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", color = factor_info$smooth_color, 
                  fill = factor_info$smooth_fill, alpha = 0.2) +
      labs(
        title = paste("Relationship Between", factor_info$title_prefix, 
                      "and", input$crime_type, "Rate by State"),
        subtitle = paste0(
          factor_info$subtitle, "\n",
          "Correlation coefficient (r) = ", cor_value
        ),
        x = factor_info$label,
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
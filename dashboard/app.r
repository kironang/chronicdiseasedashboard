library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(shinyWidgets)

# Load data
data <- read_csv("../data/data.csv")

ui <- fluidPage(

  titlePanel("Health Data Explorer"),
  
  # Dropdown 1: Category
  selectInput("category", "Select Category", choices = unique(data$category)),
  
  # Conditional UI for Indicator
  uiOutput("indicator_ui"),
  
  # Conditional UI for Group
  uiOutput("group_ui"),
  
  # Conditional UI for Years (inside dropdown)
  uiOutput("year_ui"),
  
  # Plot output
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  
  # Filter indicators based on selected category
  filtered_indicators <- reactive({
    req(input$category)
    data %>%
      filter(category == input$category) %>%
      distinct(indicator) %>%
      pull(indicator)
  })
  
  output$indicator_ui <- renderUI({
    req(filtered_indicators())
    selectInput("indicator", "Select Indicator", choices = filtered_indicators())
  })
  
  # Filter groups based on selected indicator
  filtered_groups <- reactive({
    req(input$category, input$indicator)
    data %>%
      filter(category == input$category, indicator == input$indicator) %>%
      distinct(group) %>%
      pull(group)
  })
  
  output$group_ui <- renderUI({
    req(filtered_groups())
    selectInput("group", "Select Group", choices = filtered_groups())
  })
  
  # Filter years based on selected group
  filtered_years <- reactive({
    req(input$category, input$indicator, input$group)
    data %>%
      filter(category == input$category,
             indicator == input$indicator,
             group == input$group) %>%
      distinct(year) %>%
      pull(year)
  })
  
  output$year_ui <- renderUI({
    req(filtered_years())
    
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years", "Select Years",
                         choices = sort(filtered_years()),
                         selected = sort(filtered_years()))
    )
  })
  
  output$plot <- renderPlotly({
    req(input$years)
    
    plot_data <- data %>%
      filter(category == input$category,
             indicator == input$indicator,
             group == input$group,
             year %in% input$years) %>%
      arrange(year)
    
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value') %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(0,100,80,0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = paste("Indicator:", input$indicator),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year"))
  })
}

shinyApp(ui = ui, server = server)

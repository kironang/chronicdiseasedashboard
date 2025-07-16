library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(shinyWidgets)

# Load data
data <- read_csv("../data/data.csv")

ui <- fluidPage(
  
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    
    # ---- Section 1: Population & Access ----
    tabPanel("Population & Access",
             selectInput("category1", "Select Category", 
                         choices = c("Social Determinants of Health", 
                                     "Childcare and Education", 
                                     "Access to Care", 
                                     "Population Health", 
                                     "Demographics")),
             uiOutput("indicator_ui1"),
             uiOutput("group_ui1"),
             uiOutput("year_ui1"),
             plotlyOutput("plot1")
    ),
    
    # ---- Section 2: Mortality ----
    tabPanel("Mortality",
             selectInput("category2", "Select Category", choices = c("Mortality")),
             uiOutput("indicator_ui2"),
             uiOutput("group_ui2"),
             uiOutput("year_ui2"),
             plotlyOutput("plot2")
    ),
    
    # ---- Section 3: Disease & Lifestyle ----
    tabPanel("Disease & Lifestyle",
             selectInput("category3", "Select Category", choices = c("Disease and Lifestyle Conditions")),
             uiOutput("indicator_ui3"),
             uiOutput("group_ui3"),
             uiOutput("year_ui3"),
             plotlyOutput("plot3")
    ),
    
    # ---- Section 4: More Information ----
    tabPanel("More Information",
             h3("About this Dashboard"),
             p("This dashboard allows users to explore public health indicators segmented into different themes."),
             p("Use the tabs to navigate between areas like Mortality, Disease & Lifestyle Conditions, and more."),
             p("Data is sourced from public health records. Each section is interactive and allows filtering by group and year.")
    )
  )
)

server <- function(input, output, session) {
  
  # ========== Section 1 Logic ==========
  filtered_indicators1 <- reactive({
    req(input$category1)
    data %>%
      filter(category == input$category1) %>%
      distinct(indicator) %>%
      pull()
  })
  
  output$indicator_ui1 <- renderUI({
    selectInput("indicator1", "Select Indicator", choices = filtered_indicators1())
  })
  
  filtered_groups1 <- reactive({
    req(input$category1, input$indicator1)
    data %>%
      filter(category == input$category1, indicator == input$indicator1) %>%
      distinct(group) %>%
      pull()
  })
  
  output$group_ui1 <- renderUI({
    selectInput("group1", "Select Group", choices = filtered_groups1())
  })
  
  filtered_years1 <- reactive({
    req(input$category1, input$indicator1, input$group1)
    data %>%
      filter(category == input$category1,
             indicator == input$indicator1,
             group == input$group1) %>%
      distinct(year) %>%
      pull()
  })
  
  output$year_ui1 <- renderUI({
    req(filtered_years1())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years1", "Select Years",
                         choices = sort(filtered_years1()),
                         selected = sort(filtered_years1()))
    )
  })
  
  output$plot1 <- renderPlotly({
    req(input$years1)
    plot_data <- data %>%
      filter(category == input$category1,
             indicator == input$indicator1,
             group == input$group1,
             year %in% input$years1) %>%
      arrange(year)
    
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value') %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(0,100,80,0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = paste("Indicator:", input$indicator1),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year"))
  })
  
  # ========== Section 2 Logic ==========
  filtered_indicators2 <- reactive({
    req(input$category2)
    data %>%
      filter(category == input$category2) %>%
      distinct(indicator) %>%
      pull()
  })
  
  output$indicator_ui2 <- renderUI({
    selectInput("indicator2", "Select Indicator", choices = filtered_indicators2())
  })
  
  filtered_groups2 <- reactive({
    req(input$category2, input$indicator2)
    data %>%
      filter(category == input$category2, indicator == input$indicator2) %>%
      distinct(group) %>%
      pull()
  })
  
  output$group_ui2 <- renderUI({
    selectInput("group2", "Select Group", choices = filtered_groups2())
  })
  
  filtered_years2 <- reactive({
    req(input$category2, input$indicator2, input$group2)
    data %>%
      filter(category == input$category2,
             indicator == input$indicator2,
             group == input$group2) %>%
      distinct(year) %>%
      pull()
  })
  
  output$year_ui2 <- renderUI({
    req(filtered_years2())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years2", "Select Years",
                         choices = sort(filtered_years2()),
                         selected = sort(filtered_years2()))
    )
  })
  
  output$plot2 <- renderPlotly({
    req(input$years2)
    plot_data <- data %>%
      filter(category == input$category2,
             indicator == input$indicator2,
             group == input$group2,
             year %in% input$years2) %>%
      arrange(year)
    
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value') %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(255,0,0,0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = paste("Indicator:", input$indicator2),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year"))
  })
  
  # ========== Section 3 Logic ==========
  filtered_indicators3 <- reactive({
    req(input$category3)
    data %>%
      filter(category == input$category3) %>%
      distinct(indicator) %>%
      pull()
  })
  
  output$indicator_ui3 <- renderUI({
    selectInput("indicator3", "Select Indicator", choices = filtered_indicators3())
  })
  
  filtered_groups3 <- reactive({
    req(input$category3, input$indicator3)
    data %>%
      filter(category == input$category3, indicator == input$indicator3) %>%
      distinct(group) %>%
      pull()
  })
  
  output$group_ui3 <- renderUI({
    selectInput("group3", "Select Group", choices = filtered_groups3())
  })
  
  filtered_years3 <- reactive({
    req(input$category3, input$indicator3, input$group3)
    data %>%
      filter(category == input$category3,
             indicator == input$indicator3,
             group == input$group3) %>%
      distinct(year) %>%
      pull()
  })
  
  output$year_ui3 <- renderUI({
    req(filtered_years3())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years3", "Select Years",
                         choices = sort(filtered_years3()),
                         selected = sort(filtered_years3()))
    )
  })
  
  output$plot3 <- renderPlotly({
    req(input$years3)
    plot_data <- data %>%
      filter(category == input$category3,
             indicator == input$indicator3,
             group == input$group3,
             year %in% input$years3) %>%
      arrange(year)
    
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value') %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(0,0,255,0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = paste("Indicator:", input$indicator3),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year"))
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(shinyWidgets)
data <- read_csv("../data/data.csv")
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f2f5;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #2c3e50;
      }
      .container {
        max-width: 1100px;
        margin: 0 auto;
        padding: 20px;
      }
      h1, h2, h3 {
        font-weight: 600;
        color: #1a1a1a;
      }
      .card {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .plot-source {
        font-size: 0.85em;
        color: #888888;
        margin-top: 10px;
        text-align: center;
      }
      .dropdown-menu {
        max-height: 300px;
        overflow-y: auto;
      }
      .tab-content {
        padding-top: 20px;
      }
    "))
  ),
  div(class = "container",
      titlePanel("McLennan County Dashboard"),
      tabsetPanel(type = "tabs",
                  tabPanel("Population & Access",
                           div(class = "card",
                               fluidRow(
                                 column(12,
                                        selectInput("category1", "Select Category",
                                                    choices = c("Social Determinants of Health",
                                                                "Childcare and Education",
                                                                "Access to Care",
                                                                "Population Health",
                                                                "Demographics"))
                                 ),
                                 column(12, uiOutput("indicator_ui1")),
                                 column(12, uiOutput("group_ui1")),
                                 column(12, uiOutput("year_ui1")),
                                 column(12, plotlyOutput("plot1")),
                                 column(12, div(textOutput("source1"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("Mortality",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category2", "Select Category", choices = c("Mortality"))),
                                 column(12, uiOutput("indicator_ui2")),
                                 column(12, uiOutput("group_ui2")),
                                 column(12, uiOutput("year_ui2")),
                                 column(12, plotlyOutput("plot2")),
                                 column(12, div(textOutput("source2"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("Disease & Lifestyle",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category3", "Select Category", choices = c("Disease and Lifestyle Conditions"))),
                                 column(12, uiOutput("indicator_ui3")),
                                 column(12, uiOutput("group_ui3")),
                                 column(12, uiOutput("year_ui3")),
                                 column(12, plotlyOutput("plot3")),
                                 column(12, div(textOutput("source3"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("More Information",
                           div(class = "card",
                               h3("About this Dashboard"),
                               p("PLACEHOLDER"),
                           )
                  )
      )
  )
)
server <- function(input, output, session) {
  get_unit_source <- function(ind, grp) {
    d <- data %>% filter(indicator == ind, group == grp)
    unit <- unique(d$unit)
    source <- unique(d$source)
    unit <- if(length(unit) == 1) unit else ""
    source <- if(length(source) == 1) source else ""
    list(unit = unit, source = source)
  }
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
    req(input$indicator1)
    data %>%
      filter(indicator == input$indicator1) %>%
      distinct(group) %>%
      pull()
  })
  output$group_ui1 <- renderUI({
    selectInput("group1", "Select Group", choices = filtered_groups1())
  })
  filtered_years1 <- reactive({
    req(input$indicator1, input$group1)
    data %>%
      filter(indicator == input$indicator1, group == input$group1) %>%
      distinct(year) %>%
      pull()
  })
  output$year_ui1 <- renderUI({
    req(filtered_years1())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years1", "Years",
                         choices = sort(filtered_years1()),
                         selected = sort(filtered_years1()))
    )
  })
  output$plot1 <- renderPlotly({
    req(input$years1)
    plot_data <- data %>%
      filter(indicator == input$indicator1,
             group == input$group1,
             year %in% input$years1) %>%
      arrange(year)
    us <- get_unit_source(input$indicator1, input$group1)
    title_text <- paste0(input$indicator1, ifelse(us$unit != "", paste0(" (", us$unit, ")"), ""))
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value', line = list(color = '#2C3E50')) %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(44, 62, 80, 0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = list(text = title_text, x = 0.5),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year", dtick = 1))
  })
  output$source1 <- renderText({
    req(input$indicator1, input$group1)
    us <- get_unit_source(input$indicator1, input$group1)
    if(us$source != "") paste("Source:", us$source) else ""
  })
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
    req(input$indicator2)
    data %>%
      filter(indicator == input$indicator2) %>%
      distinct(group) %>%
      pull()
  })
  output$group_ui2 <- renderUI({
    selectInput("group2", "Select Group", choices = filtered_groups2())
  })
  filtered_years2 <- reactive({
    req(input$indicator2, input$group2)
    data %>%
      filter(indicator == input$indicator2, group == input$group2) %>%
      distinct(year) %>%
      pull()
  })
  output$year_ui2 <- renderUI({
    req(filtered_years2())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years2", "Years",
                         choices = sort(filtered_years2()),
                         selected = sort(filtered_years2()))
    )
  })
  output$plot2 <- renderPlotly({
    req(input$years2)
    plot_data <- data %>%
      filter(indicator == input$indicator2,
             group == input$group2,
             year %in% input$years2) %>%
      arrange(year)
    us <- get_unit_source(input$indicator2, input$group2)
    title_text <- paste0(input$indicator2, ifelse(us$unit != "", paste0(" (", us$unit, ")"), ""))
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value', line = list(color = '#C0392B')) %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(192, 57, 43, 0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = list(text = title_text, x = 0.5),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year", dtick = 1))
  })
  output$source2 <- renderText({
    req(input$indicator2, input$group2)
    us <- get_unit_source(input$indicator2, input$group2)
    if(us$source != "") paste("Source:", us$source) else ""
  })
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
    req(input$indicator3)
    data %>%
      filter(indicator == input$indicator3) %>%
      distinct(group) %>%
      pull()
  })
  output$group_ui3 <- renderUI({
    selectInput("group3", "Select Group", choices = filtered_groups3())
  })
  filtered_years3 <- reactive({
    req(input$indicator3, input$group3)
    data %>%
      filter(indicator == input$indicator3, group == input$group3) %>%
      distinct(year) %>%
      pull()
  })
  output$year_ui3 <- renderUI({
    req(filtered_years3())
    dropdownButton(
      label = "Select Years", status = "primary", circle = FALSE,
      checkboxGroupInput("years3", "Years",
                         choices = sort(filtered_years3()),
                         selected = sort(filtered_years3()))
    )
  })
  output$plot3 <- renderPlotly({
    req(input$years3)
    plot_data <- data %>%
      filter(indicator == input$indicator3,
             group == input$group3,
             year %in% input$years3) %>%
      arrange(year)
    us <- get_unit_source(input$indicator3, input$group3)
    title_text <- paste0(input$indicator3, ifelse(us$unit != "", paste0(" (", us$unit, ")"), ""))
    plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = 'Value', line = list(color = '#2980B9')) %>%
      add_ribbons(ymin = ~lower, ymax = ~upper,
                  fillcolor = 'rgba(41, 128, 185, 0.2)',
                  line = list(color = 'transparent'),
                  name = 'Uncertainty') %>%
      layout(title = list(text = title_text, x = 0.5),
             yaxis = list(title = "Value"),
             xaxis = list(title = "Year", dtick = 1))
  })
  output$source3 <- renderText({
    req(input$indicator3, input$group3)
    us <- get_unit_source(input$indicator3, input$group3)
    if(us$source != "") paste("Source:", us$source) else ""
  })
}
shinyApp(ui = ui, server = server)

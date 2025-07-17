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
                                 column(12, selectInput("category1", "Select Category",
                                                        choices = unique(data$category))),
                                 column(12, uiOutput("indicator_ui1")),
                                 column(4, uiOutput("age_ui1")),
                                 column(4, uiOutput("sex_ui1")),
                                 column(4, uiOutput("race_ui1")),
                                 column(12, uiOutput("year_ui1")),
                                 column(12, plotlyOutput("plot1")),
                                 column(12, div(textOutput("source1"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("Mortality",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category2", "Select Category",
                                                        choices = unique(data$category))),
                                 column(12, uiOutput("indicator_ui2")),
                                 column(4, uiOutput("age_ui2")),
                                 column(4, uiOutput("sex_ui2")),
                                 column(4, uiOutput("race_ui2")),
                                 column(12, uiOutput("year_ui2")),
                                 column(12, plotlyOutput("plot2")),
                                 column(12, div(textOutput("source2"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("Disease & Lifestyle",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category3", "Select Category",
                                                        choices = unique(data$category))),
                                 column(12, uiOutput("indicator_ui3")),
                                 column(4, uiOutput("age_ui3")),
                                 column(4, uiOutput("sex_ui3")),
                                 column(4, uiOutput("race_ui3")),
                                 column(12, uiOutput("year_ui3")),
                                 column(12, plotlyOutput("plot3")),
                                 column(12, div(textOutput("source3"), class = "plot-source"))
                               )
                           )
                  ),
                  tabPanel("More Information",
                           div(class = "card",
                               h3("About this Dashboard"),
                               p("PLACEHOLDER")
                           )
                  )
      )
  )
)

server <- function(input, output, session) {
  get_unit_source <- function(ind, age, sex, race) {
    d <- data %>% filter(indicator == ind, age == age, sex == sex, race == race)
    unit <- unique(d$unit)
    source <- unique(d$source)
    unit <- if(length(unit) == 1) unit else ""
    source <- if(length(source) == 1) source else ""
    list(unit = unit, source = source)
  }
  
  # Panel 1
  filtered_indicators1 <- reactive({
    req(input$category1)
    data %>% filter(category == input$category1) %>% distinct(indicator) %>% pull()
  })
  output$indicator_ui1 <- renderUI({
    selectInput("indicator1", "Select Indicator", choices = filtered_indicators1())
  })
  output$age_ui1 <- renderUI({
    req(input$indicator1)
    selectInput("age1", "Select Age", choices = data %>% filter(indicator == input$indicator1) %>% distinct(age) %>% pull())
  })
  output$sex_ui1 <- renderUI({
    req(input$indicator1)
    selectInput("sex1", "Select Sex", choices = data %>% filter(indicator == input$indicator1) %>% distinct(sex) %>% pull())
  })
  output$race_ui1 <- renderUI({
    req(input$indicator1)
    selectInput("race1", "Select Race", choices = data %>% filter(indicator == input$indicator1) %>% distinct(race) %>% pull())
  })
  filtered_years1 <- reactive({
    req(input$indicator1, input$age1, input$sex1, input$race1)
    data %>%
      filter(indicator == input$indicator1,
             age == input$age1,
             sex == input$sex1,
             race == input$race1) %>%
      distinct(year) %>% pull()
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
             age == input$age1,
             sex == input$sex1,
             race == input$race1,
             year %in% input$years1) %>%
      arrange(year)
    us <- get_unit_source(input$indicator1, input$age1, input$sex1, input$race1)
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
    req(input$indicator1, input$age1, input$sex1, input$race1)
    us <- get_unit_source(input$indicator1, input$age1, input$sex1, input$race1)
    if(us$source != "") paste("Source:", us$source) else ""
  })
  
  # Panel 2
  filtered_indicators2 <- reactive({
    req(input$category2)
    data %>% filter(category == input$category2) %>% distinct(indicator) %>% pull()
  })
  output$indicator_ui2 <- renderUI({
    selectInput("indicator2", "Select Indicator", choices = filtered_indicators2())
  })
  output$age_ui2 <- renderUI({
    req(input$indicator2)
    selectInput("age2", "Select Age", choices = data %>% filter(indicator == input$indicator2) %>% distinct(age) %>% pull())
  })
  output$sex_ui2 <- renderUI({
    req(input$indicator2)
    selectInput("sex2", "Select Sex", choices = data %>% filter(indicator == input$indicator2) %>% distinct(sex) %>% pull())
  })
  output$race_ui2 <- renderUI({
    req(input$indicator2)
    selectInput("race2", "Select Race", choices = data %>% filter(indicator == input$indicator2) %>% distinct(race) %>% pull())
  })
  filtered_years2 <- reactive({
    req(input$indicator2, input$age2, input$sex2, input$race2)
    data %>%
      filter(indicator == input$indicator2,
             age == input$age2,
             sex == input$sex2,
             race == input$race2) %>%
      distinct(year) %>% pull()
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
             age == input$age2,
             sex == input$sex2,
             race == input$race2,
             year %in% input$years2) %>%
      arrange(year)
    us <- get_unit_source(input$indicator2, input$age2, input$sex2, input$race2)
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
    req(input$indicator2, input$age2, input$sex2, input$race2)
    us <- get_unit_source(input$indicator2, input$age2, input$sex2, input$race2)
    if(us$source != "") paste("Source:", us$source) else ""
  })
  
  # Panel 3
  filtered_indicators3 <- reactive({
    req(input$category3)
    data %>% filter(category == input$category3) %>% distinct(indicator) %>% pull()
  })
  output$indicator_ui3 <- renderUI({
    selectInput("indicator3", "Select Indicator", choices = filtered_indicators3())
  })
  output$age_ui3 <- renderUI({
    req(input$indicator3)
    selectInput("age3", "Select Age", choices = data %>% filter(indicator == input$indicator3) %>% distinct(age) %>% pull())
  })
  output$sex_ui3 <- renderUI({
    req(input$indicator3)
    selectInput("sex3", "Select Sex", choices = data %>% filter(indicator == input$indicator3) %>% distinct(sex) %>% pull())
  })
  output$race_ui3 <- renderUI({
    req(input$indicator3)
    selectInput("race3", "Select Race", choices = data %>% filter(indicator == input$indicator3) %>% distinct(race) %>% pull())
  })
  filtered_years3 <- reactive({
    req(input$indicator3, input$age3, input$sex3, input$race3)
    data %>%
      filter(indicator == input$indicator3,
             age == input$age3,
             sex == input$sex3,
             race == input$race3) %>%
      distinct(year) %>% pull()
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
             age == input$age3,
             sex == input$sex3,
             race == input$race3,
             year %in% input$years3) %>%
      arrange(year)
    us <- get_unit_source(input$indicator3, input$age3, input$sex3, input$race3)
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
    req(input$indicator3, input$age3, input$sex3, input$race3)
    us <- get_unit_source(input$indicator3, input$age3, input$sex3, input$race3)
    if(us$source != "") paste("Source:", us$source) else ""
  })
}

shinyApp(ui = ui, server = server)

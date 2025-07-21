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
        background-color: #f8f9fb;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #2c3e50;
      }
      .container {
        max-width: 1100px;
        margin: 0 auto;
        padding: 30px;
      }
      h1, h2, h3 {
        font-weight: 600;
        color: #1a1a1a;
      }
      .card {
        background-color: #ffffff;
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 40px;
        box-shadow: 0 6px 18px rgba(0,0,0,0.06);
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
      .plot-source {
        font-size: 1em;
        font-weight: 500;
        color: #2c3e50;
        margin-bottom: 15px;
        padding: 10px;
        background-color: #ecf0f1;
        border-left: 4px solid #3498db;
        border-radius: 5px;
      }
      .dropdown-menu {
        max-height: 300px;
        overflow-y: auto;
      }
      .tab-content {
        padding-top: 20px;
      }
      .plotly {
        border-radius: 8px;
        padding: 5px;
        background-color: #fff;
      }
      .picker-label {
        font-weight: 500;
        color: #34495e;
      }
    "))
  ),
  div(class = "container",
      titlePanel("County Public Health Dashboard"),
      tabsetPanel(type = "tabs",
                  tabPanel("Health",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category1", "Select Category",
                                                        choices = sort(c("Population Health",
                                                                         "Access to Care",
                                                                         "Childcare and Education",
                                                                         "Demographics",
                                                                         "No Category")))),
                                 column(12, uiOutput("indicator_ui1")),
                                 column(4, uiOutput("age_ui1")),
                                 column(4, uiOutput("sex_ui1")),
                                 column(4, uiOutput("race_ui1")),
                                 column(12, uiOutput("year_ui1")),
                                 column(12, div(textOutput("source1"), class = "plot-source")),
                                 column(12, plotlyOutput("plot1"))
                               )
                           )
                  ),
                  tabPanel("Mortality",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category2", "Select Category",
                                                        choices = sort(c("Mortality")))),
                                 column(12, uiOutput("indicator_ui2")),
                                 column(4, uiOutput("age_ui2")),
                                 column(4, uiOutput("sex_ui2")),
                                 column(4, uiOutput("race_ui2")),
                                 column(12, uiOutput("year_ui2")),
                                 column(12, div(textOutput("source2"), class = "plot-source")),
                                 column(12, plotlyOutput("plot2"))
                               )
                           )
                  ),
                  tabPanel("Diseases and Lifestyle Conditions",
                           div(class = "card",
                               fluidRow(
                                 column(12, selectInput("category3", "Select Category",
                                                        choices = sort(c("Diseases and Lifestyle Conditions")))),
                                 column(12, uiOutput("indicator_ui3")),
                                 column(4, uiOutput("age_ui3")),
                                 column(4, uiOutput("sex_ui3")),
                                 column(4, uiOutput("race_ui3")),
                                 column(12, uiOutput("year_ui3")),
                                 column(12, div(textOutput("source3"), class = "plot-source")),
                                 column(12, plotlyOutput("plot3"))
                               )
                           )
                  ),
                  tabPanel("More Information",
                           div(class = "card",
                               h3("About this Dashboard"),
                               p("This dashboard was created to help visualize and explore public health data by county. 
                                  It allows users to select demographic breakdowns and indicators for a wide range of health categories. 
                                  Data sources are displayed for transparency, and all visualizations are interactive and exportable.")
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
  
  # A function to generate repeated logic for each panel
  create_panel_logic <- function(panel_number) {
    cat_input <- paste0("category", panel_number)
    ind_input <- paste0("indicator", panel_number)
    age_input <- paste0("age", panel_number)
    sex_input <- paste0("sex", panel_number)
    race_input <- paste0("race", panel_number)
    years_input <- paste0("years", panel_number)
    year_ui <- paste0("year_ui", panel_number)
    indicator_ui <- paste0("indicator_ui", panel_number)
    age_ui <- paste0("age_ui", panel_number)
    sex_ui <- paste0("sex_ui", panel_number)
    race_ui <- paste0("race_ui", panel_number)
    plot_output <- paste0("plot", panel_number)
    source_output <- paste0("source", panel_number)
    
    filtered_indicators <- reactive({
      req(input[[cat_input]])
      sort(data %>% filter(category == input[[cat_input]]) %>% distinct(indicator) %>% pull())
    })
    output[[indicator_ui]] <- renderUI({
      selectInput(ind_input, "Select Indicator", choices = filtered_indicators())
    })
    output[[age_ui]] <- renderUI({
      req(input[[ind_input]])
      choices <- data %>% filter(indicator == input[[ind_input]]) %>% distinct(age) %>% pull()
      selectInput(age_input, "Select Age", choices = sort(choices))
    })
    output[[sex_ui]] <- renderUI({
      req(input[[ind_input]])
      choices <- data %>% filter(indicator == input[[ind_input]]) %>% distinct(sex) %>% pull()
      selectInput(sex_input, "Select Sex", choices = sort(choices))
    })
    output[[race_ui]] <- renderUI({
      req(input[[ind_input]])
      choices <- data %>% filter(indicator == input[[ind_input]]) %>% distinct(race) %>% pull()
      selectInput(race_input, "Select Race", choices = sort(choices))
    })
    filtered_years <- reactive({
      req(input[[ind_input]], input[[age_input]], input[[sex_input]], input[[race_input]])
      sort(data %>%
             filter(indicator == input[[ind_input]],
                    age == input[[age_input]],
                    sex == input[[sex_input]],
                    race == input[[race_input]]) %>%
             distinct(year) %>% pull())
    })
    output[[year_ui]] <- renderUI({
      req(filtered_years())
      dropdownButton(
        label = "Select Years", status = "primary", circle = FALSE,
        checkboxGroupInput(years_input, "Years",
                           choices = filtered_years(),
                           selected = filtered_years())
      )
    })
    output[[plot_output]] <- renderPlotly({
      req(input[[years_input]])
      plot_data <- data %>%
        filter(indicator == input[[ind_input]],
               age == input[[age_input]],
               sex == input[[sex_input]],
               race == input[[race_input]],
               year %in% input[[years_input]]) %>%
        arrange(year)
      us <- get_unit_source(input[[ind_input]], input[[age_input]], input[[sex_input]], input[[race_input]])
      title_text <- paste0(input[[ind_input]], ifelse(us$unit != "", paste0(" (", us$unit, ")"), ""))
      line_color <- c("#2C3E50", "#C0392B", "#2980B9")[as.integer(panel_number)]
      fill_color <- paste0(substring(line_color, 1, 7), "33")
      
      plot_ly(plot_data, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
              name = 'Value', line = list(color = line_color)) %>%
        add_ribbons(ymin = ~lower, ymax = ~upper,
                    fillcolor = fill_color,
                    line = list(color = 'transparent'),
                    name = 'Uncertainty') %>%
        layout(title = list(text = title_text, x = 0.5),
               yaxis = list(title = "Value"),
               xaxis = list(title = "Year", dtick = 1))
    })
    output[[source_output]] <- renderText({
      req(input[[ind_input]], input[[age_input]], input[[sex_input]], input[[race_input]])
      us <- get_unit_source(input[[ind_input]], input[[age_input]], input[[sex_input]], input[[race_input]])
      if(us$source != "") paste("Source:", us$source) else ""
    })
  }
  
  # Apply logic to all panels
  lapply(1:3, function(i) create_panel_logic(i))
}

shinyApp(ui = ui, server = server)

library(shiny)
library(plotly)
library(bslib)
library(DT)
library(dplyr)
library(readr)

# Load data
data <- read_csv("../data/data.csv")  # Replace with your actual file path

# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Cardiovascular Health Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Select Group", choices = unique(data$group), selected = unique(data$group)[1]),
      selectInput("indicator", "Select Indicator", choices = unique(data$indicator), selected = unique(data$indicator)[1]),
      checkboxInput("show_table", "Show Data Table", value = FALSE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", 
                 br(),
                 plotlyOutput("trendPlot", height = "500px")
        ),
        tabPanel("Data Table",
                 conditionalPanel("input.show_table == true",
                                  br(),
                                  DTOutput("dataTable")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data %>%
      filter(group == input$group, indicator == input$indicator)
  })
  
  output$trendPlot <- renderPlotly({
    df <- filtered_data()
    
    plot_ly(df, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers',
            name = "Estimate", line = list(color = '#0072B2')) %>%
      add_ribbons(ymin = ~lower, ymax = ~upper, name = "95% CI",
                  line = list(color = 'rgba(0,114,178,0.2)'),
                  fillcolor = 'rgba(0,114,178,0.2)') %>%
      layout(title = paste(input$indicator, "in", input$group),
             xaxis = list(title = "Year"),
             yaxis = list(title = paste("Rate (", df$unit[1], ")", sep = "")),
             hovermode = "x unified")
  })
  
  output$dataTable <- renderDT({
    filtered_data()
  })
}

# Run the app
shinyApp(ui, server)

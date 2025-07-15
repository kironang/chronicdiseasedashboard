library(shiny)
library(tidyverse)
library(plotly)
library(scales)  # for axis formatting

# Load data
data <- read_csv("../data/data.csv", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("📊 Health Indicator Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Choose Indicator:", choices = sort(unique(data$indicator))),
      uiOutput("group_ui")
    ),
    mainPanel(
      plotlyOutput("time_series")
    )
  )
)

server <- function(input, output, session) {
  # Update group dropdown based on selected indicator
  output$group_ui <- renderUI({
    req(input$indicator)
    groups <- data %>%
      filter(indicator == input$indicator) %>%
      pull(group) %>%
      unique() %>%
      sort()
    selectInput("group", "Choose Group:", choices = groups)
  })
  
  # Render the plot
  output$time_series <- renderPlotly({
    req(input$indicator, input$group)
    
    plot_data <- data %>%
      filter(indicator == input$indicator, group == input$group) %>%
      arrange(year)
    
    p <- ggplot(plot_data, aes(x = year, y = value)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
      geom_line(color = "#2c3e50", size = 1.2) +
      geom_point(color = "#2980b9", size = 2) +
      scale_x_continuous(breaks = unique(plot_data$year)) +
      labs(
        title = paste(input$indicator, "in", input$group),
        x = "Year",
        y = paste0("Value (", unique(plot_data$unit), ")")
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

shinyApp(ui, server)

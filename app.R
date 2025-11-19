# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(Metrics)

# Load your pre-processed combined_df and cpi_combined_all here
# For now, we will assume they are calculated once and reused
source("forecast_model.R")  # Put your full script in a file named forecast_model.R

ui <- fluidPage(
  titlePanel("Indonesia CPI Forecast Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Forecast Model:", 
                  choices = c("MoM Forecast", "YoY Forecast")),
      helpText("This app compares actual CPI against baseline and custom forecasts.")
    ),
    
    mainPanel(
      plotOutput("cpiPlot"),
      verbatimTextOutput("metricsText")
    )
  )
)

server <- function(input, output, session) {
  
  output$cpiPlot <- renderPlot({
    if (input$model == "MoM Forecast") {
      ggplot(combined_df %>% filter(date >= as.Date("2022-01-01")), aes(x = date, y = cpi, color = type)) +
        geom_line(size = 1) +
        geom_point() +
        geom_vline(xintercept = as.Date("2025-07-01"), linetype = "dashed", color = "black") +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(as.Date("2022-01-01"), as.Date("2026-06-01"))) +
        labs(title = "CPI MoM: Actual vs Baseline VAR vs Custom Forecast", x = "Date", y = "CPI MoM (%)", color = "Legend") +
        theme_minimal()
    } else {
      ggplot(cpi_combined_all, aes(x = Date, y = cpi_yoy, color = type)) +
        geom_line(size = 1) +
        geom_point(size = 1.2, alpha = 0.7) +
        geom_vline(xintercept = as.Date("2025-07-01"), linetype = "dashed", color = "black") +
        annotate("text", x = as.Date("2025-07-01"), y = max(cpi_combined_all$cpi_yoy, na.rm = TRUE), 
                 label = "Forecast Start", vjust = -1, hjust = -0.1, angle = 90, size = 3.5) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", limits = c(as.Date("2022-01-01"), as.Date("2026-06-01"))) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_color_manual(values = c("Actual" = "firebrick", "Baseline Forecast" = "forestgreen", "Custom Forecast" = "dodgerblue")) +
        labs(title = "CPI YoY: Actual vs Forecasts", subtitle = "Indonesia, forecast starts July 2025", 
             x = "Date", y = "CPI YoY (%)", color = "Series") +
        theme_minimal(base_size = 12)
    }
  })
  
  output$metricsText <- renderPrint({
    if (input$model == "YoY Forecast") {
      cat("RMSE:", round(rmse_val, 4), "\n")
      cat("MAE :", round(mae_val, 4), "\n")
    } else {
      cat("Click 'YoY Forecast' to see model accuracy metrics.")
    }
  })
}

shinyApp(ui, server)

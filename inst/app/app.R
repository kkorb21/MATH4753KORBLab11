#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Korb Lab 11: Non-Symmetric Confidence Interval Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupvar", "Choose grouping variable:", choices = c("RIVER", "SPECIES")),
      selectInput("quantvar", "Choose quantitative variable:", choices = c("LENGTH", "WEIGHT", "DDT")),
      sliderInput("alpha", "Total alpha (significance level):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      sliderInput("alpha2", "Upper tail alpha₂:", min = 0.0, max = 0.1, value = 0.02, step = 0.01),
      numericInput("mu0", "Hypothesized mean (μ₀):", value = 45),
      sliderInput("epsilon", "Ball radius (ε):", min = 0.1, max = 10, value = 2, step = 0.1),
      sliderInput("bins", "Number of bins:", min = 10, max = 100, value = 30)
    ),
    mainPanel(
      plotOutput("ciPlot", click = "plot_click"),
      tableOutput("results")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive: filtered data
  filtered_data <- reactive({
    req(input$groupvar, input$quantvar)
    ddt %>% filter(!is.na(.data[[input$quantvar]])) # Safeguard against missing values
  })

  # Reactive: output from ciNonSym()
  ci_result <- reactive({
    x <- filtered_data()[[input$quantvar]]
    ciNonSym(x = x,
             alpha = input$alpha,
             alpha2 = input$alpha2,
             mu0 = input$mu0,
             epsilon = input$epsilon,
             bins = input$bins)
  })

  # Output: table of results
  output$results <- renderTable({
    res <- ci_result()
    data.frame(
      CI_Lower = round(res$ci[1], 3),
      CI_Upper = round(res$ci[2], 3),
      t_Lower = round(res$t[1], 3),
      t_Upper = round(res$t[2], 3),
      Ball_Lower = round(res$ball[1], 3),
      Ball_Upper = round(res$ball[2], 3),
      Contains_Ball = res$test,
      Mu_0 = input$mu0,
      Sample_Mean = round(mean(filtered_data()[[input$quantvar]]), 3),
      Sample_SD = round(sd(filtered_data()[[input$quantvar]]), 3),
      N = length(filtered_data()[[input$quantvar]])
    )
  })

  # Output: plot
  output$ciPlot <- renderPlot({
    x <- filtered_data()[[input$quantvar]]
    ci <- ci_result()$ci
    ball <- ci_result()$ball
    status <- ifelse(x >= ci[1] & x <= ci[2], "in", "out")
    plot_df <- data.frame(x = x, status = status)

    ggplot(plot_df, aes(x = x, fill = status)) +
      geom_histogram(bins = input$bins, color = "black") +
      geom_vline(xintercept = ci, linetype = "dashed", color = "blue", linewidth = 1) +
      geom_vline(xintercept = ball, linetype = "dotted", color = "red", linewidth = 1) +
      labs(title = paste("CI Visualization for", input$quantvar),
           x = input$quantvar, y = "Count", fill = "CI") +
      theme_minimal()
  })

  # Update mu0 when user clicks plot
  observeEvent(input$plot_click, {
    click_x <- round(input$plot_click$x, 2)
    updateNumericInput(session, "mu0", value = click_x)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

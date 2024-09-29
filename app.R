library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # titlePanel("Bernoulli Trials Simulator"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("trials",
                  "Number of Bernoulli Trials:",
                  min = 1,
                  max = 29,
                  value = 2,
                  step = 1),
      
      sliderInput("prob",
                  "Probability of Success:",
                  min = 0,
                  max = 1,
                  value = .5,
                  step = .01),
    ),
    
    mainPanel(
      plotOutput("binomial_plot")
    )
  )
)

server <- function(input, output) {
  
  output$binomial_plot <- renderPlot({
    
    x_vals <- 0:input$trials
    probabilities <- dbinom(x_vals, input$trials, input$prob)
    data <- data.frame(successes = x_vals, probability = probabilities)
    
    ggplot(data, aes(x = successes, y = probability)) +
      geom_bar(stat = "identity", fill = "#336666") +
      labs(x = "Number of Successes", y = "Probability") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(breaks = x_vals) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank())
  }, height = 300, width = 800 )
}


shinyApp(ui, server)
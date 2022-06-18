#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sampling demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "sample_size",
                        label = "Sample size:",
                        min = 1,
                        max = 100,
                        value = 30),
            actionButton(inputId = "run",
                         label = "Draw new sample")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop_size <- 100000
  pop_data <- rnorm(pop_size,100,15) # normal distribution
  theme_set(theme_light())

  sample_data <- reactive({
    sample_IDs <- round(runif(input$sample_size,0,pop_size),0)
    pop_data[sample_IDs]
  })

  output$distPlot <-    renderPlot({
    ggplot() +
      geom_histogram(aes(pop_data),
                     fill = "grey",
                     binwidth = 5) +
      geom_dotplot(aes(x = sample_data()),
                   fill = "yellow",
                   colour = "purple",
                   alpha = .8,
                   binwidth = 5) +
      geom_vline(xintercept = mean(sample_data()),
                 colour = "red")
  })

  output$stats <- renderPrint({
    summary(sample_data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

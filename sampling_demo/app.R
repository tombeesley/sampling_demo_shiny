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
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "pop_sd",
                   label = "Population SD",
                   value = 15,
                   min = 3,
                   max = 30),
      actionButton(inputId = "new_pop",
                   label= "Generate population data"),
      sliderInput(inputId = "sample_size",
                  label = "Sample size:",
                  value = 25, min = 1, max = 100),
      actionButton(inputId = "new_sample",
                   label= "Draw new sample")
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"),
               verbatimTextOutput("stats"),
               plotOutput("sample_hist"))
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop_size <- 200000

  pop_data <-
    reactive({
      input$new_pop
      rnorm(pop_size,100,input$pop_sd) # normal distribution
    })


  theme_set(theme_light())

  pop_scale = c(25,175)
  pop_breaks = seq(25,175,50)

  sample_data <-
    reactive({
      input$new_sample
      sample_IDs <- round(runif(input$sample_size,0,pop_size),0)
      pop_data()[sample_IDs]
    })

  output$distPlot <-
    renderPlot({
      ggplot() +
        geom_histogram(aes(pop_data()),
                       fill = "grey",
                       binwidth = 5) +
        geom_dotplot(aes(x = sample_data()),
                     fill = "yellow",
                     colour = "red",
                     alpha = .8,
                     binwidth = 1,
                     dotsize = 4) +
        geom_vline(xintercept = median(sample_data()),
                   colour = "red") +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid = element_blank()) +
        xlim(25,175)
    })

  output$stats <-
    renderPrint({
      summary(sample_data())
    })

  output$sample_hist <-
    renderPlot({
      ggplot() +
        geom_boxplot(aes(sample_data()),
                     fill = "yellow",
                     colour = "red") +
        #theme_void() +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid = element_blank()) +
        xlim(25,175)
    },
    height = 100)
}

# Run the application
shinyApp(ui = ui, server = server)

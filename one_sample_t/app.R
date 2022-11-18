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
  titlePanel("Significantly different from a mean of 100?"),
  sidebarLayout(
    sidebarPanel(
      p("Use the inputs to change the properties of the data.
        The distribution of data that would be expected under the null hypothesis is displayed in grey.
        The sampled data (the 'experiment') is shown as yellow circles.
        The t-test determines whether the sample mean is significantly different from the expected mean of 100"),
      numericInput(inputId = "true_mean",
                   label = "True mean of effect",
                   value = 110,
                   min = 50,
                   max = 150),
      numericInput(inputId = "sd",
                   label = "Standard deviation of data",
                   value = 15,
                   min = 0,
                   max = 30),
      sliderInput(inputId = "sample_size",
                  label = "Sample size:",
                  value = 25, min = 1, max = 100),
      actionButton(inputId = "new_sample",
                   label= "Draw new sample")
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"),
              #plotOutput("sample_hist"),
              verbatimTextOutput("stats"),
              verbatimTextOutput("ttest")
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop_size <- 200000

  pop_data <-
    reactive({
      rnorm(pop_size,100,input$sd) # normal distribution
    })

  theme_set(theme_light())

  pop_scale = c(0,200)
  pop_breaks = seq(0,200,50)



  sample_data <-
    reactive({
      input$new_sample
      rnorm(input$sample_size,input$true_mean,input$sd)
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
    height = 75)

  output$stats <-
    renderPrint({
      summary(sample_data())
    })

  output$ttest <-
    renderPrint({
      t.test(sample_data(),mu = 100)

    })

}

# Run the application
shinyApp(ui = ui, server = server)

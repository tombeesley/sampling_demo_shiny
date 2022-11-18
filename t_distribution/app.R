#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse) #

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("The t distribution"),
  sidebarLayout(
    sidebarPanel(
      p("Shiny app to show how t varies in repeated sampling"),
      numericInput(inputId = "true_mean",
                   label = "True mean of effect",
                   value = 10,
                   min = 0,
                   max = 20),
      numericInput(inputId = "sd",
                   label = "Standard deviation of data",
                   value = 15,
                   min = 0,
                   max = 30),
      sliderInput(inputId = "sample_size",
                  label = "Sample size:",
                  value = 25, min = 2, max = 100),
      sliderInput(inputId = "n_samples",
                  label = "Number of samples drawn",
                  value = 1000, min = 1000, max = 10000, step = 1000),
      checkboxInput(inputId = "show_samples",
                    label = "Show sample results",
                    value = FALSE),
      actionButton(inputId = "run_sim",
                   label= "Run the simulation")
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot")
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop_size <- 200000

  pop_data <-
    reactive({
      rnorm(pop_size,0,input$sd) # normal distribution
    })

  theme_set(theme_light())

  pop_scale = c(0,200)
  pop_breaks = seq(0,200,50)

  sim_results <-
    reactive({
      input$run_sim
      t_data <- NULL
      for (s in 1:input$n_samples){
        d <- rnorm(input$sample_size,input$true_mean,input$sd)
        t_data <- rbind(t_data, as.numeric(t.test(d, mu = 0)[['statistic']]))
      }
      t_data
    })

  t_dist <-
    reactive({
      t_dist <- qt(seq(0.001,.999,.001), df = input$sample_size-1)
    })

  crit_t <-
    reactive({
      qt(.975, df = input$sample_size-1)
    })


  output$distPlot <-
    renderPlot({
      gp <-
        ggplot() +
        geom_density(aes(x = t_dist()),
                     fill = "blue",
                     alpha = .4) +
        theme_classic() +
        scale_x_continuous(limits = c(-4,10), breaks = seq(-4,10,1)) +
        labs(x = "t value") +
        geom_vline(aes(xintercept = crit_t()), colour = "orange", size = 2, linetype = "dashed") +
        theme_classic()
      if (input$show_samples == TRUE) {
        gp <-
          gp +
          geom_density(aes(x = sim_results()),
                       fill = "red",
                       alpha = .4)
      }
      gp
    })

}

# Run the application
shinyApp(ui = ui, server = server)

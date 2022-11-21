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

# read data

load("survey_raw.RData")

colnames(data)

data_proc <-
  data %>%
  select(maths_skills, height, climate_estimate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Building ggplot figures"),
  sidebarLayout(
    sidebarPanel(
      p("Shiny app to show example code for ggplot figures"),
      varSelectInput(inputId = "plot_var",
                     label = "Select variable:",
                     data = mpg),
      selectInput(inputId = "hist_fill",
                  label = "Select colour:",
                  choices = c("green", "red", "blue"))
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("examplePlot"),
              textOutput("exampleCode"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  theme_set(theme_light())

  output$examplePlot <-
    renderPlot({
      mpg %>%
        ggplot() +
        geom_histogram(aes(!!input$plot_var),
                       fill = input$hist_fill)
      #line1() + line2()
    })

  #aes_text <- reactive({aes_string(!!input$plot_var)})
  # line2 <-  reactive({geom_histogram(aes(input$plot_vars))})

  output$exampleCode <-
    renderText({
      paste0('ggplot(data = mpg) + geom_histogram(aes(x = ',
             input$plot_var,
             '), fill ="',
             input$hist_fill,
             '")')
    })

}

# Run the application
shinyApp(ui = ui, server = server)

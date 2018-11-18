#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

eb <- ebMaxDepth

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bearded seal (Erignathus barbatus) Dive Data",
              windowTitle = "EB Dive app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("dateInput",
                     "Date range:",
                     min = as.POSIXct("2015-09-10 23:22:00"),
                     max = as.POSIXct("2015-09-30 03:58:00"),
                     value = c(as.POSIXct("2015-09-10 22:00:00"),
                               as.POSIXct("2015-09-30 5:00:00")))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("dive_line")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  eb_filtered <- reactive({
    ebMaxDepth %>%
      filter(time < input$dateInput[2],
             time > input$dateInput[1])
  })
  output$dive_line <- renderPlot({
    eb_filtered() %>%
      ggplot(aes(x = time, y = MAX_DEP, colour = as.factor(id)) +
      geom_path()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


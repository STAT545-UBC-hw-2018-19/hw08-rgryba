#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

ebMaxDepth <- read.csv("ebMaxDepth.csv", stringsAsFactors = F)
ebMaxDepth$time <- as.POSIXct(ebMaxDepth$time)
ebMaxDepth$MAX_DEP <- as.numeric(ebMaxDepth$MAX_DEP)


ui <- fluidPage(
   
   # Application title
   titlePanel("Bearded seal (Erignathus barbatus) Dive Data - Maximum Dive Depth",
              windowTitle = "EB Dive app"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("dateInput",
                     "Date range:",
                     min = as.POSIXct("2015-09-10 23:22:00"),
                     max = as.POSIXct("2015-09-30 03:58:00"),
                     value = c(as.POSIXct("2015-09-10 22:00:00"),
                               as.POSIXct("2015-09-30 5:00:00"))),
         checkboxGroupInput("checkGroup", "Select which seal you would like to look at or select both", 
                      choices = list("Seal 35" = 35,
                                     "Seal 38" = 38),
                      selected = c(35, 38))
      ),
      
      mainPanel(
         plotOutput("dive_line"),
         textOutput("max_depth")
      )
   )
)


server <- function(input, output) {
  eb_filtered <- reactive({
    if (is.null(input$checkGroup)) {
      return(NULL)
    }  
    ebMaxDepth %>%
      filter(time < input$dateInput[2],
             time > input$dateInput[1],
             id == input$checkGroup)
  })
  output$dive_line <- renderPlot({
    if (is.null(eb_filtered())) {
      return()
    }
    eb_filtered() %>%
      ggplot(aes(x = time, y = MAX_DEP, colour = as.factor(id))) +
      geom_path() + 
      scale_y_reverse() +
      xlab("Date") +
      ylab("Maximum dive depth (m)") +
      labs(colour='Seal ID') +
      theme_bw() +
      theme(legend.title=element_text(size=12), 
            legend.text=element_text(size=12),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14, face="bold"))
  })
  output$max_depth <- renderText ({
    paste("The maximum dive depth was " , 
            round(max(eb_filtered() %>% select(MAX_DEP)), 2), "m during this time period.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


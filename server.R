#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic to display user inputs
shinyServer(function(input, output) {
 
  #Click Button to create Graph
  output$hist <- renderPlot ({
    x    <- faithful[, 2]
    samples <- seq(min(x), max(x), length.out = input$sampleSize + 1)
     
    hist(x, breaks = samples, col = "#72A0C1", border = 'white')
  })
  
  #Gets Data from Selected Dataset and shows it on a Table
  output$dateViewer <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
  
  #Shows Review to User after Typing
  output$reviewOutput <- renderText(
    paste(input$reviewInput) 
  )
  
  
})

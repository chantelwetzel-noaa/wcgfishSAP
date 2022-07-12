#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#Read in Static commerical revenue file, maybe not best practice
commerRevData <- read.csv("assessment_prioritization-master/tables/commercial_revenue.csv", header = TRUE)

# Define server logic to display user inputs
shinyServer(function(input, output) {
  
  #Displays Table Title for Commercial Revenue
  output$commercialRevTitle <- renderText({
    "Commericial Revenue"
  })
  
  #Gets Data from Commercial Revenue csv file and shows it on a Table
  output$dataViewer <- renderTable({
   (commerRevData)
  })
  
  #Filters Commercial Revenue Data based on Species
  
  
})

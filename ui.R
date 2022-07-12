#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#Commercial Revenue Data
commerRevData <- read.csv("assessment_prioritization-master/tables/commercial_revenue.csv", header = TRUE)


# Define UI for application that draws a histogram
shinyUI (
  navbarPage(
    "Stock Assesment Priotization",
     tabPanel("Commercial Revenue Data",
      sidebarPanel (
        textOutput("commercialRevTitle"),
        selectInput("commercialSpecies",
                    "Species:",
                    c("All",
                      unique(as.character(commerRevData$Species))))
      ),
      mainPanel (
        tableOutput("dataViewer")
      )
     ),
    tabPanel("Tribal Revenue Data",
      sidebarPanel(
      
      
      ),
      mainPanel(
        #tableOutput("dateViewer")
      )
    ),
    tabPanel("Review",
      sidebarPanel(
        
        
      ),
      mainPanel(
  
      )
     )
  )
)
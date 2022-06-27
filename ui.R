#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI (
  navbarPage(
    "UI Practice",
     tabPanel("Histogram Creator",
       sidebarPanel(
         sliderInput("sampleSize", "Select Sample Size",
                      value = 10,
                      min = 0,
                      max = 100,
                      step = 10
         ),
       ),
       mainPanel(
         plotOutput("hist")
       )
     ),
    tabPanel("View DataSets",
      sidebarPanel(
        selectInput("dataset", 
                    label = "Dataset", 
                    choices = ls("package:datasets")
                    ) ,
        sliderInput(inputId = "rowSize",
                     label = "Select Row Size",
                     value = 10,
                     min = 1,
                     max = 30,
                     step = 1
                    )
      
      ),
      mainPanel(
        tableOutput("dateViewer")
      )
    ),
    tabPanel("Review",
      sidebarPanel(
        textAreaInput("reviewInput", 
                  label = h3("Leave a Review Below"),
                  value = "",
                  placeholder = "Enter text..."),
        actionButton("reviewButton", "Press Here", class = "btn-success")
        
      ),
      mainPanel(
        textOutput("reviewOutput")
      )
     )
  )
)
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
shinyUI (fluidPage (
  
  # Title for Page
  titlePanel("Practice Shiny UI"),
    
  #Will Look to have a Sidebar Section so I'll have Sidebar-Layout
  sidebarLayout (
    # Sidebar Section only w/Inputs
    sidebarPanel (
      h2("My Sidebar"),
      sliderInput (
        "ratingScale", 
        "Rating Scale",
         value = 1, 
         min = 1, 
         max = 5
      )
    ),
    mainPanel (
     #Tabs w/ Hopefully Responsive Data from Inputs
     tabsetPanel(
       tabPanel("Ratings Data", plotOutput("distPlot")) #Still Figuirng Out
     )
    )
  )
  
))
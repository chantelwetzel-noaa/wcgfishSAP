library(shiny)
library(DT)
library(tidyverse)
#Read in Static commerical revenue file, maybe not best practice
commerRevData <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribalData <- read.csv("tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
recData <- read.csv("tables/recreational_importance.csv", header = TRUE)

# Define server logic to display user inputs
shinyServer(function(input, output) {
  
  #Species Info Window
  #output$infoWindow <- rend
  
  #Displays Table Title for Commercial Revenue
  output$commercialRevTitle <- renderText({
    "Commericial Revenue"
  })
  
  #Gets Data from Commercial Revenue csv file and shows it on a Table
  #Table can Filter based on Species name or Ascend order by Rank
  output$commerdataViewer <- DT::renderDataTable({
  #    if (input$commSpeciesSelector != "All") {
        #Filter Data Based off selected list, note if I select more than one option I am looking for a row with more than one species name which doesnt exist!
        commerRevData <- commerRevData %>% filter(input$commSpeciesSelector == Species)
        
        #Format Data: Rounding Decimal Places and specifiying top "x" List
        datatable(commerRevData, options(
                                lengthMenu = c(5, 10, 20,nrow(commerRevData)))
                  )%>%formatRound(2:ncol(commerRevData), 2)
  #    } 
  #  else {
  #      datatable(commerRevData, options(
  #        lengthMenu = c(5, 10, 20,nrow(commerRevData)))
  #      )%>%formatRound(2:ncol(commerRevData), 2)
  #    }
  })
  
  
  #Just to Debug and Have stuff logged out 
  #output$commerdataViewer <- renderPrint ({
      #commerRevData %>% filter(Species == input$commSpeciesSelector )
  #})  

  
  #Can display and filter Tribal Fish Data by name and rank value
  output$tribaldataViewer <- renderTable({
    if (input$tribalSpecies != "All") {
      tribalData <- tribalData[tribalData$Species == input$tribalSpecies,]
    } else if (input$tribalrankSlider != 65) {
      tribalData <- tribalData[tribalData$Rank <= input$tribalrankSlider,]
    } else {
      tribalData
    }
  })
  
  #Can display and filter Recreational Fish Data by name and rank value
  output$recdataViewer <- renderTable({
    if (input$recSpecies != "All") {
      recData <-recData[recData$Species == input$recSpecies,]
    } else if (input$recreationrankSlider != 65) {
      recData <- recData[recData$Rank <= input$recreationrankSlider,]
    } else {
     recData
    }
  })

  
})

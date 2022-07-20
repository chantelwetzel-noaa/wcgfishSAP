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
  
  #Displays Table Title for Commercial Revenue
  output$commercialRevTitle <- renderText({
    "Commericial Revenue"
  })
  
  #Gets Data from Commercial Revenue csv file and shows it on a Table
  #Table can Filter based on Species name or Ascend order by Rank
  output$commerdataViewer <- DT::renderDataTable({
    # Renamed CSV Columns
    commerRevColumns <- c("Species", 
                           "Rank", 
                           "Factor Score", 
                           "Interum Value",  
                           "Revenue",
                           "California Revenue",
                           "Oregon Revenue",
                           "Washington Revenue")
    
   # Change data
    commerRevData <- commerRevData %>% filter(input$commSpeciesSelector == Species)
    
    # Format Data: Rounding Decimal Places and specifiying top "x" List
    datatable(commerRevData, options(lengthMenu = c(5, 10,20, nrow(commerRevData)),
                                     order = list(list(3, 'desc'))),
              colnames = commerRevColumns
              )%>% formatRound(3:ncol(commerRevData), 2)%>% 
                   formatCurrency(5:ncol(commerRevData),currency = "$")%>%
                   formatStyle(
                     columns = "Species",
                     backgroundColor = "#5F9EA0",
                     color = "white",
                     fontWeight = "bold"
                   )
  })
  
  # Commercial Species Info Window
  output$commInfoWindow <- renderPrint({
    input$comspecInfo
  })

  
  #Can display and filter Tribal Fish Data by name and rank value
  output$tribaldataViewer <- DT::renderDataTable({
    # Renamed CSV Columns
    tribalColumns <- c("Species", 
                          "Rank", 
                          "Factor Score", 
                          "Subsistence Score",  
                          "Initial Factor Score",
                          "Interum Value",
                          "Revenue"
                          )
    
    # Change data
    tribalData <- tribalData %>% filter(input$tribalSpeciesSelector == Species)
    
    # Format Data: Rounding Decimal Places and specifiying top "x" List
    datatable(tribalData, options(lengthMenu = c(5, 10,20, nrow(tribalData)),
                                     order = list(list(3, 'desc'))),
              colnames = tribalColumns
    )%>% formatRound(5:ncol(tribalData), 2)%>%
      formatRound(3, 2)%>%
      formatCurrency(ncol(tribalData),currency = "$")%>%
      formatStyle(
        columns = "Species",
        backgroundColor = "#6a5acd",
        color = "white",
        fontWeight = "bold"
      )
  })
  
  # Tribal Species Info Window
  output$tribalInfoWindow <- renderPrint({
    input$tribalspecInfo
  })
  
  #Can display and filter Recreational Fish Data by name and rank value
  output$recdataViewer <- renderTable({
   
  })
  
})

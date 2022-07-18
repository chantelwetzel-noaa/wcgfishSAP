library(shiny)
library(DT)
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
      datatable(commerRevData, options(
                lengthMenu = c(5, 10, 20,nrow(commerRevData))
                ))%>%formatRound(2:ncol(commerRevData), 2) # Round Data to 2 dec
 
  })
  
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

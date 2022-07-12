library(shiny)
#Read in Static commerical revenue file, maybe not best practice
commerRevData <- read.csv("assessment_prioritization-master/tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribalData <- read.csv("assessment_prioritization-master/tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
recData <- read.csv("assessment_prioritization-master/tables/recreational_importance.csv", header = TRUE)

# Define server logic to display user inputs
shinyServer(function(input, output) {
  
  #Displays Table Title for Commercial Revenue
  output$commercialRevTitle <- renderText({
    "Commericial Revenue"
  })
  
  #Gets Data from Commercial Revenue csv file and shows it on a Table
  #Table can Filter based on Species name or Ascend order by Rank
  output$commerdataViewer <- renderTable({
    if (input$commercialSpecies != "All") {
      commerRevData <- commerRevData[commerRevData$Species == input$commercialSpecies,]
    } else if (input$commrankSlider != 65) {
      commerRevData <- commerRevData[commerRevData$Rank <= input$commrankSlider,]
    } else {
      commerRevData
    }
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

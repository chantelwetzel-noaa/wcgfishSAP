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
    commerRevData <- commerRevData[commerRevData$Species %in% input$commSpeciesSelector,]
    
    
    # Format Data: Rounding Decimal Places and specifying top "x" List
    datatable(commerRevData, options(lengthMenu = 
                                       list( c(-1, 5, 10, 20), #ALL is a Keyword
                                             c("All", 5, 10, 20)
                                            ),
              order = list(list(1, 'asc'))),
              colnames = commerRevColumns,
              rownames = FALSE
    )%>% formatRound(3:ncol(commerRevData), 2)%>% 
      formatCurrency(5:ncol(commerRevData),
                     currency = "$",
                     digits =0
      )%>%
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
    tribalData <- tribalData[tribalData$Species %in% input$tribalSpeciesSelector,]
    
    
    # Format Data: Rounding Decimal Places and specifying top "x" List
    datatable(tribalData, options(lengthMenu = list(
                                                    c(-1, 5, 10, 20), #ALL is a Keyword
                                                    c("All", 5, 10, 20)
                                                    ),
             order = list(list(1, 'asc'))),
             colnames = tribalColumns,
             rownames = FALSE
    )%>% formatRound(5:ncol(tribalData), 2)%>%
      formatRound(3, 2)%>%
      formatCurrency(ncol(tribalData),
                     currency = "$",
                     digits = 0)%>%
      formatStyle(
        columns = "Species",
        backgroundColor = "#6a5acd",
        color = "white",
        fontWeight = "bold"
      )
  })
  
  # Tribal Species Info Window
  output$tribalInfoWindow <- renderPrint({
    tribalData[input$tribalspecInfo]
  })
  
  #Can display and filter Recreational Fish Data by name and rank value
  output$recdataViewer <- DT::renderDataTable({
    # Renamed CSV Columns
    recColumns <- c("Species", 
                    "Rank", 
                    "Factor Score", 
                    "Initial Factor Score",
                    "Pseudo Value Coast Wide",
                    "Pseudo Value California",
                    "Pseudo Value Oregon",
                    "Pseudo Value Washington",
                    "Relative Weight California",
                    "Relative Weight Oregon",
                    "Relative Weight Washington",
                    "Retained Catch Coast Wide",
                    "Retained Catch California",
                    "Retained Catch Oregon",
                    "Retained Catch Washington"
    )
    
    # Change data
    recData <- recData[recData$Species %in% input$recSpeciesSelector,]
    
    
    # Format Data: Rounding Decimal Places and specifying top "x" List
    datatable(recData, options(lengthMenu =  list( c(-1, 5, 10, 20), #ALL is a Keyword
                                                   c("All", 5, 10, 20)
                                                  ),
                               order = list(list(1, 'asc'))),
              colnames = recColumns,
              rownames = FALSE
    )%>% formatRound(5:ncol(recData), 0)%>%
      formatRound(3:4, 2)%>%
      formatRound(2, 0)%>%
      formatCurrency(ncol(recData),
                     currency = "$",
                     digits = 0)%>%
      formatStyle(
        columns = "Species",
        backgroundColor = "#fd5c63",
        color = "white",
        fontWeight = "bold"
      )%>%
      formatStyle('Rank', 
                  background = styleInterval(cuts = c(5,10),  values = c("blue", 'red', "green"))
                  )
  })
  
  output$debugScreen <- renderPrint({
    #commerRevData[commerRevData$Species %in% input$commSpeciesSelector]
    commerRevData %>% filter(input$commSpeciesSelector == Species)
  })
  
})
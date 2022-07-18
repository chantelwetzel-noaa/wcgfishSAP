library(shiny)

# Loaded Commercial Revenue Data - Will look to change
commerRevData <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribalData <- read.csv("tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
recData <- read.csv("tables/recreational_importance.csv", header = TRUE)

# Define UI for application that draws a histogram
shinyUI (
  navbarPage(
    "Stock Assesment Priotization",
     tabPanel("Commercial Revenue Data",
      sidebarPanel (
        selectInput("commercialSpecies",
                    "Commericial Species:",
                    c("All",
                      unique(as.character(commerRevData$Species))
                      )
                   ),
        textOutput("infoWindow")
      ),
      mainPanel (
        DT::dataTableOutput("commerdataViewer")
      )
     ),
    tabPanel("Tribal Revenue Data",
      sidebarPanel(
        textOutput("tribalrevTitle"),
        selectInput("tribalSpecies",
                    "Tribal Species:",
                    c("All",
                      unique(as.character(tribalData$Species))
                    )
        ),
        sliderInput("tribalrankSlider", 
                    "Tribal Species Rank Slider", 
                    min = 1,
                    max = 65,
                    value = 65
        )
      
      ),
      mainPanel(
       tableOutput("tribaldataViewer")
      )
    ),
    tabPanel("Recrerational Data",
      sidebarPanel(
        selectInput("recSpecies",
                    "Recreational Species:",
                    c("All",
                      unique(as.character(recData$Species))
                    )
        ),
        sliderInput("recreationrankSlider", 
                    "Recreational Species Rank Slider", 
                    min = 1,
                    max = 65,
                    value = 65
        )
        
      ),
      mainPanel(
        tableOutput("recdataViewer")
      )
     )
  )
)
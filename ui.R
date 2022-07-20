library(shiny)
library(shinyWidgets)

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
     tabPanel("Commercial Importance",
      sidebarPanel (
          pickerInput (
            inputId = "commSpeciesSelector",
            label = "Commercial Species",
            choices = c(unique(as.character(commerRevData$Species))),
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            selected = unique(as.character(commerRevData$Species))
          ),
          selectInput(inputId = "comspecInfo",
                      label = "Species Info Window",
                      choices = c(unique(as.character(commerRevData$Species)))
                      ),
          verbatimTextOutput (
            outputId = "commInfoWindow"
          )
      ),
      mainPanel (
        DT::dataTableOutput("commerdataViewer")
      )
     ),
    tabPanel("Tribal Importance",
      sidebarPanel(
        pickerInput (
          inputId = "tribalSpeciesSelector",
          label = "Tribal Species",
          choices = c(unique(as.character(tribalData$Species))),
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          selected = unique(as.character(tribalData$Species))
        ),
        selectInput(inputId = "tribalspecInfo",
                    label = "Species Info Window",
                    choices = c(unique(as.character(tribalData$Species)))
        ),
        verbatimTextOutput (
          outputId = "tribalInfoWindow"
        )
      ),
      mainPanel(
        DT::dataTableOutput("tribaldataViewer")
      )
    ),
    tabPanel("Recrerational Importance",
      sidebarPanel(
        
        
      ),
      mainPanel(
      )
     )
  )
)
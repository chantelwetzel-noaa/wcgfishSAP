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
               h3("Commercial Importance Information Window"),
               tags$p(HTML("The following window provides a bit of information 
                           on the Commercial Importance tab")),
               tags$p(HTML("<b>Species: </b> Denotes the Name of a Commericial
                            Fish Species
                           ")),
               tags$p(HTML("<b>Rank: </b> Denotes the Rank of a Commericial Fish
                            Species based on a number of factors
                           ")),
               tags$p(HTML("<b>Interum Value: </b> Denotes the Rank of a Commericial Fish
                            Species based on a number of factors
                           ")),
               tags$p(HTML("<b>Factor Score: </b> Denotes the Rank of a Commericial Fish
                            Species based on a number of factors
                           ")),
               tags$p(HTML("<b>Revenue: </b> Total Revenue associated with 
                             a Species along the Pacific Coast States (CA, OR, and WA)
                           ")),
               tags$p(HTML("<b>California Revenue: </b> Total Revenue associated with 
                             a Species within California
                           ")),
               tags$p(HTML("<b>Oregon Revenue: </b> Total Revenue associated with 
                             a Species within Oregon
                           ")),
               tags$p(HTML("<b>Washington Revenue: </b> Total Revenue associated with 
                             a Species within Washington")),
               h3("Commericial Species Selector"),
               pickerInput (
                 inputId = "commSpeciesSelector",
                 label = "Select Commercial Species",
                 choices = c(unique(as.character(commerRevData$Species))),
                 multiple = TRUE,
                 options = list(`actions-box` = TRUE),
                 selected = unique(as.character(commerRevData$Species))
               )
             ),
             mainPanel(
               DT::dataTableOutput("commerdataViewer")
             )),
    tabPanel("Tribal Importance",
             sidebarPanel(
               h3("Tribal Importance Information Window"),
               tags$p(HTML("The following window provides a bit of information 
               on the Tribal Importance tab")),
               tags$p(HTML("<b>Species: </b> Denotes the Name of a Tribal
               Fish Species
               ")),
               tags$p(HTML("<b>Rank: </b> Denotes the Rank of a Tribal Fish
               Species based on a number of factors
               ")),
               tags$p(HTML("<b>Factor Score: </b> Denotes the Factor Score 
               of a Tribal Fish Species
               ")),
               tags$p(HTML("<b>Subsistence Score: </b> Denotes the Subsistence
               Score of a Tribal Fish Species
               ")),
               tags$p(HTML("<b>Initial Factor Score: </b> An Factor Score derived
               from X
               ")),
               tags$p(HTML("<b>Interum Value: </b> A Value associated with Tribal
               Fish Species
               ")),
               tags$p(HTML("<b>Revenue: </b> Total Revenue associated with 
               a Species along the Pacific Coast States 
               (WA, OR, CA)
                ")),
               h3("Tribal Species Selector"),
               pickerInput (
                 inputId = "tribalSpeciesSelector",
                 label = "Select Tribal Species Below",
                 choices = c(unique(as.character(tribalData$Species))),
                 multiple = TRUE,
                 options = list(`actions-box` = TRUE),
                 selected = unique(as.character(tribalData$Species))
               )
             ),
            mainPanel(
                     DT::dataTableOutput("tribaldataViewer")
            )
          ),
    tabPanel("Recreational Importance",
         sidebarPanel(    
               h3("Recreational Importance Information Window"),
               tags$p(HTML("The following window provides a bit of information 
                           on the Recreational Importance tab")),
               tags$p(HTML("<b>Species: </b> Denotes the Name of a Recreational
                            Fish Species
                           ")),
               tags$p(HTML("<b>Rank: </b> Denotes the Rank of a Recreational Fish
                            Species based on a number of factors
                           ")),
               tags$p(HTML("<b>Factor Score: </b> Denotes the Factor Score 
                            of a Tribal Fish Species
                           ")),
               tags$p(HTML("<b>Initial Factor Score: </b> An Factor Score derived
                            from X
                           ")),
               tags$p(HTML("<b>Pseudo Value Coast Wide: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Pseudo Value California: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Pseudo Value Oregon: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Pseudo Value Washington: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Relative Weight California: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Relative Weight Oregon: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Relative Weight Washingtn: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Retained Catch Coast Wide: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Retained Catch California: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Retained Catch Oregon: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               tags$p(HTML("<b>Retained Catch Washington: </b> lorem impsum lorem impsum lorem impsum
                           ")),
               h3("Recreational Species Selector"),
               pickerInput (
                 inputId = "recSpeciesSelector",
                 label = "Select Recreational Species",
                 choices = c(unique(as.character(recData$Species))),
                 multiple = TRUE,
                 options = list(`actions-box` = TRUE),
                 selected = unique(as.character(recData$Species))
               )
        ), 
        mainPanel(
          DT::dataTableOutput("recdataViewer")
        ))
  )
)
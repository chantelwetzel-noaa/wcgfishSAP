library(shiny)
library(shinyWidgets)

# Loaded Commercial Revenue Data - Will look to change
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Stock Assessment Prioritization",
    navbarMenu("Factors",
               # subpanel for commercial importance
               tabPanel("Commercial Importance",
                        # sidebarPanel(
                          h3("Commercial Importance Information Window"),
                          tags$p(HTML("The following window provides a bit of information
                                      on the Commercial Importance tab")),
                          tags$p(HTML("<b>Species: </b> Denotes the name of a fish species")),
                          tags$p(HTML("<b>Rank: </b> Denotes the rank of a fish species based on
                                      a number of factors")),
                          tags$p(HTML("<b>Interum Value: </b> Denotes the rank of a fish species
                                      based on a number of factors")),
                          tags$p(HTML("<b>Factor Score: </b> Denotes the rank of a fish species
                                      based on a number of factors")),
                          tags$p(HTML("<b>Revenue: </b> Total revenue associated with a species
                                      along the Pacific Coast States (CA, OR, and WA)")),
                          tags$p(HTML("<b>California Revenue: </b> Total revenue associated with 
                                      a species within California")),
                          tags$p(HTML("<b>Oregon Revenue: </b> Total revenue associated with
                                      a species within Oregon")),
                          tags$p(HTML("<b>Washington Revenue: </b> Total revenue associated with
                                      a species within Washington")),
                          h3("Commercial Species Selector"),
                          pickerInput(
                            inputId = "com_species_selector",
                            label = "Select Commercial Species",
                            choices = c(unique(as.character(com_rev_data$Species))),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE),
                            selected = unique(as.character(com_rev_data$Species))
                          ),
                        # ),
                        # mainPanel(
                          gt_output("com_data_viewer")
                        # )
               ),
               # subpanel for tribal importance
               tabPanel("Tribal Importance",
                        # sidebarPanel(
                          h3("Tribal Importance Information Window"),
                          tags$p(HTML("The following window provides a bit of information 
                                      on the Tribal Importance tab")),
                          tags$p(HTML("<b>Species: </b> Denotes the name of a fish species")),
                          tags$p(HTML("<b>Rank: </b> Denotes the rank of a fish species 
                                      based on a number of factors")),
                          tags$p(HTML("<b>Factor Score: </b> Denotes the factor score of a fish species")),
                          tags$p(HTML("<b>Subsistence Score: </b> Denotes the subsistence score of a
                                      fish species")),
                          tags$p(HTML("<b>Initial Factor Score: </b> A factor score derived from X")),
                          tags$p(HTML("<b>Interum Value: </b> A value associated with fish
                                      species")),
                          tags$p(HTML("<b>Revenue: </b> Total revenue associated with a species
                                      along the Pacific Coast States (WA, OR, CA)")),
                          h3("Tribal Species Selector"),
                          pickerInput(
                            inputId = "tribal_species_selector",
                            label = "Select Tribal Species Below",
                            choices = c(unique(as.character(tribal_data$Species))),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE),
                            selected = unique(as.character(tribal_data$Species))
                          ),
                        # ),
                        # mainPanel(
                          gt_output("tribal_data_viewer")
                        # )
               ),
               
               # subpanel for recreational importance
               tabPanel("Recreational Importance",
                        # sidebarPanel(
                          h3("Recreational Importance Information Window"),
                          tags$p(HTML("The following window provides a bit of information
                                      on the Recreational Importance tab")),
                          tags$p(HTML("<b>Species: </b> Denotes the Name of a fish species")),
                          tags$p(HTML("<b>Rank: </b> Denotes the rank of a fish species based
                                      on a number of factors")),
                          tags$p(HTML("<b>Factor Score: </b> Denotes the factor Score of a 
                                      fish species")),
                          tags$p(HTML("<b>Initial Factor Score: </b> A factor score derived
                                      from X")),
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
                          pickerInput(
                            inputId = "rec_species_selector",
                            label = "Select Recreational Species",
                            choices = c(unique(as.character(rec_data$Species))),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE),
                            selected = unique(as.character(rec_data$Species))
                          ),
                        # ), 
                        # mainPanel(
                          gt_output("rec_data_viewer")
                        # )
               ),
               tabPanel("Test",
                        fileInput("upload", NULL, accept = ".csv"),
                        gt_output("table")
               )
    )
  )
)

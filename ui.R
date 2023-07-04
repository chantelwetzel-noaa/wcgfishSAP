library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(gt)
library(nmfspalette)

# Loaded Commercial Revenue Data - Will look to change
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

# read in species management groups
species_groups <- read.csv("tables/species_management_groups.csv", header = TRUE)

# Define UI for application that produces tables + description of variables
shinyUI(
 fluidPage(
   # load page layout
    dashboardPage(
      
      skin = "blue",
      
      dashboardHeader(title = "Stock Assessment Prioritization Tool",
                      titleWidth = 300),
      
      dashboardSidebar(width = 300,
                       sidebarMenu(
                         menuItem("Home", tabName = "home", icon = icon("home")),
                         menuItem("Commercial Importance", tabName = "com_table",
                                  icon = icon("table")),
                         menuItem("Tribal Importance", tabName = "tribal_table",
                                  icon = icon("table")),
                         menuItem("Recreational Importance", tabName = "rec_table",
                                  icon = icon("table")),
                         menuItem("Test Table", tabName = "test", icon = icon("table"))
                       )
      ),
      
      dashboardBody(
        
        tabItems(
          
          tabItem(tabName = "home",
                  h1("Welcome!")),
          
          tabItem(tabName = "com_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "com_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  gt_output("com_data_viewer") %>% withSpinner(),
                  h3("Species Ranking Plot"),
                  plotOutput("com_species_ranking", click = "plot_click"),
                  tableOutput("com_point_data")
          ),
          
          tabItem(tabName = "tribal_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "tribal_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  gt_output("tribal_data_viewer") %>% withSpinner(),
                  h3("Species Ranking Plot"),
                  plotOutput("tribal_species_ranking", click = "plot_click"),
                  tableOutput("tribal_point_data")
          ),
          
          tabItem(tabName = "rec_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "rec_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  gt_output("rec_data_viewer") %>% withSpinner(),
                  h3("Species Ranking Plot"),
                  plotOutput("rec_species_ranking", click = "plot_click"),
                  tableOutput("rec_point_data")
          ),
          
          tabItem(tabName = "test",
                  sidebarLayout(
                    sidebarPanel(
                      fileInput("upload", "Upload file here:", accept = ".csv"),
                      tags$hr(),
                      checkboxInput("rename", "Rename columns?", value = TRUE)
                    ),
                    mainPanel(
                      gt_output("table") %>% withSpinner()
                    )
                  ),
                  h3("Species Ranking Plot"),
                  plotOutput("test_species_ranking", click = "plot_click"),
                  tableOutput("test_point_data")
          )
        )
      )  # end dashboardBody
    )  # end dashboard Page

    # navbarMenu("Factors",
    #            # subpanel for commercial importance
    #            tabPanel("Commercial Importance",
    #                     selectInput(
    #                       inputId = "com_species_selector",
    #                       label = "Select a species management group:",
    #                       choices = c(unique(as.character(species_groups$managementGroup))),
    #                       selected = c(unique(as.character(species_groups$managementGroup))),
    #                       multiple = TRUE
    #                     ),
    #                     gt_output("com_data_viewer")
    #            ),
    #            # subpanel for tribal importance
    #            tabPanel("Tribal Importance",
    #                     h1("Tribal Importance"),
    #                     selectInput(
    #                       inputId = "tribal_species_selector",
    #                       label = "Select a species management group:",
    #                       choices = c(unique(as.character(species_groups$managementGroup))),
    #                       selected = c(unique(as.character(species_groups$managementGroup))),
    #                       multiple = TRUE
    #                     ),
    #                     gt_output("tribal_data_viewer")
    #            ),
    #            
    #            # subpanel for recreational importance
    #            tabPanel("Recreational Importance",
    #                     h1("Recreational Importance"),
    #                     selectInput(
    #                       inputId = "rec_species_selector",
    #                       label = "Select a species management group",
    #                       choices = c(unique(as.character(species_groups$managementGroup))),
    #                       selected = c(unique(as.character(species_groups$managementGroup))),
    #                       multiple = TRUE
    #                     ),
    #                     gt_output("rec_data_viewer")
    #            ),
    #            tabPanel("Test",
    #                     sidebarLayout(
    #                       sidebarPanel(
    #                         fileInput("upload", "Upload file here:", accept = ".csv"),
    #                         tags$hr(),
    #                         checkboxInput("rename", "Rename columns?", value = TRUE)
    #                       ),
    #                       mainPanel(
    #                         gt_output("table")
    #                       )
    #                     )
    #            )
    # )
  )
)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(gt)
library(nmfspalette)
library(plotly)

# load in data (10 factors)
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)
const_dem_data <- read.csv("tables/const_demand.csv", header = TRUE)
rebuilding_data <- read.csv("tables/rebuilding.csv", header = TRUE)
stock_stat_data <- read.csv("tables/stock_status.csv", header = TRUE)
fish_mort_data <- read.csv("tables/fishing_mortality.csv", header = TRUE)
eco_data <- read.csv("tables/ecosystem.csv", header = TRUE)
new_info_data <- read.csv("tables/new_information.csv", header = TRUE)
ass_freq_data <- read.csv("tables/assessment_frequency.csv", header = TRUE) 

# load in species management groups
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
                         menuItem("Factors", tabName = "factors", icon = icon("table"),
                                  menuSubItem("Commercial Importance", tabName = "com_table",
                                              icon = icon("dollar-sign")),
                                  menuSubItem("Recreational Importance", tabName = "rec_table",
                                              icon = icon("campground")),
                                  menuSubItem("Tribal Importance", tabName = "tribal_table",
                                              icon = icon("feather")),
                                  menuSubItem("Constituent Demand", tabName = "cd_table",
                                              icon = icon("person")),
                                  menuSubItem("Rebuilding", tabName = "rebuilding_table",
                                              icon = icon("hammer")),
                                  menuSubItem("Stock Status", tabName = "ss_table",
                                              icon = icon("fish")),
                                  menuSubItem("Fishing Mortality", tabName = "fm_table",
                                              icon = icon("ship")),
                                  menuSubItem("Ecosystem", tabName = "eco_table",
                                              icon = icon("water")),
                                  menuSubItem("New Information", tabName = "ni_table",
                                              icon = icon("info")),
                                  menuSubItem("Assessment Frequency", tabName = "af_table",
                                              icon = icon("calendar-check"))
                         ),
                         menuItem("Upload your own file", tabName = "test", icon = icon("upload"))
                       )
      ),
      
      dashboardBody(
        
        tabItems(
          
          tabItem(tabName = "home",
                  h1("Welcome!")),
          
          tabItem(tabName = "com_table",
                  h3("Factor Table"),
                  selectInput("com_species_selector",
                              "Select a species management group:",
                              choices = c(unique(as.character(species_groups$managementGroup))),
                              selected = c(unique(as.character(species_groups$managementGroup))),
                              multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("com_data_viewer") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("com_species_ranking"), width = 12)
                  )
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
                  fluidRow(
                    box(gt_output("rec_data_viewer") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("rec_species_ranking"), width = 12)
                  )
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
                  fluidRow(
                    box(gt_output("tribal_data_viewer") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("tribal_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "cd_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "cd_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("cd_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("cd_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "rebuilding_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "reb_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("reb_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("reb_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "ss_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "ss_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("ss_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("ss_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "fm_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "fm_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("fm_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("fm_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "eco_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "eco_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("eco_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("eco_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "ni_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "ni_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("ni_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("ni_species_ranking"), width = 12)
                  )
          ),
          
          tabItem(tabName = "af_table",
                  h3("Factor Table"),
                  selectInput(
                    inputId = "af_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  fluidRow(
                    box(gt_output("af_gt_table") %>% withSpinner(), width = 12)
                  ),
                  h3("Species Ranking Plot"),
                  fluidRow(
                    box(plotlyOutput("af_species_ranking"), width = 12)
                  )
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
                  plotlyOutput("test_species_ranking")
          )
        )
      )  # end dashboardBody
    )  # end dashboard Page
  )
)
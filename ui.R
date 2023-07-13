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
                                  menuSubItem("Commercial Importance", tabName = "com_page",
                                              icon = icon("dollar-sign")),
                                  menuSubItem("Recreational Importance", tabName = "rec_page",
                                              icon = icon("campground")),
                                  # edit tribal icon!
                                  menuSubItem("Tribal Importance", tabName = "tribal_page"),
                                  menuSubItem("Constituent Demand", tabName = "cd_page",
                                              icon = icon("person")),
                                  menuSubItem("Rebuilding", tabName = "rebuilding_page",
                                              icon = icon("hammer")),
                                  menuSubItem("Stock Status", tabName = "ss_page",
                                              icon = icon("fish")),
                                  menuSubItem("Fishing Mortality", tabName = "fm_page",
                                              icon = icon("ship")),
                                  menuSubItem("Ecosystem", tabName = "eco_page",
                                              icon = icon("water")),
                                  menuSubItem("New Information", tabName = "ni_page",
                                              icon = icon("info")),
                                  menuSubItem("Assessment Frequency", tabName = "af_page",
                                              icon = icon("calendar-check"))
                         ),
                         menuItem("Upload your own file", tabName = "test", icon = icon("upload"))
                       )
      ),
      
      dashboardBody(
        
        tabItems(
          
          tabItem(tabName = "home",
                  h1("Welcome!")),
          
          tabItem(tabName = "com_page",
                  h1("Commercial Importance"),
                  selectInput("com_species_selector",
                              "Select a species management group:",
                              choices = c(unique(as.character(species_groups$managementGroup))),
                              selected = c(unique(as.character(species_groups$managementGroup))),
                              multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("com_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Commercial Importance Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("com_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "rec_page",
                  h1("Recreational Importance"),
                  selectInput(
                    inputId = "rec_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("rec_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Recreational Importance Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, 
                        plotlyOutput("rec_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "tribal_page",
                  h1("Tribal Importance"),
                  selectInput(
                    inputId = "tribal_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("tribal_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Tribal Importance Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("tribal_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "cd_page",
                  h1("Constituent Demand"),
                  selectInput(
                    inputId = "cd_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("cd_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Constituent Demand Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("cd_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "rebuilding_page",
                  h1("Rebuilding"),
                  selectInput(
                    inputId = "reb_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("reb_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Rebuilding Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("reb_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "ss_page",
                  h1("Stock Status"),
                  selectInput(
                    inputId = "ss_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("ss_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Stock Status Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("ss_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "fm_page",
                  h1("Fishing Mortality"),
                  selectInput(
                    inputId = "fm_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("fm_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Fishing Mortality Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("fm_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "eco_page",
                  h1("Ecosystem"),
                  selectInput(
                    inputId = "eco_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("eco_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Ecosystem Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("eco_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "ni_page",
                  h1("New Information"),
                  selectInput(
                    inputId = "ni_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("ni_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "New Information Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("ni_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          tabItem(tabName = "af_page",
                  h1("Assessment Frequency"),
                  selectInput(
                    inputId = "af_species_selector",
                    label = "Select a species management group:",
                    choices = c(unique(as.character(species_groups$managementGroup))),
                    selected = c(unique(as.character(species_groups$managementGroup))),
                    multiple = TRUE
                  ),
                  em("Place cursor in the box and press delete to narrow down your selection."),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, gt_output("af_gt_table") %>% withSpinner(),
                        width = 12)
                  ),
                  fluidRow(
                    box(title = "Assessment Frequency Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("af_species_ranking") %>% withSpinner(),
                        width = 12)
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
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(gt)
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

# join data
joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName"))
joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName"))
joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName"))
joined_cd_df <- left_join(const_dem_data, species_groups, by = c("Species" = "speciesName"))
joined_reb_df <- left_join(rebuilding_data, species_groups, by = c("Species" = "speciesName"))
joined_ss_df <- left_join(stock_stat_data, species_groups, by = c("Species" = "speciesName"))
joined_fm_df <- left_join(fish_mort_data, species_groups, by = c("Species" = "speciesName"))
joined_eco_df <- left_join(eco_data, species_groups, by = c("Species" = "speciesName"))
joined_ni_df <- left_join(new_info_data, species_groups, by = c("Species" = "speciesName"))
joined_af_df <- left_join(ass_freq_data, species_groups, by = c("Species" = "speciesName")) %>%
  select(Species, Rank, Score, Recruit_Var:managementGroup)

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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("com_columns", "Select columns:",
                                           choices = colnames(joined_com_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score", "Revenue")
                        ),
                        selectInput("com_species_selector",
                                    "Select a species management group:",
                                    choices = c(unique(as.character(species_groups$managementGroup))),
                                    selected = c(unique(as.character(species_groups$managementGroup))),
                                    multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("com_gt_table") %>% withSpinner(),
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Commercial_importance_definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("rec_columns", "Select columns:",
                                           choices = colnames(joined_rec_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score", "Pseudo_CW")
                        ),
                        selectInput(
                          inputId = "rec_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("rec_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here.")
                    )
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("tribal_columns", "Select columns:",
                                           choices = colnames(joined_tribal_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score", "Revenue")
                        ),
                        selectInput(
                          inputId = "tribal_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("tribal_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("cd_columns", "Select columns:",
                                           choices = colnames(joined_cd_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score", "Choke_Stock",
                                                        "Commercial_Importance",
                                                        "Recreational_Importance",
                                                        "Landings_Constricted",
                                                        "Concern")
                        ),
                        selectInput(
                          inputId = "cd_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("cd_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("reb_columns", "Select columns:",
                                           choices = colnames(joined_reb_df),
                                           selected = c("Species", "rebuilding",
                                                        "target_year")
                        ),
                        selectInput(
                          inputId = "reb_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("reb_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("ss_columns", "Select columns:",
                                           choices = colnames(joined_ss_df),
                                           selected = c("Species", "Rank",
                                                        "Estimate", "PSA")
                        ),
                        selectInput(
                          inputId = "ss_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("ss_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("fm_columns", "Select columns:",
                                           choices = colnames(joined_fm_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score",
                                                        "Fishing_Mortality", "OFL",
                                                        "OFL_Attain_Percent",
                                                        "managementGroup")
                        ),
                        selectInput(
                          inputId = "fm_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("fm_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("eco_columns", "Select columns:",
                                           choices = colnames(joined_eco_df),
                                           selected = c("Species", "Rank",
                                                        "Factor_Score",
                                                        "Ecosystem_Score")
                        ),
                        selectInput(
                          inputId = "eco_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("eco_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("ni_columns", "Select columns:",
                                           choices = colnames(joined_ni_df),
                                           selected = c("Species", "Rank",
                                                        "notes")
                        ),
                        selectInput(
                          inputId = "ni_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("ni_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        checkboxGroupInput("af_columns", "Select columns:",
                                           choices = colnames(joined_af_df),
                                           selected = c("Species", "Score",
                                                        "Last_Assess", "MeanAge")
                        ),
                        selectInput(
                          inputId = "af_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$managementGroup))),
                          selected = c(unique(as.character(species_groups$managementGroup))),
                          multiple = TRUE
                        ),
                        em("Place cursor in the box and press delete to narrow down your selection."),
                        br(),
                        br()
                    ),
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("af_gt_table") %>% withSpinner(),
                        p("See descriptions of each column here."))
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
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(gt)
library(plotly)
source("format_species_names.R")

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

# join data + rename columns
joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName"))
joined_com_df <- joined_com_df %>%
  rename_with(~gsub("_", " ", colnames(joined_com_df)))
  
joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName"))
joined_rec_df <- joined_rec_df %>%
  rename_with(~gsub("_", " ", colnames(joined_rec_df)))

joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName"))
joined_tribal_df <- joined_tribal_df %>%
  rename_with(~gsub("_", " ", colnames(joined_tribal_df)))

joined_cd_df <- left_join(const_dem_data, species_groups, by = c("Species" = "speciesName"))
joined_cd_df <- joined_cd_df %>%
  rename_with(~gsub("_", " ", colnames(joined_cd_df)))

joined_reb_df <- left_join(rebuilding_data, species_groups, by = c("Species" = "speciesName"))
joined_reb_df <- joined_reb_df %>%
  rename_with(~gsub("_", " ", colnames(joined_reb_df)))

joined_ss_df <- left_join(stock_stat_data, species_groups, by = c("Species" = "speciesName"))
joined_ss_df <- joined_ss_df %>%
  rename_with(~gsub("_", " ", colnames(joined_ss_df)))

joined_fm_df <- left_join(fish_mort_data, species_groups, by = c("Species" = "speciesName"))
joined_fm_df <- joined_fm_df %>%
  rename_with(~gsub("_", " ", colnames(joined_fm_df)))

joined_eco_df <- left_join(eco_data, species_groups, by = c("Species" = "speciesName"))
joined_eco_df <- joined_eco_df %>%
  rename_with(~gsub("_", " ", colnames(joined_eco_df)))

joined_ni_df <- left_join(new_info_data, species_groups, by = c("Species" = "speciesName"))
joined_ni_df <- joined_ni_df %>%
  rename_with(~gsub("_", " ", colnames(joined_ni_df)))

joined_af_df <- left_join(ass_freq_data, species_groups, by = c("Species" = "speciesName"))
joined_af_df <- joined_af_df %>%
  rename_with(~gsub("_", " ", colnames(joined_af_df))) %>%
  select(Species, Rank, Score, `Recruit Variation`:managementGroup)

# freezing species column when selecting
com_cols <- colnames(joined_com_df)[colnames(joined_com_df) != "Species"]
rec_cols <- colnames(joined_rec_df)[colnames(joined_rec_df) != "Species"]
tribal_cols <- colnames(joined_tribal_df)[colnames(joined_tribal_df) != "Species"]
cd_cols <- colnames(joined_cd_df)[colnames(joined_cd_df) != "Species"]
reb_cols <- colnames(joined_reb_df)[colnames(joined_reb_df) != "Species"]
ss_cols <- colnames(joined_ss_df)[colnames(joined_ss_df) != "Species"]
fm_cols <- colnames(joined_fm_df)[colnames(joined_fm_df) != "Species"]
eco_cols <- colnames(joined_eco_df)[colnames(joined_eco_df) != "Species"]
ni_cols <- colnames(joined_ni_df)[colnames(joined_ni_df) != "Species"]
ni_cols <- ni_cols[ni_cols != "Notes"]
af_cols <- colnames(joined_af_df)[colnames(joined_af_df) != "Species"]

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
                         menuItem("2024 Overall Ranking",
                                  tabName = "overall_ranking", icon = icon("ranking-star")),
                         menuItem("Factors", tabName = "factors", icon = icon("table"),
                                  menuSubItem("Fishing Mortality", tabName = "fm_page",
                                              icon = icon("ship")),
                                  menuSubItem("Commercial Importance", tabName = "com_page",
                                              icon = icon("dollar-sign")),
                                  menuSubItem("Tribal Importance", tabName = "tribal_page"),
                                  menuSubItem("Recreational Importance", tabName = "rec_page",
                                              icon = icon("campground")),
                                  menuSubItem("Constituent Demand", tabName = "cd_page",
                                              icon = icon("person")),
                                  menuSubItem("Stock Status", tabName = "ss_page",
                                              icon = icon("fish")),
                                  menuSubItem("Rebuilding", tabName = "rebuilding_page",
                                              icon = icon("hammer")),
                                  menuSubItem("Ecosystem", tabName = "eco_page",
                                              icon = icon("water")),
                                  menuSubItem("Assessment Frequency", tabName = "af_page",
                                              icon = icon("calendar-check")),
                                  menuSubItem("New Information", tabName = "ni_page",
                                              icon = icon("info"))
                         ),
                         # menuItem("Upload file", tabName = "test", icon = icon("upload")),
                         menuItem("Contact", tabName = "contact", icon = icon("envelope"))
                       )
      ),
      
      dashboardBody(
        
        tabItems(
          
          # home page (landing)
          tabItem(tabName = "home",
                  h1("Welcome!")),
          
          # overall ranking page
          tabItem(tabName = "overall_ranking",
                  h1("2024 Stock Assessment Prioritization Ranking"),
                  # fluidRow(
                  #   box(title = "Overall Ranking", status = "primary",
                  #       solidHeader = TRUE, collapsible = TRUE,
                  #       width = 12,
                  #       plotlyOutput("overall_plot") %>% withSpinner()
                  #   )
                  # ),
                  fluidRow(
                    box(title = "Weights", status = "warning",
                        solidHeader = TRUE, width = 12,
                        div(style = "display:inline-block", numericInput("comm_weight", 
                                                                         "Comm. Factor Weight:",
                                                                         value = 0.21, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("rec_weight", 
                                                                         "Rec. Factor Weight:",
                                                                         value = 0.09, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("tribal_weight",
                                                                         "Tribal Factor Weight:",
                                                                       value = 0.05, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("cd_weight",
                                                                         "Const. Dem. Factor Weight:",
                                                                         value = 0.11, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("reb_weight",
                                                                         "Rebuild Factor Weight:",
                                                                         value = 0.10, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("ss_weight",
                                                                         "Stock Status Factor Weight:",
                                                                         value = 0.08, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("fm_weight",
                                                                         "Fishing Mort. Factor Weight:",
                                                                         value = 0.08, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("eco_weight",
                                                                         "Eco. Factor Weight:",
                                                                         value = 0.05, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("ni_weight",
                                                                         "New Info Factor Weight:",
                                                                         value = 0.05, min = 0, max = 1)),
                        div(style = "display:inline-block", numericInput("af_weight",
                                                                         "Assess. Freq. Factor Weight:",
                                                                         value = 0.18, min = 0, max = 1)),
                        textOutput("weights_sum"),
                        htmlOutput("warning"),
                        br(),
                        div(style = "display:inline-block", actionButton("rescale", "Rescale weights")),
                        div(style = "display:inline-block", actionButton("reset", "Reset weights"))
                    )
                  ),
                  fluidRow(
                    box(title = "Factor Summary", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        width = 12,
                        uiOutput("overall_gt_table") %>% withSpinner()
                    )
                  )
          ),
          
          # fishing mortality page
          tabItem(tabName = "fm_page",
                  h1("Fishing Mortality"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("fm_columns", "Select columns to display:",
                                               choices = fm_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Average Removals",
                                                            "Average OFL",
                                                            "Average OFL Attainment",
                                                            "managementGroup")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("fm_colors", "Select columns to color:",
                                               choices = fm_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                        tags$a(href="javascript:window.open('Fishing Mortality Definitions.html',
                        '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Fishing Mortality Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("fm_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # commercial importance page
          tabItem(tabName = "com_page",
                  h1("Commercial Importance"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("com_columns", "Select columns to display:",
                                               choices = com_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Revenue")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("com_colors", "Select columns to color:",
                                               choices = com_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                          tags$a(href="javascript:window.open('Commercial Importance Definitions.html',
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
          
          # tribal importance page
          tabItem(tabName = "tribal_page",
                  h1("Tribal Importance"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("tribal_columns", "Select columns to display:",
                                               choices = tribal_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Revenue")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("tribal_colors", "Select columns to color:",
                                               choices = tribal_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Tribal Importance Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Tribal Importance Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("tribal_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # recreational importance page
          tabItem(tabName = "rec_page",
                  h1("Recreational Importance"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("rec_columns", "Select columns to display:",
                                               choices = rec_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Pseudo Revenue Coastwide")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("rec_colors", "Select columns to color:",
                                               choices = rec_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Recreational Importance Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Recreational Importance Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE, 
                        plotlyOutput("rec_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # constituent demand page
          tabItem(tabName = "cd_page",
                  h1("Constituent Demand"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("cd_columns", "Select columns to display:",
                                               choices = cd_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Choke Stock Adjustment",
                                                            "Commercial Importance",
                                                            "Recreational Importance",
                                                            "Landings Constricted",
                                                            "Concern")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("cd_colors", "Select columns to color:",
                                               choices = cd_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Constituent Demand Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Constituent Demand Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("cd_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # stock status page
          tabItem(tabName = "ss_page",
                  h1("Stock Status"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("ss_columns", "Select columns to display:",
                                               choices = ss_cols,
                                               selected = c("Rank", "Fraction Unfished",
                                                            "PSA")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("ss_colors", "Select columns to color:",
                                               choices = ss_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Stock Status Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Stock Status Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("ss_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # rebuilding page
          tabItem(tabName = "rebuilding_page",
                  h1("Rebuilding"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("reb_columns", "Select columns to display:",
                                               choices = reb_cols,
                                               selected = c("Currently Rebuilding",
                                                            "Rebuilding Target Year")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("reb_colors", "Select columns to color:",
                                               choices = reb_cols,
                                               selected = c("Currently Rebuilding")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Rebuilding Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Rebuilding Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("reb_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # ecosystem page
          tabItem(tabName = "eco_page",
                  h1("Ecosystem"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("eco_columns", "Select columns to display:",
                                               choices = eco_cols,
                                               selected = c("Rank", "Factor Score",
                                                            "Ecosystem Score")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("eco_colors", "Select columns to color:",
                                               choices = eco_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Ecosystem Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Ecosystem Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("eco_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # assessment frequency page
          tabItem(tabName = "af_page",
                  h1("Assessment Frequency"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("af_columns", "Select columns to display:",
                                               choices = af_cols,
                                               selected = c("Score",
                                                            "Last Assessment Year",
                                                            "Target Assessment Frequency")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("af_colors", "Select columns to display:",
                                               choices = af_cols,
                                               selected = c("Score")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('Assessment Frequency Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "Assessment Frequency Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("af_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # new information page
          tabItem(tabName = "ni_page",
                  h1("New Information"),
                  fluidRow(
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("ni_columns", "Select columns to display:",
                                               choices = ni_cols,
                                               selected = c("Rank", "Notes")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("ni_colors", "Select columns to color:",
                                               choices = ni_cols,
                                               selected = c("Rank")
                            ),
                            em("**Selecting a column that is not in the table will cause an error.",
                               style = "color:red")
                          )
                        ),
                        br(),
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
                        p("See descriptions of each column",
                          tags$a(href="javascript:window.open('New Information Definitions.html',
                          '_blank', 'width = 600, height = 400')", "here."))
                    )
                  ),
                  fluidRow(
                    box(title = "New Information Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("ni_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # upload file page
          # tabItem(tabName = "test",
          #         sidebarLayout(
          #           sidebarPanel(
          #             fileInput("upload", "Upload file here:", accept = ".csv"),
          #             tags$hr(),
          #             checkboxInput("rename", "Rename columns?", value = TRUE)
          #           ),
          #           mainPanel(
          #             gt_output("table") %>% withSpinner()
          #           )
          #         ),
          #         plotlyOutput("test_species_ranking")
          # )
          
          # contact us page
          tabItem(tabName = "contact",
                  h1("Contact Us"),
                  h4("We look forward to hearing your feedback and questions."),
                  h4("If you encounter any issues or have suggestions for
                     improvement, please visit the", tags$q("Issues"), "tab in our",
                     tags$a(href="https://github.com/chantelwetzel-noaa/wcgfishSAP/issues",
                            target = "_blank", "repository"), "to report them."),
                  br(),
                  fluidRow(
                    box(
                      width = 12,
                      h4(strong("Chantel Wetzel"), style = "color:#002B7B"),
                      h4("Mathematical Statistician"),
                      h4(strong("Office:"), "(206) 302-1753"),
                      h4(strong("Email:"), "chantel.wetzel@noaa.gov"),
                      h4("Northwest Fisheries Science Center"),
                    )
                  )
          )
        )
      )  # end dashboardBody
    )  # end dashboard Page
  )
)
# PACKAGES:
## shiny
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)

## data manipulation
library(dplyr)

## tables + visualizations
library(gt)
library(plotly)

# FUNCTIONS:
source("format_species_names.R")
source("format_table.R")


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
colnames(species_groups)[2] <- "Management Group"

# join data + rename columns
joined_com_df <- format_table(com_rev_data, species_groups)
  
joined_rec_df <- format_table(rec_data, species_groups)

joined_tribal_df <- format_table(tribal_data, species_groups)

joined_cd_df <- format_table(const_dem_data, species_groups)

joined_reb_df <- format_table(rebuilding_data, species_groups)

joined_ss_df <- format_table(stock_stat_data, species_groups)

joined_fm_df <- format_table(fish_mort_data, species_groups)

joined_eco_df <- format_table(eco_data, species_groups)

joined_ni_df <- format_table(new_info_data, species_groups)

joined_af_df <- format_table(ass_freq_data, species_groups)
joined_af_df <- joined_af_df %>%
  select(Species, Rank, Factor Score, `Recruit Variation`:`Management Group`)

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
af_cols <- colnames(joined_af_df)[colnames(joined_af_df) != "Species"]


# Define UI for application that produces tables + description of variables
shinyUI(
 fluidPage(
   
   # CSS to format features
   tags$head(
     # right-align HTML files + add margin (methodology page)
     tags$style(
       HTML("
       .align-right {
          text-align: right;
          margin-right: 15px;
       }")
     ),
     # align numericInputs on overall ranking page
     tags$style(
       HTML(
         ".numeric-input-container { 
            display: flex;
            align-items: flex-end;
            margin-right: 15px;
          }
          .numeric-input-container .shiny-input-container:not(:last-child) {
            margin-right: 10px;
          }"
       )
     ),
     # format scrolling text sections (factor pages)
     tags$style(
       HTML("
       .scrolling-text {
          height: 250px;
          overflow-y: scroll;
          border: 1px solid #ccc;
          padding: 10px;
       }")
     ),
     # format bulleted list font size  (resources page)
     tags$style(
       HTML("
       .custom-bulleted-list {
          font-size: 16px;
       }")
     )
   ),
   
   # format mathematical expressions
   withMathJax(),
   
   # load page layout
    dashboardPage(
      
      skin = "blue",
      
      dashboardHeader(title = "U.S. West Coast Groundfish Stock Assessment Prioritization",
                      titleWidth = 300),
      
      dashboardSidebar(width = 300,
                       sidebarMenu(
                         menuItem("Home", tabName = "home", icon = icon("home")),
                         menuItem("Methodology", tabName = "methodology",
                                  icon = icon("list-check")),
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
                         menuItem("Stock Assessment Calendar", tabName = "calendar",
                                  icon = icon("calendar")),
                         menuItem("Resources", tabName = "resources", icon = icon("book")),
                         menuItem("Contact", tabName = "contact", icon = icon("envelope")),
                         div(
                           style = "position: absolute; left: 0; right: 0; margin-top: 10px;
                           text-align: center;",
                           img(src = "logos/noaa-logo-rgb-blue-2022.png", height = "150px")
                         )
                       )
      ),
      
      dashboardBody(
        
        tabItems(
          
          # home page (landing)
          tabItem(tabName = "home",
                  h1("Stock Assessment Prioritization for U.S. West Coast Groundfish"),
                  h4("Groundfish stocks off the U.S. West Coast are managed by the Pacific
                  Fishery Management Council (PFMC).  The PFMC groundfish Fishery Management Plan
                  contains a large diversity of 100+ species and stocks. The number of species off
                  the West Coast far exceeds the capacity and resources to conduct scientific assessments 
                  each management cycle to determine population status to inform harvest limits.
                  Stock assessment prioritization is aimed to be an objective and transparent tool to
                  consider a wide range of fishery, economic, and population indicators in order to
                  identify species with the greatest need for new assessments. Stock assessment
                  prioritization allows us to work with regional partners to decide which stocks
                  are assessed each year."),
                  tags$footer(
                    style = "text-align: center;",
                    HTML(paste0("<img src='logos/noaa-logo-rgb-blue-2022.png' alt='Image 1',
                                height='80' style='margin-right: 15px;'>")),
                    HTML(paste0("<img src='logos/pfmc_black.png' alt='Image 2',
                                height='80'>"))
                  )
          ),
          
          # methodology page
          tabItem(tabName = "methodology",
                  h1("Methodology"),
                  fluidRow(
                    # clickable table of contents
                    box(
                      title = "Table of Contents", status = "primary",
                      solidHeader = TRUE, width = 3,
                      tags$ol(
                        tags$li(tags$a(href = "#introduction", "Introduction")),
                        tags$li(tags$a(href = "#factors", "Description of Factors")),
                        tags$li(tags$a(href = "#commercial_importance",
                                       "Commercial Importance")),
                        tags$li(tags$a(href = "#tribal_importance",
                                       "Tribal Importance")),
                        tags$li(tags$a(href = "#recreational_importance",
                                       "Recreational Importance")),
                        tags$li(tags$a(href = "#constituent_demand",
                                       "Constituent Demand")),
                        tags$li(tags$a(href = "#stock_status",
                                       "Stock Status Relative to Management Targets")),
                        tags$li(tags$a(href = "#rebuilding", "Rebuilding Status")),
                        tags$li(tags$a(href = "#fishing_mortality",
                                       "Fishing Mortality, Relative to Overfishing Limits")),
                        tags$li(tags$a(href = "#ecosystem", "Ecosystem Importance")),
                        tags$li(tags$a(href = "#new_information",
                                       "Relevant New Types of Information Available")),
                        tags$li(tags$a(href = "#assessment_frequency",
                                       "Assessment Frequency")),
                        tags$li(tags$a(href = "#future_spex",
                                       "Future Limiting Harvest Specifications"))
                      )
                    ),
                    # HTML outputs of knitted Rmd files
                    htmlOutput("intro", align = "right",
                               class = "align-right"),
                    htmlOutput("factors", align = "right",
                               class = "align-right"),
                    htmlOutput("comm_importance", align = "right",
                               class = "align-right"),
                    htmlOutput("tribal_importance", align = "right",
                               class = "align-right"),
                    htmlOutput("rec_importance", align = "right",
                               class = "align-right"),
                    htmlOutput("const_demand", align = "right",
                               class = "align-right"),
                    htmlOutput("abundance", align = "right",
                               class = "align-right"),
                    htmlOutput("rebuild", align = "right",
                               class = "align-right"),
                    htmlOutput("fishing_mort", align = "right",
                               class = "align-right"),
                    htmlOutput("ecosystem", align = "right",
                               class = "align-right"),
                    htmlOutput("new_data", align = "right",
                               class = "align-right"),
                    htmlOutput("assessment_freq", align = "right",
                               class = "align-right"),
                    htmlOutput("future_spex", align = "right",
                               class = "align-right")
                  )
          ),
          
          # overall ranking page
          tabItem(tabName = "overall_ranking",
                  h1("2024 Stock Assessment Prioritization Ranking"),
                  fluidRow(
                    # overall ranking plot
                    box(title = "Overall Ranking", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        width = 12,
                        plotlyOutput("overall_ranking") %>% withSpinner(),
                        div(style = "display:inline-block", p("Show")),
                        div(style = "display:inline-block",
                            selectInput("num_col", label = NULL,
                                        choices = c("10", "20", "21-40", "41-60", "61-65"),
                                        selected = "10",
                                        width = "75px")
                        )
                    ),
                  ),
                  fluidRow(
                    # weights panel
                    box(title = "Weights", status = "warning",
                        solidHeader = TRUE, width = 12,
                        column(2, textOutput("weights_sum"), htmlOutput("warning"),
                               br(),
                               bsButton("rescale", " Rescale weights",
                                        icon = icon("scale-balanced")),
                               bsPopover("rescale", title = "What happens here?",
                                         content = "Rescaling the weights will
                                         distribute the remainder evenly to all
                                         non-zero weights."),
                               actionButton("reset", " Reset weights",
                                            icon = icon("arrow-rotate-right"))
                        ),
                        div(
                          class = "numeric-input-container",
                          numericInput("comm_weight", "Comm. Importance Factor Weight:",
                                       value = 0.21, min = 0, max = 1, step = 0.01,
                                       width = "100%"
                          ),
                          numericInput("rec_weight", "Rec. Importance Factor Weight:",
                                                 value = 0.09, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("tribal_weight", "Tribal Importance Factor Weight:",
                                                 value = 0.05, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("cd_weight", "Const. Demand Factor Weight:",
                                                 value = 0.11, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("reb_weight", "Rebuilding Factor Weight:",
                                                 value = 0.10, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("ss_weight", "Stock Status Factor Weight:",
                                                 value = 0.08, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("fm_weight", "Fishing Mortality Factor Weight:",
                                                 value = 0.08, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("eco_weight", "Ecosystem Factor Weight:",
                                                 value = 0.05, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("ni_weight", "New Information Factor Weight:",
                                                 value = 0.05, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          ),
                          numericInput("af_weight", "Assmt. Frequency Factor Weight:",
                                                 value = 0.18, min = 0, max = 1, step = 0.01,
                                                 width = "100%"
                          )
                        )
                    )
                  ),
                  fluidRow(
                    # overall ranking table
                    box(title = "Factor Summary", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        width = 12,
                        uiOutput("overall_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("overall_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("overall_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("overall_rds", "Download R object")
                        )
                    )
                  )
          ),
          
          # fishing mortality page
          tabItem(tabName = "fm_page",
                  h1("Fishing Mortality"),
                  fluidRow(
                    # controls panel
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
                                                            "Management Group")
                            )
                          ),
                          tabPanel(
                            "Coloring",
                            br(),
                            checkboxGroupInput("fm_colors", "Select columns to color:",
                                               choices = fm_cols,
                                               selected = c("Rank")
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the percent attainment relative to the
                                  species-specific average overfishing limit (OFL) or the
                                  OFL-contribution for species managed within a complex.
                                  Values range between 10, the maximum score, and 0,
                                  the minimum score."
                                ),
                                p(strong("Average Removals:"),
                                  "The average removals in metric tons across a range of recent years.
                                  Source: GEMM."
                                ),
                                p(strong("Average OFL:"),
                                  "Average OFL or the OFL-contribution across a range of recent years
                                  by species."
                                ),
                                p(strong("Average OFL Attainment:"),
                                  "Percent average attainment of removals relative to the OFL across
                                  a range of years by species."
                                ),
                                p(strong("Average ACL:"),
                                  "Average ACL or the ACL-contribution across a range of recent years
                                  by species."
                                ),
                                p(strong("Average ACL Attainment:"),
                                  "Percent average attainment of removals relative to the ACL across
                                  a range of years by species."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "fm_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # fishing mortality table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("fm_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("fm_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("fm_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("fm_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # fishing mortality plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized
                                  to the maximum initial factor score and scaled to range between
                                  10, the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Interim Value:"),
                                  "Unscaled factor score based on the coastwide revenue by species
                                  exponentiated by a value of 0.18 to account for highly variable
                                  coastwide revenues by species."
                                ),
                                p(strong("Revenue:"),
                                  "The summed inflation adjusted ex-vessel revenue across states
                                  by species. Source: PacFIN."
                                ),
                                p(strong("CA Revenue:"),
                                  "Total inflation adjusted ex-vessel revenue associated by species
                                  within California. Source: PacFIN."
                                ),
                                p(strong("OR Revenue:"),
                                  "Total inflation adjusted ex-vessel revenue associated by species
                                  within Oregon. Source: PacFIN."
                                ),
                                p(strong("WA Revenue:"),
                                  "Total inflation adjusted ex-vessel revenue associated by species
                                  within Washington. Source: PacFIN."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput("com_species_selector",
                                    "Select a species management group:",
                                    choices = c(unique(as.character(species_groups$`Management Group`))),
                                    selected = c(unique(as.character(species_groups$`Management Group`))),
                                    multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # commercial importance table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("com_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("com_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("com_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("com_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # commercial importance plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "The sum of the subsistence score plus the initial factor score.
                                  Values range between 10, the maximum score, and 0, the minimum
                                  score."
                                ),
                                p(strong("Subsistence Score:"),
                                  "A pre-specified relative importance weight by species to the
                                  tribal fishery. Values range between 3.0, maximum score, and 0,
                                  the minimum score."
                                ),
                                p(strong("Initial Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized to the
                                  maximum initial factor score and scaled to range between 7,
                                  the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Interim Value:"),
                                  "Unscaled factor score based on the tribal revenue by species
                                  exponentiated by a value of 0.18 to account for highly variable
                                  revenues by species."
                                ),
                                p(strong("Revenue:"),
                                  "Total revenue based on the inflation adjusted ex-vessel revenue
                                  by species caught within tribal fisheries. Source: PacFIN."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "tribal_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # tribal importance table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("tribal_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("tribal_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("tribal_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("tribal_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # tribal importance plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized to the
                                  maximum initial factor score and scaled to range between 10,
                                  the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Initial Factor Score:"),
                                  "Unscaled factor score based on the coastwide pseudo revenue by
                                  species exponentiated by a value of 0.18 to account for highly
                                  variable coastwide pseudo revenues by species."
                                ),
                                p(strong("Pseudo Revenue Coastwide:"),
                                  "The sum of the state-specific pseudo revenues for each species."
                                ),
                                p(strong("Pseudo Revenue CA:"),
                                  "The state-specific retained recreational catch multiplied by the state-specific
                                  relative importance for California."
                                ),
                                p(strong("Pseudo Revenue OR:"),
                                  "The state-specific retained recreational catch multiplied by the state-specific
                                  relative importance for Oregon."
                                ),
                                p(strong("Pseudo Revenue WA:"),
                                  "The state-specific retained recreational catch multiplied by the state-specific
                                  relative importance for Washington."
                                ),
                                p(strong("Species Importance CA:"),
                                  "The pre-specified relative importance for a recreational
                                  species in California."
                                ),
                                p(strong("Species Importance OR:"),
                                  "The pre-specified relative importance for a recreational
                                  species in Oregon."
                                ),
                                p(strong("Species Importance WA:"),
                                  "The pre-specified relative importance for a recreational
                                  species in Washington."
                                ),
                                p(strong("Retained Catch Coastwide:"),
                                  "The sum of the retained recreational catch by state and species."
                                ),
                                p(strong("Retained Catch CA:"),
                                  "The sum of the retained recreational catch in California by species."
                                ),
                                p(strong("Retained Catch OR:"),
                                  "The sum of the retained recreational catch in Oregon by species."
                                ),
                                p(strong("Retained Catch WA:"),
                                  "The sum of the retained recreational catch in Washington by species."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "rec_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # recreational importance table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("rec_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("rec_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("rec_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("rec_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # recreational importance plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized to the
                                  maximum initial factor score and scaled to range between 12,
                                  the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Choke Stock Adjustment:"),
                                  "Indicates species where ACLs may result in constraint on
                                  opportunities across the groundfish fishery. Species that are
                                  currently managed under rebuilding plans receive the maximum
                                  score of 10. An adjustment values is calculated based on the
                                  percent attainment between recent average mortality to future
                                  ACL values. Adjustments range between -2.0 to 4 for
                                  non-rebuilding species with a value of 4 being given to species
                                  with potential future attainments greater than 100 percent."
                                ),
                                p(strong("Total Modifiers:"),
                                  "The sum of the commercial importance, recreational importance,
                                  landings construed, and species of concern modifiers."
                                ),
                                p(strong("Commercial Importance:"),
                                  "Species with state-specific commercial importance that are not
                                  reflected in the coastwide commercial factor. Values range
                                  between 2 to 0."
                                ),
                                p(strong("Recreational Importance:"),
                                  "Species with state-specific recreational importance that are not
                                  reflected in the coastwide recreational factor. Values range
                                  between 2 to 0."
                                ),
                                p(strong("Landings Constricted:"),
                                  "Species where landings have been constricted due to rebuilding
                                  or have recently rebuilt. Values range between 4 to 0."
                                ),
                                p(strong("Concern:"),
                                  "Species that have been identified by stakeholders or management
                                  to be of high concern. Species of concern are assigned a value
                                  of 1 and all other species receive a value of 0."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "cd_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # constituent demand table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("cd_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("cd_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("cd_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("cd_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # constituent demand plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized to the
                                  maximum initial factor score and scaled to range between 10,
                                  the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Fraction Unfished:"),
                                  "The estimated fraction of unfished biomass from the year of the
                                  most recent assessment. Species with multiple stock areas and/or
                                  assessment areas reflect the coastwide fraction unfished. Species
                                  that do not have a benchmark, update, or data-moderate assessment
                                  estimated fraction unfished are shown as NA."
                                ),
                                p(strong("Target Fraction Unfished:"),
                                  "The management target fraction unfished where the target fraction
                                  unfished is 25 percent for flatfish and 40 percent all other species."
                                ),
                                p(strong("MSST:"),
                                  "The minimum stock status threshold (MSST) that a stock would be
                                  declared overfished where the MSST is 12.5 percent for flatfish
                                  and 25 percent all other species."
                                ),
                                p(strong("PSA:"),
                                  "The productivity susceptibility analysis score from",
                                  tags$a(href="https://afspubs.onlinelibrary.wiley.com/doi/full/10.1080/02755947.2011.591264",
                                         target = "_blank", "Cope et al. 2011."), "The PSA score is
                                  used to calculate the factor score for species that do not have
                                  an assessment with an estimated fraction unfished."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "ss_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br(),
                        p("More information on current and historical groundfish stock status
                          can be found", a(href = "https://connect.fisheries.noaa.gov/species_quadplots/",
                                           target = "_blank", "here."))
                    ),
                    # constituent demand table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("ss_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("ss_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("ss_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("ss_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # constituent demand plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the factor score. Values range between 10, maximum score,
                                  and 0, minimum score where species not currently managed under a
                                  rebuilding plan are given a score of 0. Species currently managed
                                  under a rebuilding plan are scored based on the time to rebuilding
                                  and current trend in the stock abundance. The factor score for species
                                  managed under a rebuilding plan are determined as: 4 = projected to
                                  rebuild in over 20 years, 6 = projected to rebuild within 20 years,
                                  9 = projected to be rebuilt by the next assessment cycle, and
                                  10 = under a rebuilding plan with a declining abundance."
                                ),
                                p(strong("Rebuilding Target Year:"),
                                  "The target rebuilding year for species managed under a rebuilding
                                  plan."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "reb_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # rebuilding table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("reb_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("reb_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("reb_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("reb_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # rebuilding plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the scaled initial factor score for each species.
                                  Calculated based on the initial factor score standardized to the
                                  maximum ecosystem score and scaled to range between 10,
                                  the maximum score, and 0, the minimum score."
                                ),
                                p(strong("Ecosystem Score:"),
                                  "Scoring based on top-down and bottom-up importance scores."
                                ),
                                p(strong("Quantile:"),
                                  "The score of each species of the top-down and bottom-up ecosystem
                                  scores by quantile. Values range from 1 to 0."
                                ),
                                p(strong("Assessment Frequency Adjustment:"),
                                  "An adjustment applied to the assessment frequency factor based
                                  on the ecosystem importance by species. Species in the top third
                                  quantile receive a score of +1, species in the bottom third quantile
                                  receive a score of -1, all other species receive a score of 0."
                                ),
                                p(strong("Ecopath Functional Group:"),
                                  "Species food web grouping within the Ecopath with Ecosim
                                  ecological/ecosystem modeling software."
                                ),
                                p(strong("Top-down Score:"),
                                  "Represents the importance of each species as a predator of managed
                                  or protected species in the California Current ecosystem. This index
                                  represents the total consumption in the ecosystem that can be
                                  attributed to each species."
                                ),
                                p(strong("Bottom-up Score:"),
                                  "Represents the importance of the species as a prey species to
                                  all predators in the California Current ecosystem. This index has
                                  been used to describe the importance of forage species to
                                  ecosystem dynamics."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "eco_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # ecosystem table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("eco_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("eco_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("eco_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("eco_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # ecosystem plot
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
                    # controls panel
                    box(title = "Controls", status = "warning",
                        solidHeader = TRUE, width = 3,
                        tabsetPanel(
                          tabPanel(
                            "Columns",
                            br(),
                            checkboxGroupInput("af_columns", "Select columns to display:",
                                               choices = af_cols,
                                               selected = c("Factor Score",
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the factor score for each species based on the sum total
                                  of years past target assessment frequency and the following
                                  adjustments: beyond assessment, greater than 10 years, less than 6
                                  years, and greater than target assessment frequency."
                                ),
                                p(strong("Recruitment Variation:"),
                                  "The assumed variation, above or below, in annual recruitment
                                  relative to the stock recruitment curve from the most recent
                                  stock assessment. A NA is shown for species without a benchmark
                                  or data-moderate assessment."
                                ),
                                p(strong("Mean Age:"),
                                  "The estimated mean age in fisheries catch from the most recent
                                  assessment. A NA is shown for species without a benchmark or
                                  data-moderate assessment."
                                ),
                                p(strong("Transformed Mean Age:"),
                                  "The mean age in the catch by species transformed to limit
                                  variations across species where the transformed mean age is
                                  calculated as \\( (20 * \\bar{A}_{S})^{0.38} \\)  where \\( \\bar{A}_{S} \\)
                                  is the mean age in the catch by species \\( s \\)."
                                ),
                                p(strong("Recruitment Adjustment:"),
                                  "Values ranging between -1 to 1 based upon the recruitment variation
                                  where species with high recruitment variation (>0.9) are assigned a
                                  value of -1, low recruitment variation (<0.3) are assigned a value
                                  of 1, and all other species assigned a value of 0."
                                ),
                                p(strong("Fishery Importance Adjustment:"),
                                  "Values ranging between -1 to 1 based on the importance by species
                                  where species in the top third ranking are assigned a value of -1,
                                  bottom third ranking are assigned a value of 1, and all other
                                  species assigned a value of 0."
                                ),
                                p(strong("Ecosystem Importance Adjustment:"),
                                  "Values ranging between -1 to 1 based on the ecosystem importance
                                  by species where species in the top third ranking are assigned a
                                  value of -1, bottom third ranking are assigned a value of 1, and
                                  all other species assigned a value of 0."
                                ),
                                p(strong("Target Assessment Frequency:"),
                                  "The target assessment frequency in years based on the transformed
                                  mean age in the catch plus the sum of the adjustments by recruitment
                                  variation, fishery importance, and ecosystem importance capped to
                                  be equal to 10 or less. This adjusted mean catch age was then rounded
                                  to the nearest factor of 2 to align with the PFMC 2-year assessment
                                  cycle and any species with a calculated assessment frequency less
                                  than 4 years or species yet to be assessed were adjusted to be
                                  set equal to 4."
                                ),
                                p(strong("Last Assessment Year:"),
                                  "The year the most recent benchmark, update, or data-moderate
                                  assessment was conducted by species. Species with only a data-limited
                                  assessment are shown as NA."
                                ),
                                p(strong("Years Since Last Assessment:"),
                                  "The number of years since the most recent assessment."
                                ),
                                p(strong("Years Past Target Assessment Frequency:"),
                                  "The difference of the target assessment and the years since the
                                  last assessment. If a species was assessed in the most recent
                                  assessment cycle this value is adjusted to be equal to -4 and any
                                  species yet to be assessed is assigned a value of 4."
                                ),
                                p(strong("Beyond Assessment Adjustment:"),
                                  "An adjustment value of +1 to account for any species where
                                  the time since the last assessment is greater than the target
                                  assessment frequency otherwise species are assigned a value
                                  of 0."
                                ),
                                p(strong("Greater Than 10 Years:"),
                                  "An adjustment value of +1 for any species where the time since
                                  the last assessment is greater than 10 years otherwise species
                                  are assigned a value of 0."
                                ),
                                p(strong("Less Than 6 Years:"),
                                  "An adjustment value of -1 for species where the time since
                                  the last assessment is less than six years and the
                                  Scientific and Statistical Committee indicated that an update
                                  assessment could be conducted."
                                ),
                                p(strong("Greater Than Target Assessment Frequency:"),
                                  "An additional adjustment value of +1 for species where the time
                                  since the last assessment is greater than the target assessment
                                  frequency."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "af_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # assessment frequency table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("af_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("af_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("af_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("af_rds", "Download R object")
                        )
                        
                    )
                  ),
                  fluidRow(
                    # assessment frequency plot
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
                    # controls panel
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
                            )
                          ),
                          tabPanel(
                            "Column Descriptions",
                            br(),
                            div(class = "scrolling-text",
                                p(strong("Rank:"),
                                  "Denotes the rank of a fish species based on this factor."
                                ),
                                p(strong("Factor Score:"),
                                  "Denotes the factor score for each species.
                                  Calculated based on the summed total of the scores from
                                  rockfish steepness prior, abundance information, population
                                  dynamics information, past issues can be addressed."
                                ),
                                p(strong("Year Last Assessed:"),
                                  "The year of the most recent assessment for each species where
                                  a NA indicates that a species has not been assessed or only a
                                  data-limited assessment catch recommendation model has been
                                  conducted."
                                ),
                                p(strong("Assessment Type:"),
                                  "The assessment type for the most recent assessment."
                                ),
                                p(strong("Steepness Prior:"),
                                  "Species that where the most recent assessment was conducted prior
                                  to 2015 and used a fixed value for steepness that was less productive
                                  relative to the current steepness prior."
                                ),
                                p(strong("Abundance Information:"),
                                  "Adjustment for species where either new trend or abundance information
                                  are available or the existing trend information used in the assessment
                                  can be extended by six or more years. Values range from 3 to 0 where
                                  species with the greatest change in abundance information receive
                                  a value of 3."
                                ),
                                p(strong("Population Dynamics Information:"),
                                  "Adjustment for species where new research provides additional
                                  information on population dynamics or biological processes.
                                  Values range from 3 to 0 where species with the critical new
                                  information receive a value of 3."
                                ),
                                p(strong("Past Issues Can Be Addressed:"),
                                  "Adjustment for species where particular issues identified in
                                  previous assessment can be adequately addressed. Values range
                                  from 1 to 0."
                                ),
                                p(strong("Notes:"),
                                  "Description of the new information that could be incorporated
                                  in a new assessment."
                                ),
                                p(strong("Management Group:"),
                                  "Management group associated with a species within the fishery
                                  management plan."
                                )
                            )
                          )
                        ),
                        br(),
                        selectInput(
                          inputId = "ni_species_selector",
                          label = "Select a species management group:",
                          choices = c(unique(as.character(species_groups$`Management Group`))),
                          selected = c(unique(as.character(species_groups$`Management Group`))),
                          multiple = TRUE
                        ),
                        em("To edit your selection, place cursor in the box. Press delete to
                           narrow down your selection."),
                        br(),
                        br()
                    ),
                    # new information table
                    box(title = "Factor Table", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 9,
                        gt_output("ni_gt_table") %>% withSpinner(),
                        div(style = "display:inline-block",
                            downloadButton("ni_csv", "Download CSV")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("ni_xlsx", "Download Excel spreadsheet")
                        ),
                        div(style = "display:inline-block",
                            downloadButton("ni_rds", "Download R object")
                        )
                    )
                  ),
                  fluidRow(
                    # new information plot
                    box(title = "New Information Ranking Plot", status = "primary",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("ni_species_ranking") %>% withSpinner(),
                        width = 12)
                  )
          ),
          
          # stock assessment calendar page
          tabItem(tabName = "calendar",
                  h1("2023 Stock Assessment Calendar"),
                  fluidRow(
                    box(
                      width = 12,
                      h4("Below is an annotated 2023 assessment planning calendar that
                      identifies potential weeks in which STAR panels can be scheduled.
                      Based on the expected availability of 2022 data and the time
                      needed for model development and documentation, it is unlikely
                      that any full assessments could be reviewed before May. As of
                      January 2022, the June and September Pacific Fishery Management Council
                      meeting date have not been announced. Once meeting dates for June
                      and September 2023 are available these weeks will be shaded and
                      potential STAR panel weeks will be finalized."),
                      br(),
                      div(
                        img(src = "figs/calendar.png", height = "467px", width = "600px",
                            style = "display: block; margin-left: auto; margin-right: auto;")
                      ),
                      br()
                    )
                  ),
          ),
          
          # resources page
          tabItem(tabName = "resources",
                  h1("Resources"),
                  fluidRow(
                    box(
                      width = 12,
                      tabsetPanel(
                        tabPanel(
                          "Background Information",
                          br(),
                          tags$ul(
                            class = "custom-bulleted-list",
                            tags$li(
                              tags$a(href="https://www.fisheries.noaa.gov/s3/dam-migration/prioritizingfishstockassessments_finalweb.pdf",
                                     target = "_blank", "Prioritizing Fish Stock Assessments")
                            )
                          )
                        ),
                        tabPanel(
                          "PFMC Decisions",
                          h3("2022"),
                          tags$ul(
                            class = "custom-bulleted-list",
                            tags$li(
                              tags$a(href="https://www.pcouncil.org/september-2022-decision-summary-document/",
                                     target = "_blank", "September 2022 Decision Summary Document")
                            ),
                            tags$li(
                              tags$a(href="https://www.pcouncil.org/documents/2022/09/g-7-a-supplemental-gmt-report-1-2.pdf/#page=2",
                                     target = "_blank", "Final Recommendations of Species to be Assessed in 2023")
                            )
                          )
                        ),
                        tabPanel(
                          "References",
                          br(),
                          tags$ul(
                            class = "custom-bulleted-list",
                            tags$li(
                              tags$a(href="https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1080/02755947.2011.591264",
                                     target = "_blank", "Cope et al. 2011. An Approach to Defining Stock Complexes for U.S. West
                                     Coast Groundfishes Using Vulnerabilities and Ecological Distributions. Fisheries Management. 31:589-604."),
                            ),
                            tags$li(
                              tags$a(href="https://www.sciencedirect.com/science/article/pii/S0304380016301909",
                              target = "_blank", "Koehn et al. 2016. Developing a high taxonomic resolution food web model to
                              assess the functional role of forage fish in the California Current ecosystem. Ecological
                              Modelling 335: 87–100."),
                            ),
                            tags$li(
                              tags$a(href="https://www.science.org/doi/10.1126/science.1209395",
                                     target = "_blank", "Smith et al. 2011. Impacts of Fishing Low–Trophic Level Species on Marine Ecosystems.
                                     Science 333(6046): 1147–1150.")
                            )
                          )
                        )
                      )
                    )
                  )
          ),
          
          # contact us page
          tabItem(tabName = "contact",
                  h1("Contact Us"),
                  h4("We look forward to hearing your feedback and questions."),
                  h4("If you encounter any bugs or functionality issues, please visit the",
                     tags$q("Issues"), "tab in our",
                     tags$a(href="https://github.com/chantelwetzel-noaa/wcgfishSAP/issues",
                            target = "_blank", "repository"), "to report them.",
                     "If you would like to provide suggestions or feedback, feel free to visit the",
                     tags$q("Discussions"),
                     tags$a(href="https://github.com/chantelwetzel-noaa/wcgfishSAP/discussions",
                            target = "_blank", "tab"), "to report them."),
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
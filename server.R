library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(plotly)
library(stringr)
library(viridis)

# load in data
## SPELLING INCONSISTENCIES:
## const_demand, assessment_frequency, ecosystem, new_information,
## rebuilding use sentence case for both species names

## rougheye rockfish has spelling error
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)
const_dem_data <- read.csv("tables/const_demand.csv", header = TRUE)

## table has no rank column
rebuilding_data <- read.csv("tables/rebuilding.csv", header = TRUE)

stock_stat_data <- read.csv("tables/stock_status.csv", header = TRUE)
fish_mort_data <- read.csv("tables/fishing_mortality.csv", header = TRUE)
eco_data <- read.csv("tables/ecosystem.csv", header = TRUE)
new_info_data <- read.csv("tables/new_information.csv", header = TRUE,
                          na.strings = c("", "NA"))

## rank column at end of table
ass_freq_data <- read.csv("tables/assessment_frequency.csv", header = TRUE)

## load in species management groups 
species_groups <- read.csv("tables/species_management_groups.csv", header = TRUE)

# replace species column
com_rev_data$Species <- str_to_sentence(species_groups$speciesName)
tribal_data$Species <- str_to_sentence(species_groups$speciesName)
rebuilding_data$Species <- str_to_sentence(species_groups$speciesName)
stock_stat_data$Species <- str_to_sentence(species_groups$speciesName)
eco_data$Species <- str_to_sentence(species_groups$speciesName)
ass_freq_data$Species <- str_to_sentence(species_groups$speciesName)

# order species alphabetically, replace species column
rec_data <- rec_data[order(rec_data$Species),]
rec_data$Species <- str_to_sentence(species_groups$speciesName)

fish_mort_data <- fish_mort_data[order(fish_mort_data$Species),]
fish_mort_data$Species <- str_to_sentence(species_groups$speciesName)

const_dem_data <- const_dem_data[order(const_dem_data$Species),]
const_dem_data$Species <- str_to_sentence(species_groups$speciesName)

new_info_data <- new_info_data[order(new_info_data$Species),]
new_info_data$Species <- str_to_sentence(species_groups$speciesName)

# join tables + species mgmt. groups
species_groups$speciesName <- str_to_sentence(species_groups$speciesName)
joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName"))
joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName"))
joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName"))
joined_cd_df <- left_join(const_dem_data, species_groups, by = c("Species" = "speciesName"))
joined_reb_df <- left_join(rebuilding_data, species_groups, by = c("Species" = "speciesName"))
joined_ss_df <- left_join(stock_stat_data, species_groups, by = c("Species" = "speciesName"))
joined_fm_df <- left_join(fish_mort_data, species_groups, by = c("Species" = "speciesName"))
joined_eco_df <- left_join(eco_data, species_groups, by = c("Species" = "speciesName"))
joined_ni_df <- left_join(new_info_data, species_groups, by = c("Species" = "speciesName"))
joined_af_df <- left_join(ass_freq_data, species_groups, by = c("Species" = "speciesName"))

# define server logic to display user inputs
shinyServer(function(input, output) {
  
  # commercial revenue table
  output$com_gt_table <- render_gt({
    
    # filter data down to species selected
    joined_com_df <- joined_com_df[joined_com_df$managementGroup %in% input$com_species_selector,]
    
    # create commercial revenue gt table output, display in ascending order by rank
    joined_com_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Commercial Importance",
        subtitle = "Measured by average ex-vessel revenue data
        between 2018-2021"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Interum_Value = "Interim Value",
        CA_Revenue = "California Revenue",
        OR_Revenue = "Oregon Revenue",
        WA_Revenue = "Washington Revenue",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = 3:4, decimals = 2) %>%
      fmt_currency(columns = 5:ncol(com_rev_data), decimals = 0) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      
      # add descriptions of variables as footnotes
      tab_footnote(footnote = "Denotes the rank of a fish species based
                   on a number of factors.",
                   locations = cells_column_labels(columns = Rank)) %>%
      tab_footnote(footnote = "Denotes a weighted sum of all factors.",
                   locations = cells_column_labels(columns = Factor_Score)) %>%
      tab_footnote(footnote = "Denotes a value associated with a fish species.",
                   locations = cells_column_labels(columns = Interum_Value)) %>%
      tab_footnote(footnote = "Total revenue associated with a species
                   along the Pacific Coast States (CA, OR, and WA).",
                   locations = cells_column_labels(columns = Revenue)) %>%
      tab_footnote(footnote = "Total revenue associated with a species
                   within California.",
                   locations = cells_column_labels(columns = CA_Revenue)) %>%
      tab_footnote(footnote = "Total revenue associated with a species
                   within Oregon.",
                   locations = cells_column_labels(columns = OR_Revenue)) %>%
      tab_footnote(footnote = "Total revenue associated with a species
                   within Washington.",
                   locations = cells_column_labels(columns = WA_Revenue)) %>%
      tab_footnote(footnote = "Management group associated with a species
                   within the fishery management plan.",
                   locations = cells_column_labels(columns = managementGroup)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
    
  })
  
  # commercial importance species ranking plot
  com_plot <- ggplot(joined_com_df, aes(x = Species, y = Rank,
                                        Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Commercial Importance",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$com_ranking <- renderPlotly({
    ggplotly(com_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # recreational importance table
  output$rec_gt_table <- render_gt({
    
    # filter data down to species selected
    joined_rec_df <- joined_rec_df[joined_rec_df$managementGroup %in% input$rec_species_selector,]
    
    # create recreational gt table output, display in ascending order by rank
    joined_rec_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Recreational Importance",
        subtitle = "Enter subtitle here"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Initial_Factor_Score = "Initial Factor Score",
        Pseudo_CW = "Pseudo Value Coastwide",
        Pseudo_CA = "Pseudo Value California",
        Pseudo_OR = "Pseudo Value Oregon",
        Pseudo_WA = "Pseudo Value Washington",
        Rel_Weight_CA = "Relative Weight California",
        Rel_Weight_OR = "Relative Weight Oregon",
        Rel_Weight_WA = "Relative Weight Washington",
        Ret_Catch_CW = "Retained Catch Coastwide",
        Ret_Catch_CA = "Retained Catch California",
        Ret_Catch_OR = "Retained Catch Oregon",
        Ret_Catch_WA = "Retained Catch Washington",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = 3:ncol(rec_data), decimals = 2) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      
      # add description of variables as footnotes
      tab_footnote(footnote = "Denotes the rank of a fish species based
                   on a number of factors.",
                   locations = cells_column_labels(columns = Rank)) %>%
      tab_footnote(footnote = "Denotes a weighted sum of all factors.",
                   locations = cells_column_labels(columns = Factor_Score)) %>%
      tab_footnote(footnote = "Given value for the recreational landing
                   of a species along the Pacific Coast States (WA, OR, CA).",
                   locations = cells_column_labels(columns = Pseudo_CW)) %>%
      tab_footnote(footnote = "Given value for the recreational landing
                   of a species in California.",
                   locations = cells_column_labels(columns = Pseudo_CA)) %>%
      tab_footnote(footnote = "Given value for the recreational landing
                   of a species in Oregon.",
                   locations = cells_column_labels(columns = Pseudo_OR)) %>%
      tab_footnote(footnote = "Given value for the recreational landing
                   of a species in Washington.",
                   locations = cells_column_labels(columns = Pseudo_WA)) %>%
      tab_footnote(footnote = "Denotes a weight associated with a fish species
                   in California.",
                   locations = cells_column_labels(columns = Rel_Weight_CA)) %>%
      tab_footnote(footnote = "Denotes a weight associated with a fish species
                   in Oregon.",
                   locations = cells_column_labels(columns = Rel_Weight_OR)) %>%
      tab_footnote(footnote = "Denotes a weight associated with a fish species
                   in Washington.",
                   locations = cells_column_labels(columns = Rel_Weight_WA)) %>%
      tab_footnote(footnote = "Management group associated with a species
                   within the fishery management plan.",
                   locations = cells_column_labels(columns = managementGroup)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # recreational importance species ranking plot
  rec_plot <- ggplot(joined_rec_df, aes(x = Species, y = Rank,
                                        Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Recreational Importance",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$rec_species_ranking <- renderPlotly({
    ggplotly(rec_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # tribal revenue table
  output$tribal_gt_table <- render_gt({
    
    # filter data down to species selected
    joined_tribal_df <- joined_tribal_df[joined_tribal_df$managementGroup %in% input$tribal_species_selector,]
    
    # create tribal revenue gt table output, display in ascending order by rank
    joined_tribal_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Tribal Importance",
        subtitle = "Enter subtitle here"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Subsitence_Score = "Subsistence Score",
        Initial_Factor_Score = "Initial Factor Score",
        Interum_Value = "Interim Value",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = 3:6, decimals = 2) %>%
      fmt_currency(columns = Revenue, decimals = 0) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      
      # add descriptions of variables as footnotes
      tab_footnote(footnote = "Denotes the rank of a fish species based
                   on a number of factors.",
                   locations = cells_column_labels(columns = Rank)) %>%
      tab_footnote(footnote = "Denotes a weighted sum of all factors.",
                   locations = cells_column_labels(columns = Factor_Score)) %>%
      tab_footnote(footnote = "Denotes the subsistence score of a fish species.",
                   locations = cells_column_labels(columns = Subsitence_Score)) %>%
      tab_footnote(footnote = "Factor score before adding the subsistence score.",
                   locations = cells_column_labels(columns = Initial_Factor_Score)) %>%
      tab_footnote(footnote = "Denotes a value associated with a fish species.",
                   locations = cells_column_labels(columns = Interum_Value)) %>%
      tab_footnote(footnote = "Total revenue associated with a species along the
                   Pacific Coast States (WA, OR, CA).",
                   locations = cells_column_labels(columns = Revenue)) %>%
      tab_footnote(footnote = "Management group associated with a species
                   within the fishery management plan.",
                   locations = cells_column_labels(columns = managementGroup)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # tribal importance species ranking plot
  tribal_plot <- ggplot(joined_tribal_df, aes(x = Species, y = Rank,
                                              Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Tribal Importance",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$tribal_species_ranking <- renderPlotly({
    ggplotly(tribal_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # constituent demand table
  output$cd_gt_table <- render_gt({
    
    # filter data down to species selected
    joined_cd_df <- joined_cd_df[joined_cd_df$managementGroup %in% input$cd_species_selector,]
    
    # create recreational gt table output, display in ascending order by rank
    joined_cd_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Constituent Demand"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Choke_Stock = "Choke Stock",
        Commercial_Importance = "Commercial Importance",
        Recreational_Importance = "Recreational Importance",
        Landings_Constricted = "Landings Constricted",
        Percent_Attainment = "Percent Attainment"
      ) %>%
      fmt_percent(columns = Percent_Attainment, decimals = 2,
                  scale_values = FALSE) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # constituent demand species ranking plot
  cd_plot <- ggplot(joined_cd_df, aes(x = Species, y = Rank,
                                      Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Constituent Demand",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$cd_species_ranking <- renderPlotly({
    ggplotly(cd_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # rebuilding table
  output$reb_gt_table <- render_gt({
    joined_reb_df <- joined_reb_df[joined_reb_df$managementGroup %in% input$reb_species_selector,]

    joined_reb_df %>%
      arrange(rebuilding) %>%
      gt() %>%
      tab_header(
        title = "Rebuilding"
      ) %>%
      cols_label(
        rebuilding = "Rebuilding",
        target_year = "Target Year",
        managementGroup = "Management Group"
      ) %>%
      data_color(columns = rebuilding, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # rebuilding species ranking plot - uses rebuilding score
  reb_plot <- ggplot(joined_reb_df, aes(x = Species, y = rebuilding)) +
    geom_segment(aes(x = Species, xend = Species, y = 0, yend = rebuilding),
                 color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    ylim(NA, 10) +
    labs(
      title = "Fish Species Ranking by Rebuilding",
      x = "Species (in alphabetical order)", y = "Rebuilding Score", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$reb_species_ranking <- renderPlotly({
    ggplotly(reb_plot, tooltip = c("x", "y", "color"))
  })
  
  # stock status table
  output$ss_gt_table <- render_gt({
    joined_ss_df <- joined_ss_df[joined_ss_df$managementGroup %in% input$ss_species_selector,]

    joined_ss_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Stock Status"
      ) %>%
      cols_label(
        managementGroup = "Management Group"
      ) %>%
      fmt_percent(columns = 4:6, decimals = 2) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # stock status species ranking plot
  ss_plot <- ggplot(joined_ss_df, aes(x = Species, y = Rank,
                                      Score = Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Stock Status",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$ss_species_ranking <- renderPlotly({
    ggplotly(ss_plot, tooltip = c("x", "y", "Score", "color"))
  })
  
  # fishing mortality table
  output$fm_gt_table <- render_gt({
    joined_fm_df <- joined_fm_df[joined_fm_df$managementGroup %in% input$fm_species_selector,]
    
    joined_fm_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Fishing Mortality"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Fishing_Mortality = "Fishing Mortality",
        OFL_Attain_Percent = "OFL Attain Percent",
        Total_Below_OFL = "Total Below AFL",
        ABC_Attain_Percent = "ABC Attain Percent",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = c(4, 5, 7, 8), decimals = 2) %>%
      fmt_percent(columns = c(6, 9), decimals = 2, scale_values = FALSE) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      data_color(columns = managementGroup, target_columns = OFL,
                 method = "factor",
                 domain = c("minor slope rockfish",
                            "minor nearshore rockfish",
                            "minor shelf rockfish",
                            "other flatfish",
                            "other groundfish"),
                 palette = c("#F9E3D6"),
                 na_color = "white") %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      tab_footnote(footnote = "Cells highlighted red indicate OFL contributions.",
                   locations = cells_column_labels(columns = OFL)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # fishing mortality species ranking plot
  fm_plot <- ggplot(joined_fm_df, aes(x = Species, y = Rank,
                                      Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Fishing Mortality",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$fm_species_ranking <- renderPlotly({
    ggplotly(fm_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # ecosystem table
  output$eco_gt_table <- render_gt({
    joined_eco_df <- joined_eco_df[joined_eco_df$managementGroup %in% input$eco_species_selector,]
    
    joined_eco_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Ecosystem"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Ecosystem_Score = "Ecosystem Score",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = Factor_Score, decimals = 2) %>%
      fmt_percent(columns = Quantile, decimals = 0) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # ecosystem species ranking plot
  eco_plot <- ggplot(joined_eco_df, aes(x = Species, y = Rank,
                                        Factor_Score = Factor_Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Ecosystem",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$eco_species_ranking <- renderPlotly({
    ggplotly(eco_plot, tooltip = c("x", "y", "Factor_Score", "color"))
  })
  
  # new information table
  output$ni_gt_table <- render_gt({
    joined_ni_df <- joined_ni_df[joined_ni_df$managementGroup %in% input$ni_species_selector,]
    
    joined_ni_df %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "New Information"
      ) %>%
      cols_label(
        Factor_score = "Factor Score",
        Last_full_assessment = "Last Full Assessment",
        Las_assessment = "Last Assessment",
        steepness_prior = "Steepness Prior",
        abundance_info = "Abundance Info",
        dynamics_info = "Dynamics Info",
        issues = "Issues",
        notes = "Notes",
        managementGroup = "Management Group"
      ) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # new information species ranking plot
  ni_plot <- ggplot(joined_ni_df, aes(x = Species, y = Rank,
                                      Factor_score = Factor_score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by New Information",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$ni_species_ranking <- renderPlotly({
    ggplotly(ni_plot, tooltip = c("x", "y", "Factor_score", "color"))
  })
  
  # assessment frequency table
  output$af_gt_table <- render_gt({
    joined_af_df <- joined_af_df[joined_af_df$managementGroup %in% input$af_species_selector,]
    
    joined_af_df %>%
      select(Species, Rank, Score, Recruit_Var:managementGroup) %>%
      arrange(Rank) %>%
      gt() %>%
      tab_header(
        title = "Assessment Frequency"
      ) %>%
      cols_label(
        Recruit_Var = "Recruitment Variability",
        MeanAge = "Mean Age",
        Trans_MeanAge = "Transformed Mean Age",
        Recruit_Adj = "Adjustment for Recruit. Var.",
        Mortality_Adj = "Adjustment for Mort.",
        Eco_Adj = "Adjustment for Eco.",
        Total_Adj = "Total Adjustment",
        MeanAge_Adj = "Adjusted Mean Age",
        MeanAge_Adj_Round = "Adjusted Mean Age (rounded)",
        Last_Assess = "Last Assessment",
        Years_Since_Assess = "Years since Assessment",
        Beyond_Target_Freq = "Beyond Target Frequency",
        Adj_Negative = "Negative Adjustment",
        Greater_Than_10 = "Greater Than 10",
        Less_Than_6_Update = "Less Than 6 - Update",
        Greater_Than_Target_Freq = "Greater Than Target Frequency",
        Constraint_2022 = "2022 Constraint",
        managementGroup = "Management Group"
      ) %>%
      fmt_number(columns = c(3:5, 10), decimals = 2) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # assessment frequency species ranking plot
  af_plot <- ggplot(joined_af_df, aes(x = Species, y = Rank,
                                      Score = Score)) +
    geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                 color = "gray") +
    geom_hline(yintercept = 65, color = "gray") +
    geom_point(aes(color = managementGroup), size = 3) +
    scale_y_reverse() +
    labs(
      title = "Fish Species Ranking by Fishing Mortality",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme_light() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$af_species_ranking <- renderPlotly({
    ggplotly(af_plot, tooltip = c("x", "y", "Score", "color"))
  })
  
  # tab where user can input own .csv file, create gt table
  # upload file
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  # clean file
  tidied <- reactive({
    out <- data()
    if(input$rename) {
      names(out) <- gsub("_", " ", names(out))
    }

    out
  })
  
  # produce gt table of selected file
  output$table <- render_gt({
    tidied() %>%
      arrange(Rank) %>%
      gt() %>%
      fmt_number(columns = 3:ncol(tidied()), decimals = 2) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # interactive ranking plot of selected file
  output$test_species_ranking <- renderPlotly({
    ggplot(tidied(), aes(x = Species, y = Rank, size = Rank)
           ) + geom_point() +
      labs(
        title = "Fish Species Ranking",
        caption = "Click points to view more information about selected entries.",
        x = "Species (in alphabetical order)", y = "Rank") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      scale_color_viridis(discrete = TRUE)
  })
})
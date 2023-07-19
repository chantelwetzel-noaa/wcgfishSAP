library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(plotly)
library(stringr)
library(tidyr)
library(viridis)

# load in data
## SPELLING INCONSISTENCIES:
## const_demand, assessment_frequency, ecosystem, new_information,
## rebuilding use sentence case for both species names

## rougheye rockfish has spelling error
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

const_dem_data <- read.csv("tables/const_demand.csv", header = TRUE) %>%
  mutate_at(c("Commercial_Importance", "Recreational_Importance",
              "Landings_Constricted"), ~replace_na(., 0))

## table has no rank column
rebuilding_data <- read.csv("tables/rebuilding.csv", header = TRUE)

stock_stat_data <- read.csv("tables/stock_status.csv", header = TRUE)

fish_mort_data <- read.csv("tables/fishing_mortality.csv", header = TRUE)

eco_data <- read.csv("tables/ecosystem.csv", header = TRUE) %>%
  mutate(Factor_Score = na_if(Factor_Score, " -   "))
eco_data$Factor_Score <- as.numeric(eco_data$Factor_Score)

new_info_data <- read.csv("tables/new_information.csv", header = TRUE)

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

joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_cd_df <- left_join(const_dem_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_reb_df <- left_join(rebuilding_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(desc(Currently_Rebuilding))

joined_ss_df <- left_join(stock_stat_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_fm_df <- left_join(fish_mort_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_eco_df <- left_join(eco_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_ni_df <- left_join(new_info_data, species_groups, by = c("Species" = "speciesName")) %>%
  arrange(Rank)

joined_af_df <- left_join(ass_freq_data, species_groups, by = c("Species" = "speciesName")) %>%
  select(Species, Rank, Score, Recruit_Variation:managementGroup) %>%
  arrange(Rank)


# define server logic to display user inputs
shinyServer(function(input, output) {
  
  
  # commercial revenue table
  output$com_gt_table <- render_gt({
    
    # filter data down to species selected
    joined_com_df <- joined_com_df[joined_com_df$managementGroup %in% input$com_species_selector,]
    
    # decimal_cols <- input$com_columns[sapply(input$com_columns, is.numeric)]

    # create commercial revenue gt table output, display in ascending order by rank
    com_base_table <- joined_com_df %>%
      select(input$com_columns) %>%
      gt() %>%
      tab_header(
        title = "Commercial Importance",
        subtitle = "Measured by average ex-vessel revenue data
        between 2018-2021"
      )
      # data_color(columns = -c("Species"), method = "auto", palette = "viridis")
    
    if("Rank" %in% input$com_columns) {
      com_base_table <- com_base_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    } else {
      com_base_table <- com_base_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    com_base_table %>%
      fmt_currency(columns = contains("Revenue"), decimals = 0) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
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
    rec_table <- joined_rec_df %>%
      select(input$rec_columns) %>%
      gt() %>%
      tab_header(
        title = "Recreational Importance"
      )
    
    if("Rank" %in% input$rec_columns) {
      rec_table <- rec_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    } else {
      rec_table <- rec_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    rec_table %>%
      fmt_currency(columns = contains("Pseudo"), decimals = 0) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
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
    tribal_table <- joined_tribal_df %>%
      select(input$tribal_columns) %>%
      gt() %>%
      tab_header(
        title = "Tribal Importance"
      )
    
    if("Rank" %in% input$tribal_columns) {
      tribal_table <- tribal_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    } else {
      tribal_table <- tribal_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    tribal_table %>%
      fmt_currency(columns = contains("Revenue"), decimals = 0) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
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
    cd_table <- joined_cd_df %>%
      select(input$cd_columns) %>%
      gt() %>%
      tab_header(
        title = "Constituent Demand"
      )
    
    if("Rank" %in% input$cd_columns) {
      cd_table <- cd_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    } else {
      cd_table <- cd_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    cd_table %>%
      fmt_percent(columns = contains("Percent"), decimals = 1,
                  scale_values = FALSE) %>%
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

    reb_table <- joined_reb_df %>%
      select(input$reb_columns) %>%
      gt() %>%
      tab_header(
        title = "Rebuilding"
      )
    
    if("rebuilding" %in% input$reb_columns) {
      reb_table <- reb_table %>%
        fmt_number(columns = -c("rebuilding"), decimals = 2) %>%
        data_color(columns = rebuilding, method = "numeric", palette = "viridis")
    } else {
      reb_table <- reb_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    reb_table %>%
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

    ss_table <- joined_ss_df %>%
      select(input$ss_columns) %>%
      gt() %>%
      tab_header(
        title = "Stock Status"
      )
    
    if("Rank" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    }
    
    if("Estimate" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        fmt_percent(columns = Estimate, decimals = 1)
    }
    
    if("Target" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        fmt_percent(columns = Target, decimals = 1)
    }
    
    if("MSST" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        fmt_percent(columns = MSST, decimals = 1)
    }
    
    ss_table %>%
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
  
  
  # fishing mortality table - CREATE CONDITIONALS
  output$fm_gt_table <- render_gt({
    joined_fm_df <- joined_fm_df[joined_fm_df$managementGroup %in% input$fm_species_selector,]
    
    fm_table <- joined_fm_df %>%
      select(input$fm_columns) %>%
      gt() %>%
      tab_header(
        title = "Fishing Mortality"
      )
    
    if("Rank" %in% input$fm_columns) {
      fm_table <- fm_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    } else {
      fm_table <- fm_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    if("OFL_Attain_Percent" %in% input$fm_columns) {
      fm_table <- fm_table %>%
        tab_style(style = list(cell_fill(color = "red"),
                               cell_text(color = "white")),
                  locations = cells_body(columns = OFL_Attain_Percent,
                                         rows = OFL_Attain_Percent > 1.00)
        ) %>%
        tab_footnote(footnote = "Cells highlighted bright red indicate
                     high OFL attainment percentages.",
                     locations = cells_column_labels(columns = OFL_Attain_Percent))
    }
    
    fm_table %>%
      fmt_percent(columns = contains("Percent"), decimals = 1) %>%
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
      
      # doesn't appear?
      tab_footnote(
        footnote = "Cells highlighted light red indicate OFL contributions.",
        locations = cells_column_labels(columns = OFL)
      ) %>%
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
  
  
  # ecosystem table - factor score has 3 decimals
  output$eco_gt_table <- render_gt({
    joined_eco_df <- joined_eco_df[joined_eco_df$managementGroup %in% input$eco_species_selector,]
    
    eco_table <- joined_eco_df %>%
      select(input$eco_columns) %>%
      gt() %>%
      tab_header(
        title = "Ecosystem"
      )
    
    if("Rank" %in% input$eco_columns) {
      eco_table <- eco_table %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    }
    
    if("Factor_Score" %in% input$eco_columns) {
      eco_table <- eco_table %>%
        fmt_number(columns = Factor_Score, decimals = 2)
    }
    
    eco_table %>%
      fmt_number(columns = ends_with("Score"), decimals = 2) %>%
      fmt_percent(columns = contains("Quantile"), decimals = 1) %>%
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
    
    ni_table <- joined_ni_df %>%
      select(input$ni_columns) %>%
      gt() %>%
      tab_header(
        title = "New Information"
      )
    
    if("Rank" %in% input$ni_columns) {
      ni_table <- ni_table %>%
        data_color(columns = Rank, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    }
    
    ni_table %>%
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
    
    af_table <- joined_af_df %>%
      select(input$af_columns) %>%
      gt() %>%
      tab_header(
        title = "Assessment Frequency"
      )
    
    if("Score" %in% input$af_columns) {
      af_table <- af_table %>%
        data_color(columns = Score, method = "numeric", palette = "viridis",
                   reverse = TRUE)
    }
    
    af_table %>%
      fmt_number(columns = contains("Age"), decimals = 2) %>%
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
      fmt_number(columns = -c("Rank"), decimals = 2) %>%
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
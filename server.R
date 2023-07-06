library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(plotly)
library(stringr)
library(viridis)

# load in commercial revenue data + clean up rougheye rockfish name
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)
com_rev_data[com_rev_data == "rougheye rocommercial_revenueckfish"] <- "rougheye rockfish"

# load in rest of data
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)
const_dem_data <- read.csv("tables/const_demand.csv", header = TRUE)

# table has no rank column
rebuilding_data <- read.csv("tables/rebuilding.csv", header = TRUE)

stock_stat_data <- read.csv("tables/stock_status.csv", header = TRUE)
fish_mort_data <- read.csv("tables/fishing_mortality.csv", header = TRUE)
eco_data <- read.csv("tables/ecosystem.csv", header = TRUE)
new_info_data <- read.csv("tables/new_information.csv", header = TRUE)

# rank column at end of table
ass_freq_data <- read.csv("tables/assessment_frequency.csv", header = TRUE)

# SPELLING INCONSISTENCIES:
## const_demand, assessment_frequency, ecosystem, new_information,
## rebuilding use sentence case for both species names

# load in species management groups 
species_groups <- read.csv("tables/species_management_groups.csv", header = TRUE)

# clean up cryptic species names
species_groups[species_groups == "blue/deacon rockfish"] <- "blue rockfish"
species_groups[species_groups == "gopher/black and yellow rockfish"] <- "gopher rockfish"
species_groups[species_groups == "rougheye/blackspotted rockfish"] <- "rougheye rockfish"
species_groups[species_groups == "vermilion/sunset rockfish"] <- "vermilion rockfish"

# join data + species management groups
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

# convert all species names to sentence case
joined_com_df$Species <- str_to_sentence(joined_com_df$Species)
joined_tribal_df$Species <- str_to_sentence(joined_tribal_df$Species)
joined_rec_df$Species <- str_to_sentence(joined_rec_df$Species)
joined_fm_df$Species <- str_to_sentence(joined_fm_df$Species)

# define server logic to display user inputs
shinyServer(function(input, output) {
  
  # commercial revenue table
  output$com_data_viewer <- render_gt({
    
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
      opt_footnote_marks(marks = c("1", "2", "3", "4", "5", "6", "7", "8")) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
    
  })
  
  # commercial importance species ranking plot
  com_plot <- ggplot(joined_com_df, aes(x = Species, y = Rank, size = Rank)) +
    geom_point(aes(color = managementGroup)) +
    scale_size(trans = "reverse") +
    labs(
      title = "Fish Species Ranking by Commercial Importance",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$com_species_ranking <- renderPlotly({
    ggplotly(com_plot, tooltip = c("x", "y", "color"))
  })
  
  # tribal revenue table
  output$tribal_data_viewer <- render_gt({
    
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
      opt_footnote_marks(marks = c("1", "2", "3", "4", "5", "6", "7")) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # tribal importance species ranking plot
  tribal_plot <- ggplot(joined_tribal_df, aes(x = Species, y = Rank, size = Rank)) +
    geom_point(aes(color = managementGroup)) +
    scale_size(trans = "reverse") +
    labs(
      title = "Fish Species Ranking by Tribal Importance",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$tribal_species_ranking <- renderPlotly({
    ggplotly(tribal_plot, tooltip = c("x", "y", "color"))
  })
  
  # recreational importance table
  output$rec_data_viewer <- render_gt({
    
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
      
      # shades cells w/ NA values red
      # using brute force—find better way to do this
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Pseudo_CA,
                                       rows = is.na(Pseudo_CA))) %>%
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Pseudo_OR,
                                       rows = is.na(Pseudo_OR))) %>%
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Pseudo_WA,
                                       rows = is.na(Pseudo_WA))) %>%
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Rel_Weight_CA,
                                       rows = is.na(Rel_Weight_CA))) %>%
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Rel_Weight_OR,
                                       rows = is.na(Rel_Weight_OR))) %>%
      tab_style(style = list(cell_fill(color = "#F9E3D6"),
                             cell_text(style = "italic")),
                locations = cells_body(columns = Rel_Weight_WA,
                                       rows = is.na(Rel_Weight_WA))) %>%
      
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
      opt_footnote_marks(marks = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                   "10")) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # recreational importance species ranking plot
  rec_plot <- ggplot(joined_rec_df, aes(x = Species, y = Rank, size = Rank)) +
    geom_point(aes(color = managementGroup)) +
    scale_size(trans = "reverse") +
    labs(
      title = "Fish Species Ranking by Recreational Importance",
      caption = "Click points to view more information about selected entries.",
      x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_color_viridis(discrete = TRUE)
  
  output$rec_species_ranking <- renderPlotly({
    ggplotly(rec_plot, tooltip = c("x", "y", "color"))
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
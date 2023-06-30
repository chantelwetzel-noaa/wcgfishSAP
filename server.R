library(shiny)
library(dplyr)
library(gt)
library(gtExtras)
library(viridis)

# read in commercial revenue data
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# read in tribal revenue data
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

# read in recreational revenue data 
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

# read in species management groups
species_groups <- read.csv("tables/species_management_groups.csv", header = TRUE)
# species_groups <- species_groups %>%
#   rename(Species = speciesName)

# join data + species management groups
joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName"))
joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName"))
joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName"))

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
      opt_interactive(use_search = TRUE)
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
      opt_interactive(use_search = TRUE)
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
      # using brute force—is there a better way to do this??
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
      opt_interactive(use_search = TRUE)
  })
  
  # tab where user can input own .csv file, create gt table
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  output$table <- render_gt({
    data() %>%
      arrange(Rank) %>%
      gt()
  })
})
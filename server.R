library(shiny)
library(dplyr)
library(DT)
library(gt)
library(gtExtras)
library(viridis)

#Read in Static commercial revenue file, maybe not best practice
com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

# Loaded Tribal Revenue Data - Will look to change 
tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

#  Loaded Recreational Revenue Data - Will look to change 
rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

# Define server logic to display user inputs
shinyServer(function(input, output) {
  
  #Gets Data from Commercial Revenue csv file and shows it on a Table
  #Table can Filter based on Species name or Ascend order by Rank
  output$com_data_viewer <- render_gt({
    
    # filter data down to species selected
    com_rev_data <- com_rev_data[com_rev_data$Species %in% input$com_species_selector,]
    
    # order rank in ascending order
    com_rev_data <- arrange(com_rev_data, Rank)
    
    # create commercial revenue gt table output
    gt(com_rev_data) %>%
      tab_header(
        title = "Commercial Importance"
      ) %>%
      cols_label(
        Factor_Score = "Factor Score",
        Interum_Value = "Interum Value",
        CA_Revenue = "California Revenue",
        OR_Revenue = "Oregon Revenue",
        WA_Revenue = "Washington Revenue"
      ) %>%
      fmt_number(columns = 3:ncol(com_rev_data), decimals = 2) %>%
      fmt_currency(columns = 5:ncol(com_rev_data), decimals = 0) %>%
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) %>%
      opt_interactive(use_search = TRUE)
  })
  
  #Can display and filter Tribal Fish Data by name and rank value
  output$tribal_data_viewer <- DT::renderDataTable({
    # Renamed CSV Columns
    tribal_col <- c("Species", 
                    "Rank", 
                    "Factor Score", 
                    "Subsistence Score",  
                    "Initial Factor Score",
                    "Interum Value",
                    "Revenue")
    
    # Change data
    tribal_data <- tribal_data[tribal_data$Species %in% input$tribal_species_selector,]
    
    
    # create tribal datatable output
    datatable(tribal_data, options(lengthMenu = list(c(10, 20, 30, -1), 
                                                     c(10, 20, 30, "All")),
                                   order = list(list(1, "asc"))),
              class = "cell-border stripe", rownames = FALSE, colnames = tribal_col) %>%
      
      # round numeric entries + add $ sign to monetary values
      formatRound(5:ncol(tribal_data), 2) %>%
      formatRound(3, 2) %>%
      formatCurrency(ncol(tribal_data), currency = "$", digits = 0) %>%
      
      # assign color coding
      formatStyle(columns = "Species", fontWeight = "bold") %>% 
      formatStyle("Rank", background = styleInterval(cuts = c(10, 20, 30 , 40, 50, 60),  
                                                     values = c("#FDE725FF", "#95D840FF",
                                                                "#3CBB75FF", "#1F968BFF",
                                                                "#2D708EFF", "#404788FF",
                                                                "#481567FF")),
                  color = styleInterval(cuts = c(30), values = c("black", "white")))
  })
  
  # Can display and filter Recreational Fish Data by name and rank value
  output$rec_data_viewer <- DT::renderDataTable({
    # Renamed CSV Columns
    rec_col <- c("Species", 
                 "Rank", 
                 "Factor Score", 
                 "Initial Factor Score",
                 "Pseudo Value Coast Wide",
                 "Pseudo Value California",
                 "Pseudo Value Oregon",
                 "Pseudo Value Washington",
                 "Relative Weight California",
                 "Relative Weight Oregon",
                 "Relative Weight Washington",
                 "Retained Catch Coast Wide",
                 "Retained Catch California",
                 "Retained Catch Oregon",
                 "Retained Catch Washington")
    
    # Change data
    rec_data <- rec_data[rec_data$Species %in% input$rec_species_selector,]
    
    # create recreational datatable output
    datatable(rec_data, options(lengthMenu = list(c(10, 20, 30, -1), #ALL is a Keyword
                                                  c(10, 20, 30, "All")),
                                order = list(list(1, "asc"))),
              class = "cell-border stripe", rownames = FALSE, colnames = rec_col) %>%
      
      # round numeric entries + add $ sign to monetary values
      formatRound(5:ncol(rec_data), 0) %>%
      formatRound(3:4, 2) %>%
      formatRound(2, 0) %>%
      formatCurrency(ncol(rec_data), currency = "$", digits = 0) %>%
      
      # assign color coding
      formatStyle(columns = "Species", backgroundColor = "#FD5C63", color = "white",
                  fontWeight = "bold") %>%
      formatStyle("Rank", background = styleInterval(cuts = c(10, 20, 30, 40, 50, 60),
                                                     values = c("#F4F9F4", "#A7D7C5",
                                                                "#74B49B", "#5C8D89",
                                                                "#698474", "#2C5D63",
                                                                "#283739")),
                  color = styleInterval(cuts = c(30), values = c("black", "white")))
  })
})
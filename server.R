library(shiny)
library(DT)
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
  output$com_data_viewer <- DT::renderDataTable({
    # Renamed CSV Columns
    com_rev_col <- c("Species", 
                      "Rank", 
                      "Factor Score", 
                      "Interum Value",  
                      "Revenue",
                      "California Revenue",
                      "Oregon Revenue",
                      "Washington Revenue")
    
    # Change data
    com_rev_data <- com_rev_data[com_rev_data$Species %in% input$com_species_selector,]
    
    # create commercial rev. datatable output
    datatable(com_rev_data, options = options(lengthMenu = list(c(10, 20, 30, -1),
                                                                c(10, 20, 30, "All")),
                                              order = list(list(1, "asc"))),
              class = "cell-border stripe", rownames = FALSE, colnames = com_rev_col) %>%
      
      # round numeric entries + add $ sign for monetary values
      formatRound(3:ncol(com_rev_data), 2) %>%
      formatCurrency(5:ncol(com_rev_data), currency = "$", digits = 0) %>%
      
      # assign color coding
      formatStyle(columns = "Species", backgroundColor = "#5F9EA0",
                  color = "white", fontWeight = "bold") %>%
      formatStyle("Rank", background = styleInterval(cuts = c(10, 20, 30, 40, 50, 60),
                                                     values = c("#F4F9F4", "#A7D7C5",
                                                                "#74B49B", "#5C8D89",
                                                                "#698474", "#2C5D63",
                                                                "#283739")),
                  color = styleInterval(cuts = c(30), values = c("black", "white")))
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
      formatStyle(columns = "Species", backgroundColor = "#6A5ACD", color = "white",
                  fontWeight = "bold") %>% 
      formatStyle("Rank", background = styleInterval(cuts = c(10, 20, 30 , 40, 50, 60),  
                                                     values = c("#F4F9F4", "#A7D7C5",
                                                                "#74B49B", "#5C8D89",
                                                                "#698474", "#2C5D63",
                                                                "#283739")),
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
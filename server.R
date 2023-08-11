library(shiny)
library(shinyBS)
library(plyr)
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

com_rev_data <- read.csv("tables/commercial_revenue.csv", header = TRUE)

rec_data <- read.csv("tables/recreational_importance.csv", header = TRUE)

tribal_data <- read.csv("tables/tribal_revenue.csv", header = TRUE)

const_dem_data <- read.csv("tables/const_demand.csv", header = TRUE) %>%
  mutate_at(c("Commercial_Importance", "Recreational_Importance",
              "Landings_Constricted"), ~replace_na(., 0))
# adjust negative scores
cd_adj <- -min(const_dem_data[, 3])
const_dem_data$Factor_Score <- const_dem_data[, 3] + cd_adj

## table has no rank column
rebuilding_data <- read.csv("tables/rebuilding.csv", header = TRUE)
rebuilding_data <- replace(rebuilding_data, rebuilding_data == "", NA)

stock_stat_data <- read.csv("tables/stock_status.csv", header = TRUE)

fish_mort_data <- read.csv("tables/fishing_mortality.csv", header = TRUE)

eco_data <- read.csv("tables/ecosystem.csv", header = TRUE)

new_info_data <- read.csv("tables/new_information.csv", header = TRUE)
new_info_data <- replace(new_info_data, new_info_data == "", NA)

## rank column at end of table
assess_freq_data <- read.csv("tables/assessment_frequency.csv", header = TRUE)
# adjust negative scores
af_adj <- -min(assess_freq_data[, 3])
assess_freq_data$Score <- assess_freq_data[, 3] + af_adj


## load in species management groups, format cryptic species names
species_groups <- read.csv("tables/species_management_groups.csv", header = TRUE)
species_groups <- format_species_names(x = species_groups)


# replace species column
com_rev_data$Species <- species_groups$speciesName
tribal_data$Species <- species_groups$speciesName
rebuilding_data$Species <- species_groups$speciesName
stock_stat_data$Species <- species_groups$speciesName
eco_data$Species <- species_groups$speciesName
assess_freq_data$Species <- species_groups$speciesName


# order species alphabetically, replace species column
rec_data <- rec_data[order(rec_data$Species),]
rec_data$Species <- species_groups$speciesName

fish_mort_data <- fish_mort_data[order(fish_mort_data$Species),]
fish_mort_data$Species <- species_groups$speciesName

const_dem_data <- const_dem_data[order(const_dem_data$Species),]
const_dem_data$Species <- species_groups$speciesName

new_info_data <- new_info_data[order(new_info_data$Species),]
new_info_data$Species <- species_groups$speciesName


# join tables + species mgmt. groups, rename columns
joined_com_df <- left_join(com_rev_data, species_groups, by = c("Species" = "speciesName"))
joined_com_df <- joined_com_df %>%
  rename_with(~gsub("_", " ", colnames(joined_com_df))) %>%
  arrange(Rank)

joined_rec_df <- left_join(rec_data, species_groups, by = c("Species" = "speciesName"))
joined_rec_df <- joined_rec_df %>%
  rename_with(~gsub("_", " ", colnames(joined_rec_df))) %>%
  arrange(Rank)

joined_tribal_df <- left_join(tribal_data, species_groups, by = c("Species" = "speciesName"))
joined_tribal_df <- joined_tribal_df %>%
  rename_with(~gsub("_", " ", colnames(joined_tribal_df))) %>%
  arrange(Rank)

joined_cd_df <- left_join(const_dem_data, species_groups, by = c("Species" = "speciesName"))
joined_cd_df <- joined_cd_df %>%
  rename_with(~gsub("_", " ", colnames(joined_cd_df))) %>%
  arrange(Rank)

joined_reb_df <- left_join(rebuilding_data, species_groups, by = c("Species" = "speciesName"))
joined_reb_df <- joined_reb_df %>%
  rename_with(~gsub("_", " ", colnames(joined_reb_df))) %>%
  arrange(desc(`Factor Score`))

joined_ss_df <- left_join(stock_stat_data, species_groups, by = c("Species" = "speciesName"))
joined_ss_df <- joined_ss_df %>%
  rename_with(~gsub("_", " ", colnames(joined_ss_df))) %>%
  arrange(Rank)

joined_fm_df <- left_join(fish_mort_data, species_groups, by = c("Species" = "speciesName"))
joined_fm_df <- joined_fm_df %>%
  rename_with(~gsub("_", " ", colnames(joined_fm_df))) %>%
  arrange(Rank)

joined_eco_df <- left_join(eco_data, species_groups, by = c("Species" = "speciesName"))
joined_eco_df <- joined_eco_df %>%
  rename_with(~gsub("_", " ", colnames(joined_eco_df))) %>%
  arrange(Rank)

joined_ni_df <- left_join(new_info_data, species_groups, by = c("Species" = "speciesName"))
joined_ni_df <- joined_ni_df %>%
  rename_with(~gsub("_", " ", colnames(joined_ni_df))) %>%
  arrange(Rank)

joined_af_df <- left_join(assess_freq_data, species_groups, by = c("Species" = "speciesName"))
joined_af_df <- joined_af_df %>%
  rename_with(~gsub("_", " ", colnames(joined_af_df))) %>%
  select(Species, Rank, Score, `Recruit Variation`:managementGroup) %>%
  arrange(Rank)


# define server logic to display user inputs
shinyServer(function(input, output, session) {
  
  # overall ranking table
  results <- data.frame(species_groups$speciesName,
                        com_rev_data$Factor_Score,
                        rec_data$Factor_Score,
                        tribal_data$Factor_Score,
                        const_dem_data$Factor_Score,
                        rebuilding_data$Factor_Score,
                        stock_stat_data$Score,
                        fish_mort_data$Factor_Score,
                        eco_data$Factor_Score,
                        new_info_data$Factor_score,
                        assess_freq_data$Score)
  
  # add factor weights
  comm_weight <- reactive(input$comm_weight)
  rec_weight <- reactive(input$rec_weight)
  tribal_weight <- reactive(input$tribal_weight)
  cd_weight <- reactive(input$cd_weight)
  reb_weight <- reactive(input$reb_weight)
  ss_weight <- reactive(input$ss_weight)
  fm_weight <- reactive(input$fm_weight)
  eco_weight <- reactive(input$eco_weight)
  ni_weight <- reactive(input$ni_weight)
  af_weight <- reactive(input$af_weight)
  sum_weights <- reactive(round(comm_weight() + rec_weight() + tribal_weight() +
                                cd_weight() + reb_weight() + ss_weight() + fm_weight() +
                                eco_weight() + ni_weight() + af_weight(), 3))
  
  # display sum of factor weights
  output$weights_sum <- renderText({
    paste("Sum of weights:", sum_weights())
  })
  
  # warning if sum is > 1.00
  output$warning <- renderText({
    if(sum_weights() != 1.00) {
      paste("<span style=\"color:red\">WARNING: Ensure all weights add up to 1.</span>")
    }
  })
  
  # reset weights if button is pressed
  observeEvent(input$reset, {
    updateNumericInput(session, "comm_weight", value = 0.21)
    updateNumericInput(session, "rec_weight", value = 0.09)
    updateNumericInput(session, "tribal_weight", value = 0.05)
    updateNumericInput(session, "cd_weight", value = 0.11)
    updateNumericInput(session, "reb_weight", value = 0.10)
    updateNumericInput(session, "ss_weight", value = 0.08)
    updateNumericInput(session, "fm_weight", value = 0.08)
    updateNumericInput(session, "eco_weight", value = 0.05)
    updateNumericInput(session, "ni_weight", value = 0.05)
    updateNumericInput(session, "af_weight", value = 0.18)
  }, ignoreInit = TRUE)
  
  # function to rescale weights
  rescale_weights <- function(factor_weights) {
    # count non-zero weights
    count <- length(factor_weights[factor_weights > 0])
    
    # avoid zero division
    if(count == 0) {
      rescaled_weights <- rep(1 / length(factor_weights), length(factor_weights))
    } else {
      rescaled_weights <- factor_weights
      rem <- (1.000 - sum_weights()) / count
      rescaled_weights[factor_weights > 0] <- factor_weights[factor_weights > 0] + rem
    }
    return(rescaled_weights)
  }
  
  # add popover to rescale button
  addPopover(session, "rescale", title = "What happens here?",
             placement = "top",
             content = "Rescaling the weights will distribute the remainder
             evenly to all non-zero weights.")
  
  # rescale weights if button is pressed
  observeEvent(input$rescale, {
    # store factor weights
    factor_weights <- reactiveVal(c(comm_weight(), rec_weight(),
                                    tribal_weight(), cd_weight(),
                                    reb_weight(), ss_weight(),
                                    fm_weight(), eco_weight(),
                                    ni_weight(), af_weight()))
    
    factor_weights(rescale_weights(factor_weights()))
    updateNumericInput(session, "comm_weight", value = round(factor_weights()[1], 4))
    updateNumericInput(session, "rec_weight", value = round(factor_weights()[2], 4))
    updateNumericInput(session, "tribal_weight", value = round(factor_weights()[3], 4))
    updateNumericInput(session, "cd_weight", value = round(factor_weights()[4], 4))
    updateNumericInput(session, "reb_weight", value = round(factor_weights()[5], 4))
    updateNumericInput(session, "ss_weight", value = round(factor_weights()[6], 4))
    updateNumericInput(session, "fm_weight", value = round(factor_weights()[7], 4))
    updateNumericInput(session, "eco_weight", value = round(factor_weights()[8], 4))
    updateNumericInput(session, "ni_weight", value = round(factor_weights()[9], 4))
    updateNumericInput(session, "af_weight", value = round(factor_weights()[10], 4))
  }, ignoreInit = TRUE)
  
  
  # create reactive dataframe (used for table + plot)
  overall_data <- reactive({
    # multiply factor scores with weights
    results$com_rev_data.Factor_Score <- results$com_rev_data.Factor_Score * comm_weight()
    results$rec_data.Factor_Score <- results$rec_data.Factor_Score * rec_weight()
    results$tribal_data.Factor_Score <- results$rec_data.Factor_Score * tribal_weight()
    results$const_dem_data.Factor_Score <- results$const_dem_data.Factor_Score * cd_weight()
    results$rebuilding_data.Factor_Score <- results$rebuilding_data.Factor_Score * reb_weight()
    results$stock_stat_data.Score <- results$stock_stat_data.Score * ss_weight()
    results$fish_mort_data.Factor_Score <- results$fish_mort_data.Factor_Score * fm_weight()
    results$eco_data.Factor_Score <- results$eco_data.Factor_Score * eco_weight()
    results$new_info_data.Factor_score <- results$new_info_data.Factor_score * ni_weight()
    results$assess_freq_data.Score <- results$assess_freq_data.Score * af_weight()
    
    # create column with weighted sum
    results$total <- rowSums(results[2:11])
    
    results <- results %>%
      arrange(desc(total))
    
    # create rank column
    results$rank <- NA
    order_totals <- order(results$total, results$species_groups.speciesName,
                          decreasing = TRUE)
    results$rank[order_totals] <- 1:nrow(results)
    
    results <- results %>%
      select(species_groups.speciesName, rank, total,
             com_rev_data.Factor_Score:assess_freq_data.Score)
    
    colnames(results) <- c("Species", "Rank", "Total",
                           "Comm.", "Rec.", "Tribal", "Const. Dem.",
                           "Rebuild", "Stock Status", "Fishing Mort.",
                           "Ecosystem", "New Info", "Assess. Freq.")
    
    results
  })
  
  
  # overall ranking table
  output$overall_gt_table <- renderUI({
    req(overall_data())
    
    # create table if weights sum to 1
    if(sum_weights() == 1.00) {
      overall_table <- overall_data() %>%
        gt() %>%
        tab_header(
          title = "Overall Factor Summary"
        ) %>%
        cols_label(
          Total = "Wt.'d Total Score",
          `Comm.` = "Comm. Factor Score",
          `Rec.` = "Rec. Factor Score",
          `Tribal` = "Tribal Factor Score",
          `Const. Dem.` = "Const. Dem. Factor Score",
          Rebuild = "Rebuild Factor Score",
          `Stock Status` = "Stock Status Factor Score",
          `Fishing Mort.` = "Fishing Mort. Factor Score",
          Ecosystem = "Eco. Factor Score",
          `New Info` = "New Info Factor Score",
          `Assess. Freq.` = "Assess. Freq. Factor Score"
        ) %>%
        fmt_number(columns = -c("Rank"), decimals = 2) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                  locations = cells_body(columns = c("Species", "Rank"))
        ) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                  locations = cells_body(columns = Total)) %>%
        opt_interactive(use_search = TRUE,
                        use_highlight = TRUE,
                        use_page_size_select = TRUE)
      
      # color cells of columns
      overall_table <- overall_table %>%
        data_color(columns = 4:13, method = "numeric",
                   palette = c("Greys"), reverse = TRUE
        )
      
      overall_table
    }
  })
  
  # download overall ranking table
  output$overall_csv <- downloadHandler(
    filename = function() {
      paste("overall_ranking_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(overall_data(), file)
    }
  )
  
  # output$overall_xls <- downloadHandler(
  #   filename = function() {
  #     paste("overall_table_", Sys.Date(), ".xls", sep = "")
  #   },
  #   content = function(file) {
  #     save(overall_data(), file = file)
  #   }
  # )
  
  # overall ranking plot
  output$overall_ranking <- renderPlotly({
    req(overall_data())
    
    if(sum_weights() == 1.00) {
      # reshape dataframe
      for_plot <- overall_data() %>%
        pivot_longer(
          cols = `Comm.`:`Assess. Freq.`,
          names_to = "factor",
          values_to = "score"
        )
      
      for_plot$rank_species <- paste0(for_plot$Rank, ". ", for_plot$Species)
      
      if(input$num_col == "10" | input$num_col == "20") {
        top_species <- head(for_plot, as.numeric(input$num_col) * 10)
      } else if(input$num_col == "21-40") {
        top_species <- for_plot[201:400, ]
      } else if(input$num_col == "41-60") {
        top_species <- for_plot[401:600, ]
      } else {
        top_species <- for_plot[601:650, ]
      }
      
      # create plot
      overall_plot <- ggplot(top_species, aes(x = reorder(rank_species, score, sum),
                                              y = score,
                                              fill = factor,
                                              text = paste0("Species: ", Species,
                                                            "\nFactor: ", factor,
                                                            "\nWt'd. Factor Score: ",
                                                            paste0(round(score, digits = 2),
                                                                   " (",
                                                                  round((score / Total) * 100,
                                                                        digits = 1),
                                                                  "%)"),
                                                            "\nTotal Wt'd. Factor Score: ",
                                                            round(Total, digits = 2)))
        ) +
        geom_col() +
        coord_flip() +
        labs(
          title = "Overall Fish Species Ranking",
          y = "Overall Weighted Factor Score", x = "Species",
          fill = "Factors"
        ) +
        theme_light() +
        scale_fill_viridis(discrete = TRUE)
      
      ggplotly(overall_plot, tooltip = "text", dynamicTicks = TRUE)
    }
  })
  

  # commercial revenue table
  output$com_gt_table <- render_gt({
    req(joined_com_df)
    
    # filter data down to species selected
    joined_com_df <- joined_com_df[joined_com_df$managementGroup %in% input$com_species_selector,]
  
    # create commercial revenue gt table output, display in ascending order by rank
    com_table <- joined_com_df %>%
      select("Species", input$com_columns) %>%
      gt() %>%
      tab_header(
        title = "Commercial Importance",
        subtitle = "Measured by total inflation adjusted ex-vessel revenue data ($1,000)
        between 2018-2022 (source: PacFIN)"
      )
    
    for(i in input$com_colors) {
      if(i %in% input$com_columns) {
        if(i == "Rank") {
          com_table <- com_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          com_table <- com_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Rank" %in% input$com_columns) {
      com_table <- com_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2)
    } else {
      com_table <- com_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    com_table %>%
      fmt_currency(columns = contains("Revenue"), decimals = 0) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # commercial importance species ranking plot
  output$com_ranking <- renderPlotly({
    req(joined_com_df)
    
    com_plot <- ggplot(joined_com_df, aes(x = Species, y = Rank,
                                          text = paste0("Species: ", Species,
                                                        "\nRank: ", Rank,
                                                        "\nFactor Score: ",
                                                        round(`Factor Score`, digits = 2),
                                                        "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(com_plot, tooltip = "text")
  })
  
  
  # recreational importance table
  output$rec_gt_table <- render_gt({
    req(joined_rec_df)
    
    # filter data down to species selected
    joined_rec_df <- joined_rec_df[joined_rec_df$managementGroup %in% input$rec_species_selector,]
    
    # create recreational gt table output, display in ascending order by rank
    rec_table <- joined_rec_df %>%
      select("Species", input$rec_columns) %>%
      gt() %>%
      tab_header(
        title = "Recreational Importance",
        subtitle = "Measured by total recreational catch
        between 2018-2022 (source: GEMM)"
      )
    
    for(i in input$rec_colors) {
      if(i %in% input$rec_columns) {
        if(i == "Rank") {
          rec_table <- rec_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          rec_table <- rec_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Rank" %in% input$rec_columns) {
      rec_table <- rec_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2)
    } else {
      rec_table <- rec_table %>%
        fmt_number(columns = everything(), decimals = 2)
    }
    
    rec_table %>%
      fmt_currency(columns = contains("Revenue"), decimals = 0) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # recreational importance species ranking plot
  output$rec_species_ranking <- renderPlotly({
    req(joined_rec_df)
    
    rec_plot <- ggplot(joined_rec_df, aes(x = Species, y = Rank,
                                          text = paste0("Species: ", Species,
                                                        "\nRank: ", Rank,
                                                        "\nFactor Score: ",
                                                        round(`Factor Score`, digits = 2),
                                                        "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(rec_plot, tooltip = "text")
  })
  
  
  # tribal revenue table
  output$tribal_gt_table <- render_gt({
    req(joined_tribal_df)
    
    # filter data down to species selected
    joined_tribal_df <- joined_tribal_df[joined_tribal_df$managementGroup %in% input$tribal_species_selector,]
    
    # create tribal revenue gt table output, display in ascending order by rank
    tribal_table <- joined_tribal_df %>%
      select("Species", input$tribal_columns) %>%
      gt() %>%
      tab_header(
        title = "Tribal Importance",
        subtitle = "Measured by total inflation adjusted ex-vessel revenue data for tribal landings
        between 2018-2022 (source: PacFIN)"
      )
    
    for(i in input$tribal_colors) {
      if(i %in% input$tribal_columns) {
        if(i == "Rank") {
          tribal_table <- tribal_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          tribal_table <- tribal_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Rank" %in% input$tribal_columns) {
      tribal_table <- tribal_table %>%
        fmt_number(columns = -c("Rank"), decimals = 2)
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
  output$tribal_species_ranking <- renderPlotly({
    req(joined_tribal_df)
    
    tribal_plot <- ggplot(joined_tribal_df, aes(x = Species, y = Rank,
                                                text = paste0("Species: ", Species,
                                                              "\nRank: ", Rank,
                                                              "\nFactor Score: ",
                                                              round(`Factor Score`, digits = 2),
                                                              "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(tribal_plot, tooltip = "text")
  })
  
  
  # constituent demand table
  ## negative scores adjusted
  output$cd_gt_table <- render_gt({
    req(joined_cd_df)
    
    # filter data down to species selected
    joined_cd_df <- joined_cd_df[joined_cd_df$managementGroup %in% input$cd_species_selector,]
    
    # create recreational gt table output, display in ascending order by rank
    cd_table <- joined_cd_df %>%
      select("Species", input$cd_columns) %>%
      gt() %>%
      tab_header(
        title = "Constituent Demand"
      )
    
    for(i in input$cd_colors) {
      if(i %in% input$cd_columns) {
        if(i == "Rank") {
          cd_table <- cd_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else if(i != "Concern") {
          cd_table <- cd_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    cd_table %>%
      fmt_percent(columns = contains("Percent"), decimals = 1,
                  scale_values = TRUE) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # constituent demand species ranking plot
  output$cd_species_ranking <- renderPlotly({
    req(joined_cd_df)
    
    cd_plot <- ggplot(joined_cd_df, aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor Score`, digits = 2),
                                                      "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(cd_plot, tooltip = "text")
  })
  
  
  # rebuilding table
  output$reb_gt_table <- render_gt({
    req(joined_reb_df)
    
    joined_reb_df <- joined_reb_df[joined_reb_df$managementGroup %in% input$reb_species_selector,]

    reb_table <- joined_reb_df %>%
      select("Species", input$reb_columns) %>%
      gt() %>%
      tab_header(
        title = "Rebuilding"
      )
    
    for(i in input$reb_colors) {
      if(i %in% input$reb_columns) {
        if(i == "Rebuilding Target Year") {
          reb_table <- reb_table %>%
            data_color(columns = `Rebuilding Target Year`, method = "auto", palette = "viridis",
                       reverse = TRUE)
        } else {
          reb_table <- reb_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    reb_table %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # rebuilding species ranking plot - uses rebuilding score
  output$reb_species_ranking <- renderPlotly({
    req(joined_reb_df)
    
    reb_plot <- ggplot(joined_reb_df, aes(x = Species, y = `Factor Score`,
                                          text = paste0("Species: ", Species,
                                                        "\nFactor Score: ",
                                                        round(`Factor Score`, digits = 2),
                                                        "\nManagement Group: ", managementGroup))
      ) +
      geom_segment(aes(x = Species, xend = Species, y = 0, yend = `Factor Score`),
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
    
    ggplotly(reb_plot, tooltip = "text")
  })
  
  
  # stock status table
  output$ss_gt_table <- render_gt({
    req(joined_ss_df)
    
    joined_ss_df <- joined_ss_df[joined_ss_df$managementGroup %in% input$ss_species_selector,]

    ss_table <- joined_ss_df %>%
      select("Species", input$ss_columns) %>%
      gt() %>%
      tab_header(
        title = "Stock Status",
        subtitle = "Measured by the estimated fraction unfished at the time of the most
        recent assessment or the PSA score for un-assessed species"
      )
    
    # reverse color scale for Fraction_Unfished?
    for(i in input$ss_colors) {
      if(i %in% input$ss_columns) {
        if(i == "Rank") {
          ss_table <- ss_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          ss_table <- ss_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Fraction Unfished" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        fmt_percent(columns = `Fraction Unfished`, decimals = 1)
    }
    
    if("Target Fraction Unfised" %in% input$ss_columns) {
      ss_table <- ss_table %>%
        fmt_percent(columns = `Target Fraction Unfised`, decimals = 1)
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
  output$ss_species_ranking <- renderPlotly({
    req(joined_ss_df)
    
    ss_plot <- ggplot(joined_ss_df, aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(Score, digits = 2),
                                                      "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(ss_plot, tooltip = "text")
  })
  
  
  # fishing mortality table
  ## footnotes disappear if less than 2
  output$fm_gt_table <- render_gt({
    req(joined_fm_df)
    
    fm_cols <- colnames(joined_fm_df)[colnames(joined_fm_df) != "Species"]
      
    joined_fm_df <- joined_fm_df[joined_fm_df$managementGroup %in% input$fm_species_selector,]
      
    fm_table <- joined_fm_df %>%
      select("Species", input$fm_columns) %>%
      gt() %>%
      tab_header(
        title = "Fishing Mortality"
      )
      
    for(i in input$fm_colors) {
      if(i %in% input$fm_columns) {
        if(i == "Rank") {
          fm_table <- fm_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          fm_table <- fm_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
      
    if("Average OFL Attainment" %in% input$fm_columns) {
      fm_table <- fm_table %>%
        tab_style(style = cell_text(color = "red", weight = "bold"),
                  locations = cells_body(
                    columns = `Average OFL Attainment`,
                    rows = `Average OFL Attainment` > 1.00
                  )
        ) %>%
        tab_footnote(footnote = "Cells highlighted red indicate
                     high OFL attainment percentages.",
                     locations = cells_column_labels(columns = `Average OFL Attainment`))
    }
      
    if("Average OFL" %in% input$fm_columns &
       "managementGroup" %in% input$fm_columns) {
      fm_table <- fm_table %>%
        tab_style(style = cell_text(style = "italic"),
                  locations = cells_body(
                    columns = `Average OFL`,
                    rows = managementGroup != "species specific"
                  )
        ) %>%
        tab_footnote(footnote = "Cells with italic text indicate OFL contributions.",
                     locations = cells_column_labels(columns = `Average OFL`)
        )
    }
    
    fm_table %>%
      fmt_number(columns = contains("Average"), decimals = 2) %>%
      fmt_percent(columns = contains("Attainment"), decimals = 1) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # fishing mortality species ranking plot
  output$fm_species_ranking <- renderPlotly({
    req(joined_fm_df)
    
    fm_plot <- ggplot(joined_fm_df, aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor Score`, digits = 2),
                                                      "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(fm_plot, tooltip = "text")
  })
  
  
  # ecosystem table - factor score has 3 decimals
  output$eco_gt_table <- render_gt({
    req(joined_eco_df)
    
    joined_eco_df <- joined_eco_df[joined_eco_df$managementGroup %in% input$eco_species_selector,]
    
    eco_table <- joined_eco_df %>%
      select("Species", input$eco_columns) %>%
      gt() %>%
      tab_header(
        title = "Ecosystem"
      )
    
    for(i in input$eco_colors) {
      if(i %in% input$eco_columns) {
        if(i == "Rank") {
          eco_table <- eco_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          eco_table <- eco_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Factor Score" %in% input$eco_columns) {
      eco_table <- eco_table %>%
        fmt_number(columns = `Factor Score`, decimals = 2)
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
  output$eco_species_ranking <- renderPlotly({
    req(joined_eco_df)
    
    eco_plot <- ggplot(joined_eco_df, aes(x = Species, y = Rank,
                                          text = paste0("Species: ", Species,
                                                        "\nRank: ", Rank,
                                                        "\nFactor Score: ",
                                                        round(`Factor Score`, digits = 2),
                                                        "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(eco_plot, tooltip = "text")
  })
  
  
  # new information table
  output$ni_gt_table <- render_gt({
    req(joined_ni_df)
    
    joined_ni_df <- joined_ni_df[joined_ni_df$managementGroup %in% input$ni_species_selector,]
    
    ni_table <- joined_ni_df %>%
      select("Species", input$ni_columns) %>%
      gt() %>%
      tab_header(
        title = "New Information"
      )
    
    for(i in input$ni_colors) {
      if(i %in% input$ni_columns) {
        if(i == "Rank") {
          ni_table <- ni_table %>%
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else if(i == "Year Last Assessed") {
          ni_table <- ni_table %>%
            data_color(columns = `Year Last Assessed`, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          ni_table <- ni_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    ni_table %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # new information species ranking plot
  output$ni_species_ranking <- renderPlotly({
    req(joined_ni_df)
    
    ni_plot <- ggplot(joined_ni_df, aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor score`, digits = 2),
                                                      "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(ni_plot, tooltip = "text")
  })
  
  
  # assessment frequency table
  output$af_gt_table <- render_gt({
    req(joined_af_df)
    
    joined_af_df <- joined_af_df[joined_af_df$managementGroup %in% input$af_species_selector,]
    
    af_table <- joined_af_df %>%
      select("Species", input$af_columns) %>%
      gt() %>%
      tab_header(
        title = "Assessment Frequency"
      )
    
    for(i in input$af_colors) {
      if(i %in% input$af_columns) {
        if(i == "Score") {
          af_table <- af_table %>%
            data_color(columns = Score, method = "numeric", palette = "viridis")
        } else if(i == "Last Assessment Year") {
          af_table <- af_table %>%
            data_color(columns = `Last Assessment Year`, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          af_table <- af_table %>%
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
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
  output$af_species_ranking <- renderPlotly({
    req(joined_af_df)
    
    af_plot <- ggplot(joined_af_df, aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(Score, digits = 2),
                                                      "\nManagement Group: ", managementGroup))
      ) +
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
    
    ggplotly(af_plot, tooltip = "text")
  })
  
})
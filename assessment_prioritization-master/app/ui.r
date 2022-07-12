library(shiny)
library(DT)

ui <- basicPage(
  fluidPage(
    titlePanel("Assessment Prioritization for 2023"),
      mainPanel(
        tabsetPanel(
          tabPanel("Factor Summary",
            value=1
          ),
          tabPanel("Commercial Revenue",
            value=2
          ), id="conditionedPanels"
        )
      )
  )
)

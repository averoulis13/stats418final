library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Generate fake data
xnbap23df <- tibble(
  Player = paste0("Player", 1:500),
  Age = sample(18:40, size = 500, replace = TRUE),
  G = sample(0:82, size = 500, replace = TRUE),
  GS = sample(0:82, size = 500, replace = TRUE),
  MP = round(runif(500, min = 0, max = 48), digits = 1),
  FG = round(runif(500, min = 0, max = 15), digits = 1),
  P3A = round(runif(500, min = 0, max = 20), digits = 1)
)

#START DASH TAB HERE - Ignore fake data above, this was for quick testing. renamed so will not affect script.


#This is for a bar chart to show the top 15 players in each field. 

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("NBA Player Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select a variable:",
                  choices = c("Age", "G", "GS", "MP", "FG", "3PA"))
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)


server <- function(input, output) {
  
  # subset
  nbap23df_react <- reactive({
    nbap23df %>%
      select(Player, !!sym(input$var)) %>%
      arrange(desc(!!sym(input$var))) %>%
      head(15)
  })
  
  #bar chart
  output$barplot <- renderPlot({
    ggplot(nbap23df_react(), aes(x = Player, y = !!sym(input$var))) +
      geom_col() +
      labs(x = "", y = input$var, title = paste0("Top 15 Players by ", input$var)) +
      coord_flip() +
      geom_text(aes(label = !!sym(input$var)), hjust = -0.2, size = 3)
  })
}


shinyApp(ui = ui, server = server)


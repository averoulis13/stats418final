#working two tabs for top15


# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("NBA Stats"),
  
  # Define the sidebar layout with two tabs
  sidebarLayout(
    sidebarPanel(
      # First tab - Player stats
      tabsetPanel(
        tabPanel("Player Stats",
                 selectInput("var1", "Select a variable:",
                             choices = c("Age", "G", "GS", "MP", "FG", "3PA"))
        ),
        # Second tab - Team stats
        tabPanel("Team Stats",
                 selectInput("var2", "Select a variable:",
                             choices = c("FG%", "TRB", "AST", "STL", "BLK"))
        )
      )
    ),
    
    mainPanel(
      # First tab - Player stats
      tabsetPanel(
        tabPanel("Player Stats",
                 plotOutput("barplot1")
        ),
        # Second tab - Team stats
        tabPanel("Team Stats",
                 plotOutput("barplot2")
        )
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # First tab - Player stats
  nbap23df_react <- reactive({
    nbap23df %>%
      select(Player, !!sym(input$var1)) %>%
      arrange(desc(!!sym(input$var1))) %>%
      head(15)
  })
  
  output$barplot1 <- renderPlot({
    ggplot(nbap23df_react(), aes(x = Player, y = !!sym(input$var1))) +
      geom_col() +
      labs(x = "", y = input$var1, title = paste0("Top 15 Players by ", input$var1)) +
      coord_flip() +
      geom_text(aes(label = !!sym(input$var1)), hjust = -0.2, size = 3)
  })
  
  # Second tab - Team stats
  oppjoin23_react <- reactive({
    oppjoin23 %>%
      select(Team, !!sym(input$var2)) %>%
      arrange(desc(!!sym(input$var2))) %>%
      head(15)
  })
  
  output$barplot2 <- renderPlot({
    ggplot(oppjoin23_react(), aes(x = Team, y = !!sym(input$var2))) +
      geom_col() +
      labs(x = "", y = input$var2, title = paste0("Top 15 Teams by ", input$var2)) +
      coord_flip() +
      geom_text(aes(label = !!sym(input$var2)), hjust = -0.2, size = 3)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
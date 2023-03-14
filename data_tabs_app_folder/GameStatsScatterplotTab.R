library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Player Game Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Variable 1:",
                  choices = c("GS", "MP", "FG", "3PA", "FGA", "FT", "FTA", "PTS", "TOV", "AST", "TRB", "Date")),
      selectInput("var2", "Variable 2:",
                  choices = c("GS", "MP", "FG", "3PA", "FGA", "FT", "FTA", "PTS", "TOV", "AST", "TRB", "Date"))
    ),
    
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

server <- function(input, output) {
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    ggplot(pascal_df, aes_string(x = input$var1, y = input$var2)) +
      geom_point() +
      labs(x = input$var1, y = input$var2, title = paste0(input$var1, " vs. ", input$var2))
  })
  
}

shinyApp(ui, server)

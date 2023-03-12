#Initial Scrape of Lakers data to test Rshiny Dashboard tabs

library(RCurl)
library(XML)

bball <- "https://www.basketball-reference.com/teams/LAL/"

bburl <- getURL(bball)

# read in HTML data
tbls_bball <- readHTMLTable(bburl)

typeof(tbls_bball)
## [1] "list"

length(tbls_bball)

head(tbls_bball[[1]])


bball_tbl <- data.frame(tbls_bball[[1]])

bball_tbl$Wins <- as.numeric(bball_tbl$W)

bball_tbl$Losses <- as.numeric(bball_tbl$L)

#More data cleaning done in terminal but this should be all thats necessary.

##RShiny implementation

library(shiny)
library(ggplot2)
library(dplyr)

#start UI

#dash testing

bball_tbl <- bball_tbl %>% 
  mutate(id = paste(bball_tbl$Team, bball_tbl$Season, sep = "_"))

ui <- fluidPage(
  
  titlePanel("NBA Statistics Dashboard"),
  
  # tabs
  tabsetPanel(
    # Scatterplot/Tab 1
    tabPanel("Scatterplot",
             # Dropdown menu for selecting variables
             selectInput(inputId = "x_var", label = "X-axis variable", 
                         choices = c("points", "rebounds", "assists")),
             selectInput(inputId = "y_var", label = "Y-axis variable", 
                         choices = c("points", "rebounds", "assists")),
             # Dropdown menu for selecting team
             selectInput(inputId = "team", label = "Team", 
                         choices = unique(bball_tbl$team)),
             # Output scatterplot
             plotOutput(outputId = "scatterplot")
    ),
    # Histogram/Tab 2
    tabPanel("Histogram",
             # Dropdown menu for selecting variable
             selectInput(inputId = "hist_var", label = "Histogram variable", 
                         choices = c("points", "rebounds", "assists")),
             # Dropdown menu for selecting team
             selectInput(inputId = "hist_team", label = "Team", 
                         choices = unique(bball_tbl$team)),
             # Output histogram
             plotOutput(outputId = "histogram")
    ),
    # Summary Tiles/Tab 3
    tabPanel("Summary",
             fluidRow(
               column(4, valueBoxOutput("wins_box")),
               column(4, valueBoxOutput("losses_box")),
               column(4, valueBoxOutput("playoffs_box"))
             )
    )
  )
)

# send to server
server <- function(input, output) {
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data = filter(bball_tbl, team = input$team),
           aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      labs(x = input$x_var, y = input$y_var)
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(data = filter(bball_tbl, team == input$hist_team),
           aes_string(x = input$hist_var)) +
      geom_histogram() +
      labs(x = input$hist_var, y = "Frequency")
  })
  
  # Summary tiles
  output$wins_box <- renderValueBox({
    valueBox(paste("Total wins:", nrow(filter(bball_tbl, result == "W"))),
             icon = icon("thumbs-up"), color = "green")
  })
  output$losses_box <- renderValueBox({
    valueBox(paste("Total losses:", nrow(filter(bball_tbl, result == "L"))),
             icon = icon("thumbs-down"), color = "red")
  })
  output$playoffs_box <- renderValueBox({
    valueBox(paste("Total playoffs:", nrow(filter(bball_tbl, playoffs == "Y"))),
             icon = icon("trophy"), color = "blue")
  })
}

# open dash
shinyApp(ui = ui, server = server)
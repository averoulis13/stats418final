# App with 2 tabs: Game Logs for each Player and Overall Team Data

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RCurl)
library(XML)
library(DT)
library(ggvis)

# have to run this line if variables are not stored in environment
#source("Stats418_final_modeling_ex.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(strong("NBA Prop Predictor")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("pname", h3("Player Name"), 
                  choices = plinks$Player, selected = plinks$Player[1]),
      br(),
      selectInput("tname",h3("Team Name"),
                  choices = oppjoin23$Team, selected = oppjoin23$Team[1]),
      br(),
      radioButtons("stat1", "Stat Type:",
                   c("Points" = "PTS",
                     "Rebounds" = "TRB.x",
                     "Assists" = "AST.x")),
      
      br(),
      strong("Player Table tab represents the selected player's game logs for the whole season"),
      br(),
      br(),
      strong("Team Table tab represents all teams' aggregate statistics for the whole season"),
      br(),
      br(),
      img(src="nba-logo.jpg",height=100,width=150)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Player Table", tableOutput("ptable")),
                  tabPanel("Team Table",tableOutput("ttable"))
      )
      
    )
    
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$ptable <- renderTable({
    gamestore[[input$pname]] %>% select(G.x:`+/-`) %>% arrange(desc(as.numeric(G.x)))
  })
  
  output$ttable <- renderTable({
    oppjoin23 %>% select(-Opp)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
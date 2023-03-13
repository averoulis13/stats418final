# Points Histogram App


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RCurl)
library(XML)
library(DT)
library(ggvis)

############ have to run this line if variables are not stored in environment ###################
# source("Stats418_final_modeling_ex.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(strong("NBA Prop Predictor")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      strong("This is where the different tabs would be listed"),
      br(),
      br(),
      img(src="nba-logo.jpg",height=100,width=200),
      br(),
      p("NBA Picture")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      column(3,
             selectInput("pname", h3("Player Name"), 
                         choices = plinks$Player, selected = plinks$Player[1])),
      
      column(3,
             selectInput("tname",h3("Team Name"),
                         choices = oppjoin23$Team, selected = oppjoin23$Team[1])),
      column(3,
             selectInput("sname", h3("Statistic"), 
                         choices = c("Points","Rebounds","Assists"), selected = "Points")),
      plotOutput("ptplot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$ptplot <- renderPlot({
    ggplot(data=gamestore[[input$pname]],aes(x=as.numeric(PTS))) + geom_histogram(fill="red",bins=60) + 
      labs(title="Frequencies of Points Scored",x="Points", y = "Frequency") +
      theme(
        panel.background = element_rect(fill = '#0E1A24', color = 'white'),
        panel.grid.major = element_line(color = '#E5E4E2', linewidth = 0.03 ),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#0C1623"),
        plot.margin = margin(12,12,12,12),
        axis.text = element_text(colour = "white", face="bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(colour = "red",face="bold",size=15),
        axis.title.y = element_text(colour = "red",face="bold",size=15),
        plot.title = element_text(color = "red",face="bold"),
        legend.position = "none")
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
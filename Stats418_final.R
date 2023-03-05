library(dplyr)
library(tidyverse)
library(stringr)
library(curl)
library(rvest)
library(lubridate)

setwd("/Users/alejandroveroulis13/Desktop/Stats\ 418/Finalproject")


## STEP 1: DATA COLLECTION AND CLEANING

### Website NBA season data 2022-23
nba23 <- read_html("https://www.basketball-reference.com/leagues/NBA_2023.html")

# scraping all data tables from webpage
nba23tables <- html_table(nba23)

# TABLE 1: forming data frame for team opponent's per 100 possession metrics
nbaopp23 <- nba23tables[[10]]
nbaopp23 <- nbaopp23 %>% select(Team,`FG%`,`3P%`,`2P%`,TRB,AST,STL,BLK,TOV,PF)

# TABLE 2: forming data frame for team opponent's advanced metrics
oppadv23 <- nba23tables[[11]]
oppadv23 <- oppadv23[,colSums(is.na(oppadv23))==0] # taking out NA columns
oppadvnames <- as.character(oppadv23[1,]) # getting column names
oppadvnames[18:21] <- c("oeFG%","oTOV%","oORB%","oFT/FGA")
names(oppadv23) <- oppadvnames # assigning column names
oppadv23 <- oppadv23[-1,] %>% select(Team,ORtg,DRtg,Pace,`oeFG%`:`FT/FGA`)


# TABLE 3: OVERALL TEAM TABLE, Joining columns to get one team dataset
oppjoin23 <- inner_join(nbaopp23,oppadv23,by='Team') %>% arrange(Team)


#######################################################################


## Getting player averages dataset 2023 to find desired players
nbapurl23 <- read_html("https://www.basketball-reference.com/leagues/NBA_2023_per_game.html")

# scraping all data tables from webpage
nbap23table <- html_table(nbapurl23)

# TABLE 4: forming data frame for player averages
nbap23df <- nbap23table[[1]]
nbap23df <- nbap23df %>% filter(Pos != "Pos") %>% select(-Rk)
nbap23df$MP <- as.numeric(nbap23df$MP) # making minutes played numeric
nbap23df$GS <- as.numeric(nbap23df$GS) # making games started numeric
# removing duplicate rows of players on multiple teams, only keeping total stats
nbap23df <- nbap23df[!duplicated(nbap23df[,c("Player")]), ] 
nbap23df <- nbap23df %>% filter(MP > 18,GS > 41) %>% arrange(-MP) # getting desired "qualifying" players

# handling special characters
for(player in 1:nrow(nbap23df)){
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"ć","c")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"č","c")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"č","c")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"ņ","n")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"ģ","g")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"ö","o")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"Ş","S")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"P.J.","PJ")
  nbap23df$Player[player] <- str_replace(nbap23df$Player[player],"ū","u")
}

# list of players with more than 41 GS, more than 18 MPG (will only include players present as of 2019)
plist23 <- nbap23df$Player 

nba_db <- read.csv("NBA_Player_IDs.csv") # database of all player ids
names(nba_db)[5] <- "Player"

# list of players and links (89 qualifying players)
plinks <- inner_join(nbap23df,nba_db,by="Player") %>% select(Player,BBRefLink) %>% drop_na()
plinks$gamelogs <- str_replace(plinks$BBRefLink,".html","/gamelog/2023")

# create the empty list for data storage
game_logs23 <- list()

# TABLE 5: GETTING A SINGLE PLAYER'S GAME LOGS
pascal_page23 <- curl::curl(plinks$gamelogs[1]) %>% read_html()
pascal <- html_table(pascal_page23)
pascal_df <- pascal[[8]][,-c(6,8)] %>% filter(GS != "Inactive") %>% filter(GS != "GS")

# create the empty list for data storage
game_logs23 <- list()

# Series of loops over all qualifying players and storing each set of game logs in a list
for(player in 1:25){
  Sys.sleep(runif(1,4,5))
  p_link <- read_html(plinks$gamelogs[player])
  p_table <- html_table(p_link)
  game_logs23[[plinks$Player[player]]] <- p_table[[8]][,-c(6,8)] %>% filter(GS != "Inactive") %>% filter(GS != "GS") %>% 
    filter(GS != "Did Not Dress") %>% filter(GS != "Did Not Play")
}

for(player in 26:50){
  Sys.sleep(runif(1,4,5))
  p_link <- read_html(plinks$gamelogs[player])
  p_table <- html_table(p_link)
  game_logs23[[plinks$Player[player]]] <- p_table[[8]][,-c(6,8)] %>% filter(GS != "Inactive") %>% filter(GS != "GS") %>% 
    filter(GS != "Did Not Dress") %>% filter(GS != "Did Not Play")
}


for(player in 51:89){
  Sys.sleep(runif(1,4,5))
  p_link <- read_html(plinks$gamelogs[player])
  p_table <- html_table(p_link)
  game_logs23[[plinks$Player[player]]] <- p_table[[8]][,-c(6,8)] %>% filter(GS != "Inactive") %>% filter(GS != "GS") %>% 
    filter(GS != "Did Not Dress") %>% filter(GS != "Did Not Play")
}

# the first few datasets
head(game_logs23)


################# Joining team statistics to each game log

# reordering team abbreviations
tmabbs <- sort(unique(nbap23df$Tm))
tmabbs[4:5] <- c("CHO","CHI") # changing order of Hornets and Bulls
tmabbs <- tmabbs[-29] # taking out team total abbreviation "TOT"

oppjoin23$Opp <- tmabbs  # adding a team abbreviation column to be able to join with game logs


# performing the joins between the player game logs and the opponent team stats
for(df in 1:length(game_logs23))(
  game_logs23[[df]] <- inner_join(game_logs23[[df]],oppjoin23,by="Opp")
)



## STEP 2: MODELING IDEAS

################## Building an expected points model

# just using one player's to test how model would be implemented
siak_train <- siak[1:round(nrow(siak)*0.75),]

siak_test <- siak[round(1+nrow(siak)*0.75):nrow(siak),]

# Variables from player: FG, FT, ORB, TOV.x, PF.x, 
# Variables from opposing team: FG%.y, TOV%,PF.y,DRtg,Pace,DRB%
# have to convert to numeric since data came in character form
m1 <- lm(as.numeric(PTS) ~ as.numeric(FG) + as.numeric(FT) + as.numeric(ORB) + as.numeric(TOV.x) + 
           as.numeric(PF.x) + as.numeric(`FG%.y`) + as.numeric(`TOV%`) + as.numeric(`PF.y`) + 
           as.numeric(DRtg) + as.numeric(Pace) + as.numeric(`DRB%`),data=siak_train)

summary(m1)

# Then try on the test set, find RMSE
rmse1 <- sqrt(sum((predict(m1,siak_test) - as.numeric(siak_test$PTS))^2)/nrow(siak_test))
rmse1

# Then run a loop to find all the RMSEs across all player sets


################### Do something similar for rebounds and assists



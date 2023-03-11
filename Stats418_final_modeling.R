library(dplyr)
library(tidyverse)
library(stringr)
library(curl)
library(rvest)
library(lubridate)
library(leaps)

setwd("/Users/alejandroveroulis13/Desktop/Stats\ 418/Finalproject")


## STEP 1: DATA COLLECTION AND CLEANING

### Website NBA season data 2022-23
nba23 <- read_html("https://www.basketball-reference.com/leagues/NBA_2023.html")

# scraping all data tables from webpage
nba23tables <- html_table(nba23)

# TABLE 1: forming data frame for team opponent's per 100 possession metrics
nbaopp23 <- nba23tables[[10]]
nbaopp23 <- nbaopp23 %>% select(Team,G,`FG%`,`3P%`,`2P%`,TRB,AST,STL,BLK,TOV,PF)

# TABLE 2: forming data frame for team opponent's advanced metrics
oppadv23 <- nba23tables[[11]]
oppadv23 <- oppadv23[,colSums(is.na(oppadv23))==0] # taking out NA columns
oppadvnames <- as.character(oppadv23[1,]) # getting column names
oppadvnames[18:21] <- c("oeFG%","oTOV%","oORB%","oFT/FGA")
names(oppadv23) <- oppadvnames # assigning column names
oppadv23 <- oppadv23[-1,] %>% select(Team,ORtg,DRtg,Pace,`oeFG%`:`FT/FGA`)


# TABLE 3: OVERALL TEAM TABLE, Joining columns to get one team dataset
oppjoin23 <- inner_join(nbaopp23,oppadv23,by='Team') %>% arrange(Team)

str(oppjoin23)
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
nbap23df <- nbap23df %>% filter(GS >= round(0.7*min(oppjoin23$G)) ) %>% arrange(-MP) # getting desired "qualifying" players

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

# list of players with at 75% of team games started (will only include players present as of 2019)
plist23 <- nbap23df$Player

nba_db <- read.csv("NBA_Player_IDs.csv") # database of all player ids
names(nba_db)[5] <- "Player"

# list of players and links
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
# This part takes a few mins to run, 71 was the max amount of players before it stopped the process
for(player in 1:71){
  Sys.sleep(runif(1,4,5))
  p_link <- read_html(plinks$gamelogs[player])
  p_table <- html_table(p_link)
  game_logs23[[plinks$Player[player]]] <- p_table[[8]][,-c(6,8)] %>% filter(GS %in% c(0,1))
}

# second loop for the remaining players
for(player in 72:nrow(plinks)){
  Sys.sleep(runif(1,4,5))
  p_link <- read_html(plinks$gamelogs[player])
  p_table <- html_table(p_link)
  game_logs23[[plinks$Player[player]]] <- p_table[[8]][,-c(6,8)] %>% filter(GS %in% c(0,1))
}


# the first few data sets
head(game_logs23)


################# Joining team statistics to each game log

# reordering team abbreviations
tmabbs <- sort(unique(nbap23df$Tm))
tmabbs[4:5] <- c("CHO","CHI") # changing order of Hornets and Bulls
tmabbs <- tmabbs[-29] # taking out team total abbreviation "TOT"

oppjoin23$Opp <- tmabbs  # adding a team abbreviation column to be able to join with game logs


# performing the joins between the player game logs and the opponent team stats
for(df in 1:length(game_logs23)){
  game_logs23[[df]] <- inner_join(game_logs23[[df]],oppjoin23,by="Opp")
}





## STEP 2: MODELING IDEAS

# You can run this to store the game logs elsewhere while experimenting with data
#gamestore <- game_logs23  

# This resets the game logs to their initial scraped state
game_logs23 <- gamestore  

# add cumulative means for player statistics
for(df in 1:length(game_logs23)){
  
  df_curr <- game_logs23[[df]]
  df_curr$MP <- round(period_to_seconds(ms(df_curr$MP))/60)
  
  # for each variable, assigning the last n-1 observations the first n-1 cumulative means of the given stat,
  # where n is the number of games played in each player's game logs
  df_curr$MP[-1] <- cummean(as.numeric(df_curr$MP)[-nrow(df_curr)])
  df_curr$FG[-1] <- cummean(as.numeric(df_curr$FG)[-nrow(df_curr)])
  df_curr$FGA[-1] <- cummean(as.numeric(df_curr$FGA)[-nrow(df_curr)])
  df_curr$`3P`[-1] <- cummean(as.numeric(df_curr$`3P`)[-nrow(df_curr)])
  df_curr$`3PA`[-1] <- cummean(as.numeric(df_curr$`3PA`)[-nrow(df_curr)])
  df_curr$FT[-1] <- cummean(as.numeric(df_curr$FT)[-nrow(df_curr)])
  df_curr$FTA[-1] <- cummean(as.numeric(df_curr$FTA)[-nrow(df_curr)])
  df_curr$ORB[-1] <- cummean(as.numeric(df_curr$ORB)[-nrow(df_curr)])
  df_curr$DRB[-1] <- cummean(as.numeric(df_curr$DRB)[-nrow(df_curr)])
  df_curr$STL.x[-1] <- cummean(as.numeric(df_curr$STL.x)[-nrow(df_curr)])
  df_curr$BLK.x[-1] <- cummean(as.numeric(df_curr$BLK.x)[-nrow(df_curr)])
  df_curr$TOV.x[-1] <- cummean(as.numeric(df_curr$TOV.x)[-nrow(df_curr)])
  df_curr$PF.x[-1] <- cummean(as.numeric(df_curr$PF.x)[-nrow(df_curr)])
  df_curr$GmSc[-1] <- cummean(as.numeric(df_curr$GmSc)[-nrow(df_curr)])
  df_curr$`+/-`[-1] <- cummean(as.numeric(df_curr$`+/-`)[-nrow(df_curr)])
  
  # making PTS numeric and creating a separate column for cumulative average of points
  df_curr$PTS <- as.numeric(df_curr$PTS)
  df_curr$Ptavg <- df_curr$PTS
  df_curr$Ptavg[-1] <- cummean(as.numeric(df_curr$Ptavg)[-nrow(df_curr)])
  
  # making TRB.x numeric and creating a separate column for cumulative average of total rebounds
  df_curr$TRB.x <- as.numeric(df_curr$TRB.x)
  df_curr$TRBavg <- df_curr$TRB.x
  df_curr$TRBavg[-1] <- cummean(as.numeric(df_curr$TRBavg)[-nrow(df_curr)])
  
  # making TRB.x numeric and creating a separate column for cumulative average of total rebounds
  df_curr$AST.x <- as.numeric(df_curr$AST.x)
  df_curr$ASTavg <- df_curr$AST.x
  df_curr$ASTavg[-1] <- cummean(as.numeric(df_curr$ASTavg)[-nrow(df_curr)])
  
  # making other variables numeric
  df_curr$FG <- as.numeric(df_curr$FG)
  df_curr$FGA <- as.numeric(df_curr$FGA)
  df_curr$`3P` <- as.numeric(df_curr$`3P`)
  df_curr$`3PA` <- as.numeric(df_curr$`3PA`)
  df_curr$FT <- as.numeric(df_curr$FT)
  df_curr$FTA <- as.numeric(df_curr$FTA)
  df_curr$ORB <- as.numeric(df_curr$ORB)
  df_curr$TOV.x <- as.numeric(df_curr$TOV.x)
  df_curr$PF.x <- as.numeric(df_curr$PF.x)
  df_curr$GmSc <- as.numeric(df_curr$GmSc)
  df_curr$`+/-` <- as.numeric(df_curr$`+/-`)
  df_curr$DRtg <- as.numeric(df_curr$DRtg)
  df_curr$Pace <- as.numeric(df_curr$Pace)
  df_curr$`DRB%` <- as.numeric(df_curr$`DRB%`)
  df_curr$STL.x <- as.numeric(df_curr$STL.x)
  df_curr$BLK.x <- as.numeric(df_curr$BLK.x)
  df_curr$DRB <- as.numeric(df_curr$DRB)
  df_curr$ORtg <- as.numeric(df_curr$ORtg)
  df_curr$`oeFG%` <- as.numeric(df_curr$`oeFG%`)
  df_curr$`oTOV%` <- as.numeric(df_curr$`oTOV%`)
  df_curr$`oFT/FGA` <- as.numeric(df_curr$`oFT/FGA`)
  df_curr$`eFG%` <- as.numeric(df_curr$`eFG%`)
  df_curr$`TOV%` <- as.numeric(df_curr$`TOV%`)
  df_curr$`FT/FGA` <- as.numeric(df_curr$`FT/FGA`)
  df_curr$`oORB%` <- as.numeric(df_curr$`oORB%`)
  
  # normalizing numerical predictors we want
  df_curr <- df_curr %>%  mutate_at(c(names(df_curr)[c(8:10,12:13,15:16,18:19,22:25,27:28,31:53)]), ~(scale(.) %>% as.vector))

  # making NA values 0
  df_curr[is.na(df_curr)] <- 0
  
  game_logs23[[df]] <- df_curr
}

##################################################################################################################
########### BUILDING AN EXPECTED POINTS MODEL
########### TRYING A MODEL FOR 1 DATASET TO SEE HOW IT WORKS

siak <- game_logs23[[1]]
str(siak)

# seeing how numeric variables correlate to points
siaknums <- siak %>% select(-Rk,-G.x,-Date,-Age,-Tm,-Opp,-GS,-`FG%.x`,-`3P%.x`,-`FT%`,-Team,-AST.x,-TRB.x,-G.y)
cors <- cor(siaknums)[,16] # the column that corresponds to correlations with PTS (the response variable)
sort(abs(cors)) # top magnitude correlations with pts


# just using one player's to test how model would be implemented; omitting 1st 10 rows for burn-in
siak_train <- siak[10:round(nrow(siak)*0.75),]
siak_test <- siak[round(1+nrow(siak)*0.75):nrow(siak),]

# Variables from player: MP, 3PA, Ptavg, STL.x
# Variables from opposing team: DRtg, eFG%, oeFG%, BLK.y, 2P%, `FT/FGA`
# Any variables with .x are from the player, variables with .y are from the opposing team

# taking out 3P, FT stats for *players* since some do not have any 3P stats, also taking out 3PA, FGA, TOV%, ORB since
# they cause linear dependencies
m1 <- regsubsets(PTS ~ BLK.x + MP + Ptavg + STL.x + DRtg + `eFG%` + `oeFG%` + 
                   BLK.y + `2P%` + `FT/FGA` + `FG%.y` + GmSc + `oORB%` + 
                   `oFT/FGA` + PF.x + DRB + FG + AST.y + `3P%.y` + TRB.y + `DRB%` + 
                   ORtg+ STL.y + TOV.x + `+/-` + Pace + `oTOV%` + TOV.y +
                   PF.y + ASTavg + TRBavg,nvmax=6,data=siak_train)

summary(m1)

m2 <- lm(PTS ~ MP + Ptavg + DRtg + `eFG%`,data=siak_train)
summary(m2)

# Then try on the test set, find RMSE (anything 8 or below seems to be a good result, scoring is very volatile)
rmse1 <- sqrt(sum((predict(m2,siak_test) - as.numeric(siak_test$PTS))^2)/nrow(siak_test))
rmse1



################## FINDING RMSEs ACROSS ALL PLAYER SETS FOR POINT PREDICTIONS
################## Then run a loop to do so

rmse_vec <- numeric(length(game_logs23)) # storage vector for RMSE's of test sets
rmse_step6 <- numeric(length(game_logs23))
rmse_step5 <- numeric(length(game_logs23))
rmse_step4 <- numeric(length(game_logs23))

for(df in 1:length(game_logs23)){
  df_curr <- game_logs23[[df]]
  
  # initializing training and test sets
  df_train <- df_curr[10:round(nrow(df_curr)*0.75),]
  df_test <- df_curr[round(1+nrow(df_curr)*0.75):nrow(df_curr),]
  
  mod1 <- lm(PTS ~ MP + Ptavg + DRtg + `eFG%`, data=df_train) # model using stepwise / common knowledge
  mod_step6 <- lm(PTS ~ BLK.x + STL.x + `2P%` + GmSc + PF.x + `+/-`,data=df_train) # model using just stepwise 6 vars
  mod_step5 <- lm(PTS ~ BLK.x + Ptavg + PF.x + AST.y + `+/-`,data=df_train) # model using just stepwise 5 vars
  mod_step4 <- lm(PTS ~ BLK.x + Ptavg + PF.x + `+/-`,data=df_train) # model using just stepwise 4 vars
  
  # Then try on the test set, find RMSE
  rmse1 <- sqrt(sum((predict(mod1,df_test) - as.numeric(df_test$PTS))^2)/nrow(df_test))
  rmse_vec[df] <- rmse1
  
  rmse6 <- sqrt(sum((predict(mod_step6,df_test) - as.numeric(df_test$PTS))^2)/nrow(df_test))
  rmse_step6[df] <- rmse6
  
  rmse5 <- sqrt(sum((predict(mod_step5,df_test) - as.numeric(df_test$PTS))^2)/nrow(df_test))
  rmse_step5[df] <- rmse5
  
  rmse4 <- sqrt(sum((predict(mod_step4,df_test) - as.numeric(df_test$PTS))^2)/nrow(df_test))
  rmse_step4[df] <- rmse4
  
}

# simpler models seem to perform better for point prediction
# using median of RMSE's to determine best models for points only
summary(rmse_vec) # the best performing model on the test data; model using stepwise / common knowledge
summary(rmse_step6) # fourth best; model using just stepwise 6 vars
summary(rmse_step5) # third best; model using just stepwise 5 vars
summary(rmse_step4) # second best; model using just stepwise 4 vars



##################################################################################################################
###### CREATING A MODEL FOR REBOUNDS, DOING SOMETHING SIMILAR TO THE POINTS MODEL
########### TRYING A MODEL FOR 1 DATASET TO SEE HOW IT WORKS

siakr <- game_logs23[[1]]
str(siakr)

# seeing how numeric variables correlate to rebounds
siakrnums <- siakr %>% select(-Rk,-G.x,-Date,-Age,-Tm,-Opp,-GS,-`FG%.x`,-`3P%.x`,-`FT%`,-Team,-PTS,-AST.x,-G.y)
cors <- cor(siakrnums)[,10] # the column that corresponds to correlations with TRB.x (the response variable)
sort(abs(cors)) # top magnitude correlations with rebounds


# just using one player's to test how model would be implemented; omitting 1st 10 rows for burn-in
siakr_train <- siakr[10:round(nrow(siakr)*0.75),]
siakr_test <- siakr[round(1+nrow(siakr)*0.75):nrow(siakr),]


# Any variables with .x are from the player, variables with .y are from the opposing team

# taking out 3P, FT stats for *players* since some do not have any 3P stats, also taking out 3PA, FGA, TOV%, ORB since
# they cause linear dependencies
m1 <- regsubsets(TRB.x ~ BLK.x + MP + Ptavg + STL.x + DRtg + `eFG%` + `oeFG%` + 
                   BLK.y + `2P%` + `FT/FGA` + `FG%.y` + GmSc + `oORB%` + 
                   `oFT/FGA` + PF.x + DRB + FG + AST.y + `3P%.y` + TRB.y + `DRB%` + 
                   ORtg + STL.y + TOV.x + `+/-` + Pace + `oTOV%` + TOV.y +
                   PF.y + ASTavg + TRBavg,nvmax=6,data=siakr_train)

summary(m1)

m2 <- lm(TRB.x ~ DRB + BLK.x + `DRB%` + ORtg + `+/-`,data=siakr_train)
summary(m2)

# Then try on the test set, find RMSE (anything 3 or below seems to be a good result, rebounds easier to predict)
rmse1 <- sqrt(sum((predict(m2,siakr_test) - as.numeric(siakr_test$TRB.x))^2)/nrow(siakr_test))
rmse1



################## FINDING RMSEs ACROSS ALL PLAYER SETS FOR REBOUND PREDICTIONS
################## Then run a loop to do so

rmse_vec <- numeric(length(game_logs23)) # storage vector for RMSE's of test sets
rmse_step6 <- numeric(length(game_logs23))
rmse_step5 <- numeric(length(game_logs23))
rmse_step4 <- numeric(length(game_logs23))

for(df in 1:length(game_logs23)){
  df_curr <- game_logs23[[df]]
  
  # initializing training and test sets
  df_train <- df_curr[10:round(nrow(df_curr)*0.75),]
  df_test <- df_curr[round(1+nrow(df_curr)*0.75):nrow(df_curr),]
  
  mod1 <- lm(TRB.x ~ DRB + BLK.x + `DRB%` + ORtg + `+/-`, data=df_train) # model using stepwise / common knowledge
  mod_step6 <- lm(TRB.x ~ `eFG%` + `oeFG%` + BLK.y + `oFT/FGA` + `DRB%` + `+/-`,data=df_train) # model using just stepwise 6 vars
  mod_step5 <- lm(TRB.x ~ `eFG%` + `oeFG%` + BLK.y + `oFT/FGA` + `DRB%`,data=df_train) # model using just stepwise 5 vars
  mod_step4 <- lm(TRB.x ~ `eFG%` + `oeFG%` + BLK.y + `oFT/FGA`,data=df_train) # model using just stepwise 4 vars
  
  # Then try on the test set, find RMSE
  rmse1 <- sqrt(sum((predict(mod1,df_test) - as.numeric(df_test$TRB.x))^2)/nrow(df_test))
  rmse_vec[df] <- rmse1
  
  rmse6 <- sqrt(sum((predict(mod_step6,df_test) - as.numeric(df_test$TRB.x))^2)/nrow(df_test))
  rmse_step6[df] <- rmse6
  
  rmse5 <- sqrt(sum((predict(mod_step5,df_test) - as.numeric(df_test$TRB.x))^2)/nrow(df_test))
  rmse_step5[df] <- rmse5
  
  rmse4 <- sqrt(sum((predict(mod_step4,df_test) - as.numeric(df_test$TRB.x))^2)/nrow(df_test))
  rmse_step4[df] <- rmse4
  
}

# simpler models seem to perform better for rebound prediction as well, but models were similar in accuracy
# using median, median, and max value as criteria for model; 

summary(rmse_vec) # the second performing model on the test data; model using stepwise / common knowledge
summary(rmse_step6) # fourth best; model using just stepwise 6 vars
summary(rmse_step5) # third best; model using just stepwise 5 vars
summary(rmse_step4) # the best; model using just stepwise 4 vars




##################################################################################################################
###### CREATING A MODEL FOR ASSISTS, DOING SOMETHING SIMILAR TO THE OTHER MODELS
########### TRYING A MODEL FOR 1 DATASET TO SEE HOW IT WORKS

siaka <- game_logs23[[1]]
str(siaka)

# seeing how numeric variables correlate to assists
siakanums <- siaka %>% select(-Rk,-G.x,-G.y,-Date,-Age,-Tm,-Opp,-GS,-`FG%.x`,-`3P%.x`,-`FT%`,-Team,-PTS,-TRB.x)
cors <- cor(siakanums)[,10] # the column that corresponds to correlations with AST.x (the response variable)
sort(abs(cors)) # top magnitude correlations with assists


# just using one player's to test how model would be implemented; omitting 1st 10 rows for burn-in
siaka_train <- siaka[10:round(nrow(siaka)*0.75),]
siaka_test <- siaka[round(1+nrow(siaka)*0.75):nrow(siaka),]


# Any variables with .x are from the player, variables with .y are from the opposing team

# taking out 3P, FT stats for *players* since some do not have any 3P stats, also taking out 3PA, FGA, TOV%, ORB since
# they cause linear dependencies
m1 <- regsubsets(AST.x ~ BLK.x + MP + Ptavg + STL.x + DRtg + `eFG%` + `oeFG%` + 
                   BLK.y + `2P%` + `FT/FGA` + `FG%.y` + GmSc + `oORB%` + 
                   `oFT/FGA` + PF.x + DRB + FG + AST.y + `3P%.y` + TRB.y + `DRB%` + 
                   ORtg + STL.y + TOV.x + `+/-` + Pace + `oTOV%` + TOV.y +
                   PF.y + TRBavg,nvmax=6,data=siaka_train)

summary(m1)

m2 <- lm(AST.x ~ ASTavg + MP + DRtg + AST.y,data=siaka_train)
summary(m2)

# Then try on the test set, find RMSE (anything 2 or below seems to be a good result, rebounds easier to predict)
rmse1 <- sqrt(sum((predict(m2,siaka_test) - as.numeric(siaka_test$AST.x))^2)/nrow(siaka_test))
rmse1



################## FINDING RMSEs ACROSS ALL PLAYER SETS FOR ASSIST PREDICTIONS
################## Then run a loop to do so

rmse_vec <- numeric(length(game_logs23)) # storage vector for RMSE's of test sets
rmse_step6 <- numeric(length(game_logs23))
rmse_step5 <- numeric(length(game_logs23))
rmse_step4 <- numeric(length(game_logs23))

for(df in 1:length(game_logs23)){
  df_curr <- game_logs23[[df]]
  
  # initializing training and test sets
  df_train <- df_curr[10:round(nrow(df_curr)*0.75),]
  df_test <- df_curr[round(1+nrow(df_curr)*0.75):nrow(df_curr),]
  
  mod1 <- lm(AST.x ~ ASTavg + MP + DRtg + AST.y, data=df_train) # model using stepwise / common knowledge
  mod_step6 <- lm(AST.x ~ Ptavg + `eFG%` + DRB + FG + Pace + TRBavg,data=df_train) # model using just stepwise 6 vars
  mod_step5 <- lm(AST.x ~ `eFG%` + GmSc + FG + `+/-` + Pace,data=df_train) # model using just stepwise 5 vars
  mod_step4 <- lm(AST.x ~ GmSc + FG + `+/-` + PF.y,data=df_train) # model using just stepwise 4 vars
  
  # Then try on the test set, find RMSE
  rmse1 <- sqrt(sum((predict(mod1,df_test) - as.numeric(df_test$AST.x))^2)/nrow(df_test))
  rmse_vec[df] <- rmse1
  
  rmse6 <- sqrt(sum((predict(mod_step6,df_test) - as.numeric(df_test$AST.x))^2)/nrow(df_test))
  rmse_step6[df] <- rmse6
  
  rmse5 <- sqrt(sum((predict(mod_step5,df_test) - as.numeric(df_test$AST.x))^2)/nrow(df_test))
  rmse_step5[df] <- rmse5
  
  rmse4 <- sqrt(sum((predict(mod_step4,df_test) - as.numeric(df_test$AST.x))^2)/nrow(df_test))
  rmse_step4[df] <- rmse4
  
}

# more complex models seem to perform better for assist prediction, but models were similar in accuracy
# using median, mean, and max as criteria for models
summary(rmse_vec) # the second best performing model on the test data; model using stepwise / common knowledge
summary(rmse_step6) # fourth best; model using just stepwise 6 vars
summary(rmse_step5) # the best; model using just stepwise 5 vars
summary(rmse_step4) # third best; model using just stepwise 4 vars


## MODEL SUMMARY

# FOR POINTS: lm(PTS ~ MP + Ptavg + DRtg + `eFG%`, data=df_train); median RMSE of 7.482


# FOR REBOUNDS: lm(TRB.x ~ `eFG%` + `oeFG%` + BLK.y + `oFT/FGA`,data=df_train); median RMSE of 3.105


# FOR ASSISTS: lm(AST.x ~ `eFG%` + GmSc + FG + `+/-` + Pace,data=df_train); median RMSE of 2.099





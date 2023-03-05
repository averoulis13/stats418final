# stats418final
Stats 418 Final Project on Predicting NBA Props

Goal: Predict NBA player statistics for a particular game against a chosen opponent, to be used as a resource for sports gamblers.

● So far we used a scraper to get player statistics and team rankings from basketball-reference.com; we had to loop through each player's web page to get game logs for the current season and filtered out games that they did not play. Each set of player game logs was stored as an element within a list. Then, we joined the opponent team data to each row in which the player faced that opposing team. 

● There were 89 players of interest that fulfilled our criteria of being in the league as of 2019, averaging at least 18 MPG and starting at least 41 team games.

● We will use stats from first 80% of games in each player's game logs to train a model for each future stat, then we'll test the model on remaining games in the game logs. 

● We are working on building an Rshiny app that predicts player props (points, rebounds, and assists) given the player's previous performances and opposing team statistics. There will be drop down menus of players, statistics, and opponents, which will allow us to make and display appropriate stat predictions for player/opponent matchups in the future. 




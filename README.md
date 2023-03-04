# stats418final
Stats 418 Final Project on Predicting NBA Props

Goal: Predict NBA player statistics for a particular game against a chosen opponent, to
be used as a resource for sports gamblers.

● Build an Rshiny app that predicts player props (points, rebs, assists)
given team and opposing team statistics

● Drop down menus of players, statistics, and opponents

● Use scraper to get player statistics and team rankings from basketball-reference.com; had to loop through each player's web page to get game logs for the current season, filtered out games that they did not play

● Use rolling averages of first ~50 games in each player's game logs to train a model for each stat, then test model on remaining games in game logs

● This will allow us to make appropriate stat predictions for player/opponent matchups in the future




# Orchestration file - run on a predetermined basis to post stats
library(baseballr)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)
source("BaseballTest.R")

# Pseudocode:
#   
#   Check if there is a game today, if there is not then quit
# 
#   If there is a game today, Check if the game is finished
#     If not, then quit
#   
#   Check if it has already been posted:
#     Read the .csv of posted games (with listed game_pk values)
#     If it has already been posted, quit
#     
#   
#   
#   If the game is finished, run the program:
#     Run all data-gathering functions
#       Confirm that they run correctly
#     Run visualization function
#       Confirm that png outputs correctly
#     Post it to twitter/whatever website
#     Add game_pk value to posted game .csv file
#   
#   Quit

run_program <- function(team_name="Washington Nationals", game_date=today()) {
  
  if (is.null(game_date)) {stop("No game date provided, or incorrect format (Should be YYYY-MM-DD)")}
  
  # Check if there is a game today
  GAMES = mlb_game_pks(game_date) %>%
    filter(teams.away.team.name == team_name | teams.home.team.name == team_name)

  if (nrow(GAMES) == 0) {stop(paste("No games found for",team_name,"on",game_date))}
  
  print(paste0(nrow(GAMES), " game(s) found for ",team_name," on ",game_date))
  
  # For every game that day - in case of doubleheaders
  for(i in 1:nrow(GAMES)) {
    g <- GAMES[i,]
    GAMEPK = pull(g %>% select(game_pk))
    
    print(paste0("Currently evaluating game_pk ",GAMEPK,": ", g$teams.home.team.name,
                 " (home) vs. ", g$teams.away.team.name," (away) on ", g$gameDate))
    
    if (g$status.detailedState != "Final") {
      print(paste0("game_pk ",GAMEPK,": status.detailedState is not Final (currently: ",
                   g$status.detailedState,"). Skipping..."))
      next
    }
    
    # Check if this game has already been posted before 
    tryCatch({
      posted_games <- read.csv("posted_games.csv") },
      error = function(cond) {
        stop("No record of previously posted games exists, or error opening file. Games should be stored in posted_games.csv")
      })
    
    if (any(posted_games$game==GAMEPK)) {
      posted_game = posted_games %>% filter(game==GAMEPK)
      
      print(paste0("game_pk ",GAMEPK,": Game already posted on ",
                   posted_game$time_posted, ". Skipping..."))
      next
    }
    
    
    # run data gathering functions
    print(paste0("game_pk ",GAMEPK,": Gathering game data..."))
    G_game_info = g %>% select(game_pk, officialDate,
                               teams.away.team.name, teams.away.score, teams.away.isWinner,
                               teams.away.leagueRecord.wins, teams.away.leagueRecord.losses,
                               teams.home.team.name, teams.home.score, teams.home.isWinner,
                               teams.home.leagueRecord.wins, teams.home.leagueRecord.losses)

    tryCatch({
        G_expected_stats = get_expected_batting_leaders(min_pa=50)
      }, error = function(cond) {
        print("Error in function call: get_expected_batting_leaders")
        print(cond)
        stop()})

    tryCatch({
      G_daily_batters = get_daily_batters(team_name, game_pk=GAMEPK)
      }, error = function(cond) {
        print("Error in function call: get_daily_batters")
        print(cond)
        stop()})
    
    tryCatch({
      G_daily_pitcher = get_daily_pitcher(team_name, game_pk=GAMEPK)
      }, error = function(cond) {
        print("Error in function call: get_daily_pitcher")
        print(cond)
        stop()})
    
    tryCatch({
      G_win_probability_chart = get_win_probability_chart(gamepk=GAMEPK)
      }, error = function(cond) {
        print("Error in function call: get_win_probability_chart")
        print(cond)
        stop()})
    
    
    print(paste0("game_pk",GAMEPK,": Successfully gathered data"))
    
    # run visualization function
    image_path <- generate_image(
      game_info=G_game_info, 
      daily_pitching=G_daily_pitcher,
      daily_batting=G_daily_batters, 
      advanced_batting=G_expected_stats,
      win_probability=G_win_probability_chart)
    
    # add game_pk to posted_games.csv
    posted_games[nrow(posted_games)+1,] = c(GAMEPK, toString(Sys.time()))
    write.csv(posted_games, "posted_games.csv", row.names = FALSE, quote=FALSE)
    
    # upload to website
    # TO BE IMPLEMENTED
    
    print("Bottom of the iteration")
    break
  }

  
  
  print("Success")
  
}


run_program(game_date = today()-1)

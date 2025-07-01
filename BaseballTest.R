# Load required packages
library(baseballr)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)

get_top_team_hitters <- function(team, year=2025) {
  
  all_hitters <- fg_batter_leaders(startseason=year, endseason=year, qual='y', 
                               sortdir = 'desc', sortstat = "wRC_plus")
  attach(all_hitters)
  
  # Filter for the specified team
  team_hitters <- hitters %>%
    dplyr::filter(team_name == team) %>%
    select(PlayerName, wRC, wRC_plus) %>%
    #arrange(desc(wRC)) %>%
    head(10)  # Get top 10 players by wRC+
  
  # Create a table for display
  table_grob <- tableGrob(team_hitters, rows = NULL, theme = ttheme_minimal())
  
  # Create a blank background
  image_path <- paste0(team, "_top_hitters_", year, ".png")
  png(image_path, width = 800, height = 800, res = 150)
  grid.draw(table_grob)
  dev.off()
  
  # Return the image path
  return(image_path)
}
#SAMPLE_get_top_team_hitters = get_top_team_hitters()

# Input: The team, the date of a game
# output: A table with the list of qualified hitters, wRC+, wRC, and change from prior day
# TODO make work same day (data seems to not update until day after) - or remove "change" col
get_top_team_hitters_postgame <- function(team, game_date = NULL) {
  
  if (is.null(game_date)) {stop("No game date provided, or incorrect format (Should be YYYY-MM-DD)")}
  
  date <- as.Date(game_date,'%Y-%m-%d')
  game_season = as.numeric(format(date,'%Y'))
  previous_date = date-1
  
  seasons = mlb_seasons_all(sport_id = 1, with_game_type_dates = TRUE)
  
  opening_day = filter(seasons, season_id==game_season)$season_start_date
  
  previous_stats <- fg_batter_leaders(startseason=game_season, endseason=game_season,
                                      startdate = opening_day, 
                                      enddate = previous_date,
                                      month="1000",
                                      qual=0, sortdir = 'desc', sortstat = "wRC")
  
  all_stats <- fg_batter_leaders(startseason=game_season, endseason=game_season,
                                    startdate = opening_day, 
                                    enddate = date,
                                    month="1000",
                                    qual='y', sortdir = 'desc', sortstat = "wRC")
  
  # daily_stats <- fg_batter_leaders(startseason=game_season, endseason=game_season,
  #                                  startdate = game_date, 
  #                                  enddate = game_date,
  #                                  month="1000",
  #                                  qual=0, sortdir = 'desc', sortstat = "wRC")
  
  
  all_stats$old_wRC = previous_stats$wRC[match(all_stats$playerid, previous_stats$playerid)]
  
  # all_stats$daily_wRC = daily_stats$wRC[match(all_stats$playerid, daily_stats$playerid)]
  
  all_stats$change = all_stats$wRC - all_stats$old_wRC
  
  team_hitters <- all_stats %>%
    dplyr::filter(team_name == "WSN") %>%
    select(PlayerName, wRC_plus, wRC, change)
  
  
  # Create a table for display
  #table_grob <- tableGrob(team_hitters, rows = NULL, theme = ttheme_minimal())
  return(team_hitters)
}
SAMPLE_get_top_team_hitters_postgame = get_top_team_hitters_postgame(team="Washington Nationals", game_date = today())

# Input: The team, the date of a game
# output: Info about the starting pitcher and their performance
get_daily_pitcher <- function(team_name="Washington Nationals", game_date=today()) {

  x = mlb_game_pks(game_date) %>%
    filter(teams.away.team.name == team_name | teams.home.team.name == team_name) %>%
    select(game_pk)
  
  prob = mlb_probables(x)
  
  nats_pitcher = prob %>% filter(team==team_name)
  
  stats_summary = mlb_player_game_stats(person_id=nats_pitcher$id, game_pk = x) %>%
    filter(group=="pitching")
  stats_summary$fullName = nats_pitcher$fullName
  return (stats_summary)
}
SAMPLE_get_daily_pitcher = get_daily_pitcher(team_name="Washington Nationals", game_date=today())

# Input (optional): The team, the date of a game
# Output: Info about the outcome of the game (home/away team, score, winner)
get_game_info <- function(team = "Washington Nationals", game_date = today()) {
  
  x = mlb_game_pks(game_date) %>%
    filter(teams.away.team.name == team | teams.home.team.name == team) %>%
    select(game_pk, gameDate,
           teams.away.team.name, teams.away.score, teams.away.isWinner,
           teams.away.leagueRecord.wins, teams.away.leagueRecord.losses,
           teams.home.team.name, teams.home.score, teams.home.isWinner,
           teams.home.leagueRecord.wins, teams.home.leagueRecord.losses)
  
  return (x)
}
SAMPLE_get_game_info = get_game_info()

#input: The team, date of a game
#output: The list of batters who appeared and their stat lines
get_daily_batters <- function(team_name = "Washington Nationals", game_date = today()) {
  
  game_pk = get_game_info(team_name, game_date)$game_pk
  
  x = mlb_batting_orders(game_pk, type="all") %>% 
     filter(teamName == team_name)
  
  player_stats = c()
  
  for (i in 1:nrow(x)) {
    
    id = x[i,1]
    stat = mlb_player_game_stats(id, game_pk) %>% filter(group=="hitting") %>%
      select(summary)
    
    player_stats = c(player_stats, stat)
  }
  
  x$summary = player_stats
  
  return (x)
}
SAMPLE_get_daily_batters = get_daily_batters()

generate_image <- function(plot) {
  
  
  # Create a blank background
  image_path <- paste0("testgridimage.png")
  png(image_path, width = 800, height = 800, res = 150)
  grid.arrange(plot, top="Caption here")
  # grid.draw(plot)
  dev.off()
}

table_grob = get_top_team_hitters_postgame(team, game_date)
get_top_team_hitters(year=2025, team="WSN")

generate_image(table_grob)

calculate_RC <- function(player) {
  
  player = stat
  
  H = player$hits
  BB = player$base_on_balls
  IBB = player$intentional_walks
  HBP = player$hit_by_pitch
  GDP = player$ground_into_double_play
  B2 = player$doubles
  B3 = player$triples
  HR = player$home_runs
  B1 = H - B2 - B3 - HR
  SH = player$sac_bunts
  SF = player$sac_flies
  SB = player$stolen_bases
  AB = player$at_bats
  
  outs_avoided = (H+BB-CS+HBP-GDP)
  numer = (z*((B1+(2*B2)+(3*B3)+(4*HR)+(.26*(BB-IBB+HBP))+(.52 * (SH + SF + SB)))))
    
  denom = (AB + BB + HBP + SF + SH)
  
  
  RC = numer/denom
  
  player$RC = RC
  
  return (player)
}


# Load required packages
# library(baseballr)
# library(ggplot2)
# library(dplyr)
# library(grid)
# library(gridExtra)
# library(lubridate)

sample_team = "Washington Nationals"
sample_date = today() - 3

# Input: The team, the date of a game
# output: A table with the list of qualified hitters, wRC+, wRC, and change from prior day
# TODO make work same day (data seems to not update until day after) - or remove "change" col
get_top_team_hitters_postgame <- function(team="WSN", game_date = today()) {
  
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
                                    qual=1, sortdir = 'desc', sortstat = "wRC")
  
  all_stats$old_wRC = previous_stats$wRC[match(all_stats$playerid, previous_stats$playerid)]
  
  all_stats$change = all_stats$wRC - all_stats$old_wRC
  
  team_hitters <- all_stats %>%
    dplyr::filter(team_name == team) %>%
    select(PlayerName, wRC_plus, wRC, change)

  # filter to hitters if they are TOP 5 OVERALL or PLAYED THAT GAME
  wRC_cutoff = pull(team_hitters %>% slice(5) %>% select(wRC))
  team_hitters = team_hitters %>%
    dplyr::filter(wRC > wRC_cutoff | change != 0) %>%
    rename(Player=PlayerName, 'wRC+'=wRC_plus, 'Change*'=change)
  
  return(team_hitters)
}
# SAMPLE_get_top_team_hitters_postgame = get_top_team_hitters_postgame(team="WSN", sample_date)


# Input: The team, the date of a game
# output: Info about the starting pitcher and their performance
get_daily_pitcher <- function(team_name="Washington Nationals", game_pk=NULL) {
  
  
  # TO DO - update season data to use the bref_daily_pitcher() function
  
  
  
  if (is.null(game_pk)) {stop("No game_pk provided")}
  
  prob = mlb_probables(game_pk)
  
  nats_pitcher = prob %>% filter(team==team_name)
  
  stats_summary = mlb_player_game_stats(person_id=nats_pitcher$id, game_pk) %>%
    filter(group=="pitching")
  stats_summary$fullName = nats_pitcher$fullName
  if(stats_summary$wins == 1) {stats_summary$result = "W"}
  else if(stats_summary$losses == 1) {stats_summary$result = "L"}
  else {stats_summary$result = "ND"}
  
  #add season stats
  season = as.numeric(format(as.Date(nats_pitcher$game_date,'%Y-%m-%d'),'%Y'))
  opening_day = filter(mlb_seasons_all(sport_id = 1, with_game_type_dates = TRUE),
                       season_id==season)$season_start_date
  
  season_stats = bref_daily_pitcher(opening_day, nats_pitcher$game_date) %>% 
    filter(Name == nats_pitcher$fullName)
  stats_summary$season_wins = pull(season_stats %>% select(W))
  stats_summary$season_losses = pull(season_stats %>% select(L))
  stats_summary$season_ERA = pull(season_stats %>% select(ERA))
  stats_summary$IP = pull(season_stats %>% select(IP))
  stats_summary$K = pull(season_stats %>% select(SO))
  stats_summary$WHIP = pull(season_stats %>% select(WHIP))
  
  return (stats_summary)
}
# SAMPLE_get_daily_pitcher = get_daily_pitcher(sample_team, 777305)


# Input (optional): The team, the date of a game
# Output: Info about the outcome of the game (home/away team, score, winner)
get_game_info <- function(team = "Washington Nationals", game_date = today()) {
  
  x = mlb_game_pks(game_date) %>%
    filter(teams.away.team.name == team | teams.home.team.name == team) %>%
    select(game_pk, officialDate,
           teams.away.team.name, teams.away.score, teams.away.isWinner,
           teams.away.leagueRecord.wins, teams.away.leagueRecord.losses,
           teams.home.team.name, teams.home.score, teams.home.isWinner,
           teams.home.leagueRecord.wins, teams.home.leagueRecord.losses)
  
  return (x)
}
# SAMPLE_get_game_info = get_game_info(sample_team, sample_date)


#input: The team, date of a game
#output: The list of batters who appeared and their stat lines
get_daily_batters <- function(team_name = "Washington Nationals", game_pk=NULL) {
  
  if (is.null(game_pk)) {stop("No game_pk provided")}
  
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
  
  # select only desired columns
  x = select(x, batting_order, fullName, abbreviation, summary)
  x = rename(x, No=batting_order, Player=fullName, Pos=abbreviation, Stats=summary)
  
  return (x)
}
# SAMPLE_get_daily_batters = get_daily_batters(sample_team, sample_date)


# Input: gamepk for a game
# Output: a win probability chart for that game
get_win_probability_chart <- function(gamepk) {
  
  wp_matrix = as.data.frame(mlb_game_wp(game_pk=gamepk))
  pbp_data = mlb_pbp(game_pk=gamepk)
  
  combined = merge(x=wp_matrix, y=pbp_data, by.x="at_bat_index", by.y="atBatIndex") 
  
  if (pull(select(slice(combined, 1), home_team)) == "Washington Nationals") {
    combined <- combined %>% 
      select(at_bat_index, 
             home_team_win_probability, 
             leverage_index, 
             about.halfInning, 
             about.inning) %>%
      rename('win_probability'=home_team_win_probability)
  } else {
    combined <- combined %>% 
      select(at_bat_index, 
             away_team_win_probability, 
             leverage_index, 
             about.halfInning, 
             about.inning) %>%
      rename('win_probability'=away_team_win_probability)
  }
  
  combined <- combined[match(unique(combined$at_bat_index), combined$at_bat_index),]
  combined$inning <- paste0(substr(toupper(combined$about.halfInning),1,1), combined$about.inning)
  
  first_at_bats <- c(combined[match(unique(combined$inning), combined$inning),]$at_bat_index)
  innings <- c(combined[match(unique(combined$inning), combined$inning),]$inning)
  
  p <- ggplot(data=combined, aes(x=at_bat_index, y=win_probability)) + 
    geom_tile(aes(fill = about.halfInning), height = Inf, alpha = 0.1, show.legend=FALSE) + 
    scale_fill_manual(values=c("#000000", "#FFFFFF")) +
    geom_line() + 
    scale_x_continuous(name=element_blank(), breaks=first_at_bats, labels=innings, 
                       limits=c(-0.5, max(combined$at_bat_index)+0.5),
                       expand=expansion(mult = 0, add = 0)) +
    scale_y_continuous(name="Nationals Win Probability", breaks=c(0,25,50,75,100),
                       labels=c("0%", "25%", "50%", "75%", "100%"),
                       limits=c(0,100), expand=expansion(mult = 0, add = 0)) +
    labs(title="Win Probability by Inning") +
    theme_test() +
    theme(axis.ticks = element_blank(), 
          panel.grid.major.y = element_line(color = "gray",size = 0.2,linetype = 2),
          plot.title= element_text(face="bold", hjust=0.5, size=12),
          plot.background = element_rect(fill="#ffffed"),
          panel.background = element_rect(fill="#fffdf7"),
          axis.text = element_text(size=6))
  
  return (p)
  
}
# SAMPLE_get_win_probability_chart = get_win_probability_chart(gamepk=777305)

# Input: Currently only minimum PAs required
# Output: Leaders for expected batting stats. Currently only works for the Nationals
get_expected_batting_leaders <- function(min_pa = 'q', year=2025, player_type="batter") {
  
  url <- paste0("https://baseballsavant.mlb.com/leaderboard/expected_statistics?type=", 
                player_type, # "pitcher", "batter-team" also work 
                "&year=", year, 
                "&position=", "", 
                "&team=", "120", # needs to be numeric to work, I think
                "&min=", min_pa, 
                "&csv=true")
  payload <- data.table::fread(url, encoding = "UTF-8", fill=TRUE)
  
  payload <- rename(payload, 
                    Player="last_name, first_name",
                    PA=pa,
                    BA=ba,
                    xBA=est_ba,
                    SLG=slg,
                    xSLG=est_slg,
                    wOBA=woba,
                    xwOBA=est_woba,
                    Diff=est_woba_minus_woba_diff)
  
  payload <- arrange(payload, desc(xwOBA))[1:11,]
  
  
  return (payload %>% select(Player, PA, BA, xBA, wOBA, xwOBA, Diff))
  
}

# helper function to calculate runs created for a given player
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


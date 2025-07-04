library(baseballr)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)


generate_image <- function(game_info=NULL, daily_pitching=NULL, 
                           daily_batting=NULL, advanced_batting=NULL,
                           win_probability=NULL) {
  
# SETUP ------------------------------------------------------------------------
  image_path <- paste0(game_info$officialDate,"-",game_info$game_pk,".png")
  png(image_path, width = 1600, height = 900, res = 140, bg="#ffffed")
  
# TOP LEFT - high-level summary of the game ------------------------------------
  # Teams, scores, record
  game_summary_table <- data.frame(Team = c(paste0(game_info$teams.away.team.name," (", game_info$teams.away.leagueRecord.wins,"-",game_info$teams.away.leagueRecord.losses,")"), 
                                            paste0(game_info$teams.home.team.name," (", game_info$teams.home.leagueRecord.wins,"-",game_info$teams.home.leagueRecord.losses,")")),
                                   Score = c(game_info$teams.away.score, game_info$teams.home.score))
  
  game_plot = tableGrob(game_summary_table, rows=NULL, theme=ttheme_minimal())

# MIDDLE LEFT - starting pitcher stat line from the game -------------------------
  # Name, decision, summary line, season record and ERA
  pitching_plot = textGrob(paste0("Starting Pitcher: ", daily_pitching$fullName,
                                  " (",daily_pitching$result,")\n",
                                  daily_pitching$summary, "\n",
                                  "Season: ",daily_pitching$season_wins, "-",
                                 daily_pitching$season_losses,", ",
                                 trimws(format(round(daily_pitching$season_ERA, 2), nsmall=2))," ERA\n",
                                 daily_pitching$IP, " IP, ", daily_pitching$K, " K, ", 
                                 daily_pitching$WHIP, " WHIP"))
  
# TOP RIGHT - win probability chart --------------------------------------------
  
  probability_plot=win_probability
  
# BOTTOM LEFT - Batting Summary Table ------------------------------------------
  
  # Set up themes for tables
  batting_theme <- ttheme_default(core=list(bg_params = list(fill="#fffdf7", col="grey80"),
                                            fg_params=list(hjust=0, x=0.1)),
                                  colhead=list(bg_params = list(fill="#ebe5d5", col="grey80")),
                                  rowhead=list(fg_params=list(hjust=1.5)))
  
  batting_summary_table <- lapply(daily_batting, unlist)
  batting_summary_table <- as.data.frame(batting_summary_table, stringAsFactors=F) %>%
    subset(select = -c(No))
  batting_plot = tableGrob(batting_summary_table, rows=daily_batting$No, 
                           theme=batting_theme)
  
# BOTTOM RIGHT - advanced season hitting stats ---------------------------------
  # advanced_batting$wRC = trimws(format(round(advanced_batting$wRC, 1), nsmall=1))
  # advanced_batting$'wRC+' = trimws(format(round(advanced_batting$'wRC+', 1), nsmall=1))
  # advanced_batting$'Change*' = trimws(format(round(advanced_batting$'Change*', 1), nsmall=1))
  advanced_plot = tableGrob(advanced_batting, rows=NULL, theme=batting_theme)
  
  
# FINAL - Arrange all plots ----------------------------------------------------
  grid.arrange(game_plot, pitching_plot, probability_plot,batting_plot, advanced_plot, 
               nrow=5, ncol=4, widths=c(.1, 2, 3, .1), heights=c(.1, 1, 1, 3, .1), 
               layout_matrix=rbind(c(NA, NA, NA, NA),
                                   c(NA, 1, 3, NA),
                                   c(NA, 2, 3, NA),
                                   c(NA, 4, 5, NA),
                                   c(NA, NA, NA, NA)),
               top=textGrob(paste("Nationals Game Results:", game_info$officialDate),
                            gp=gpar(fontface=2, fontsize=14), vjust=1),
               bottom=textGrob("Expected player statistics sorted by season xwOBA",
                              gp = gpar(fontface=3, fontsize=9), hjust=1, x=.98))
  dev.off()
  
  return (image_path)
}

# Test Function
# generate_image(game_info=SAMPLE_get_game_info, daily_pitching=SAMPLE_get_daily_pitcher,
#                daily_batting=SAMPLE_get_daily_batters, advanced_batting=SAMPLE_get_top_team_hitters_postgame,
#                win_probability=SAMPLE_get_win_probability_chart)


# testing plot for win probability ----
# clean_merge$inning <- paste0(substr(toupper(clean_merge$about.halfInning),1,1), clean_merge$about.inning)
# 
# first_at_bats <- c(clean_merge[match(unique(clean_merge$inning), clean_merge$inning),]$at_bat_index)
# innings <- c(clean_merge[match(unique(clean_merge$inning), clean_merge$inning),]$inning)
# 
# ggplot(data=clean_merge, aes(x=at_bat_index, y=home_team_win_probability)) + 
#   geom_tile(aes(fill = about.halfInning), height = Inf, alpha = 0.1, show.legend=FALSE) + 
#   scale_fill_manual(values=c("#000000", "#FFFFFF")) +
#   geom_line() + 
#   scale_x_continuous(name=element_blank(), breaks=first_at_bats, labels=innings, 
#                      limits=c(-0.5, max(clean_merge$at_bat_index)+0.5),
#                      expand=expansion(mult = 0, add = 0)) +
#   scale_y_continuous(name="Nationals Win Probability", breaks=c(0,25,50,75,100),
#                      labels=c("0%", "25%", "50%", "75%", "100%"),
#                      limits=c(0,100), expand=expansion(mult = 0, add = 0)) +
#   labs(title="Win Probability by Inning") +
#   theme_test() +
#   theme(axis.ticks = element_blank(), 
#         panel.grid.major.y = element_line(color = "gray",size = 0.2,linetype = 2),
#         plot.title = element_text(face="bold", hjust=0.5, size=12),
#         plot.background = element_rect(fill="#ffffed"),
#         panel.background = element_rect(fill="#fffdf7"),
#         axis.text = element_text(size=6))


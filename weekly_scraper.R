library("XML")
library("dplyr")
library("rvest")

#### This is the motherload
#What week is it?
week_stat <- 2
leagueId <- 45342
seasonId <- 2018

#Functions:
calc_wins_vs_field <- function(week, team) {
  wins <- sum(scores[scores$TEAM == team & scores$WEEK == week, "SCORE"] > scores[scores$WEEK == week, "SCORE"], na.rm=TRUE)
  return(wins)
}

fix_headers <- function(df) {
  colnames(df) <- as.character(unlist(df[1,]))
  df <- df[-1,]
  return(df)
}

#########################################
## Load Data ############################
#########################################
#Team Data
team_lkup <- as.data.frame(read.csv("~/data/boknows/team_lkup.csv", header = TRUE, stringsAsFactors = FALSE))
team_lkup <- team_lkup[team_lkup$SEASON == seasonId ,c("TEAM_CODE", "TEAM_NAME", "TEAM_ID")]
team_lkup$TEAM_CODE <- tolower(team_lkup$TEAM_CODE)

#Lets get last weeks season long ranks:
last_season_ranks <- as.data.frame(read.csv(paste("~/data/weekly_scrape_files/2018/season_long_week", week_stat-1, ".csv", sep= ""), 
                                            header = FALSE,
                                            stringsAsFactors = FALSE))

last_season_ranks <- last_season_ranks[,1:2]
names(last_season_ranks) <- c("last_rank", "team")

#Lets get last weeks weekly ranks
last_weeks_ranks <- as.data.frame(read.csv(paste("~/data/weekly_scrape_files/2018/weekly_rank_week", week_stat-1, ".csv", sep= ""), 
                                            header = FALSE,
                                            stringsAsFactors = FALSE))
last_weeks_ranks <- last_weeks_ranks[,1:2]
names(last_weeks_ranks) <- c("last_rank", "team")

#Lets get last weeks divisions
last_weeks_div <- as.data.frame(read.csv(paste("~/data/weekly_scrape_files/2018/divisions_week", week_stat-1, ".csv", sep= ""), 
                                           header = FALSE,
                                           stringsAsFactors = FALSE))
last_weeks_div <- last_weeks_div[,1:2]
names(last_weeks_div) <- c("last_rank", "team")

#Lets get the season long scores:
scores <- as.data.frame(read.csv("~/data/boknows/full_schedule.csv", header = TRUE, stringsAsFactors = FALSE))
scores <- scores[complete.cases(scores) & scores$SEASON == 2018,]
scores$TEAM <- tolower(scores$TEAM)
scores$OPPONENT <- tolower(scores$OPPONENT)

#Real quick apply wins vs fields stat
scores$win_vs_field <- mapply(calc_wins_vs_field, week = scores$WEEK, team = scores$TEAM)
scores$loss_vs_field <- 11 - scores$win_vs_field

#Go Get Projections:

team_list <- team_lkup[, "TEAM_ID"]

get_scores <- function(url) {
  html_scores <- read_html(url) %>%
    html_nodes(".boxscoreDangler") %>%
    html_text()
  gsub("Projected Total: ", "", html_scores)
}

get_teams <- function(url) {
  html_names <- read_html(url) %>%
    html_nodes(".playerTableBgRowHead") %>%
    html_text()
  gsub("STATS", "", html_names[c(1,3)])
}

proj_scores <- data.frame(TEAM_CODE=character(),
                          TEAM_NAME=character(),
                          PROJ_SCORE=numeric(), 
                          stringsAsFactors=FALSE)


scoringPeriodId <- week_stat
  
for (team in team_list) {
  #Build URL
  teamId <- team
  team_code <- as.character(team_lkup[team_lkup$TEAM_ID == teamId, "TEAM_CODE"])
  team_name <- as.character(team_lkup[team_lkup$TEAM_ID == teamId, "TEAM_NAME"])
  url = paste("http://games.espn.com/ffl/matchuppreview?leagueId=", leagueId, "&teamId=", teamId, "&scoringPeriodId=", scoringPeriodId, "&seasonId=", seasonId, sep = "")
  
  #Pull in detailed stats page:
  proj_df <- data.frame(TEAM_NAME = as.character(get_teams(url)), PROJ_SCORE = as.numeric(get_scores(url)))
  
  proj_scores <- bind_rows(proj_scores, c(TEAM_CODE = tolower(team_code), proj_df[proj_df$TEAM_NAME == team_name,]))
  
  Sys.sleep(runif(1, 5, 15))
  print(url)
}


#########################################
## Season Long Stats ####################
#########################################
#Aggregate Season Long Stats
season_long <- scores %>%
  group_by(TEAM) %>%
  summarize(Average = mean(SCORE),
            Wins = sum(win_vs_field),
            Losses = sum(loss_vs_field)) %>%
  arrange(desc(Wins), desc(Average))

#Apply Rank
season_long$rank <- 1:12

#Concat Record
season_long$record <- paste(season_long$Wins, "-", season_long$Losses, sep = "")

#Apply Long Name
season_long <- left_join(season_long, team_lkup, by = c("TEAM" = "TEAM_CODE"))

#Get Diff from last weeks ranks
season_long <- left_join(season_long, last_season_ranks, by = c("TEAM" = "team"))
season_long$diff <- season_long$last_rank - season_long$rank

#Clean up final table
final_season_long <- season_long[,c("rank", "TEAM", "TEAM_NAME", "diff", "record", "Average")]

write.table(final_season_long, file = paste("~/data/weekly_scrape_files/2018/season_long_week", week_stat, ".csv", sep= ""),
          sep=",",
          quote = FALSE,
          row.names=FALSE,
          col.names=FALSE)

#########################################
## This Week Stats ######################
#########################################
#Aggregate Weekly Stats
this_week <- scores[scores$WEEK == week_stat,] %>%
  group_by(TEAM) %>%
  summarize(Average = mean(SCORE),
            Wins = sum(win_vs_field),
            Losses = sum(loss_vs_field)) %>%
  arrange(desc(Wins), desc(Average))

#Apply Rank
this_week$rank <- 1:12

#Concat Record
this_week$record <- paste(this_week$Wins, "-", this_week$Losses, sep = "")

#Apply Long Name
this_week <- left_join(this_week, team_lkup, by = c("TEAM" = "TEAM_CODE"))

#Get Diff from last weeks ranks
this_week <- left_join(this_week, last_weeks_ranks, by = c("TEAM" = "team"))
this_week$diff <- this_week$last_rank - this_week$rank

#Clean up final table
final_this_week <- this_week[,c("rank", "TEAM", "TEAM_NAME", "diff", "record", "Average")]
write.table(final_this_week, file = paste("~/data/weekly_scrape_files/2018/weekly_rank_week", week_stat, ".csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)


#########################################
## Division Stats #######################
#########################################
division.page <- htmlParse("http://games.espn.com/ffl/standings?leagueId=45342&seasonId=2018")
division.page <- readHTMLTable(division.page,stringAsFactors = FALSE)

clean_records <- function(df, div) {
  colnames(df) <- as.character(unlist(df[1,]))
  df <- df[-1,]
  df$RECORD <- paste(df$W, "-", df$L, sep = "")
  df$RANK <- 1:6
  df$DIV <- div   
  df <- df[,c("RANK", "DIV", "TEAM", "RECORD")]
  
  return(df)
}

clean_points <- function(df) {
  colnames(df) <- as.character(unlist(df[1,]))
  df <- df[-1,]
  df$TEAM <- gsub(" \\(.*?\\)","",df$`TEAM, OWNER(S)`)
  df <- df[,c("TEAM", "PF", "PA")]
  
  return(df)
}

div1_records <- division.page[[3]]
div2_records <- division.page[[4]]
div1_points <- division.page[[5]]
div2_points <- division.page[[6]]

all_records <- rbind(clean_records(div2_records, 1), clean_records(div1_records, 2))
all_points <- rbind(clean_points(div2_points), clean_points(div1_points))

total_division <- left_join(all_records, all_points, by = c("TEAM" = "TEAM"))


#Apply Long Name
total_division <- left_join(total_division, team_lkup, by = c("TEAM" = "TEAM_NAME"))

#Get Diff
total_division <- left_join(total_division, last_weeks_div, by = c("TEAM_CODE" = "team"))

total_division$diff <- total_division$last_rank - total_division$RANK


#Clean up final table
final_division <- total_division[,c("RANK", "TEAM_CODE", "TEAM", "diff", "RECORD", "PF", "PA")]
write.table(final_division, file = paste("~/data/weekly_scrape_files/2018/divisions_week", week_stat, ".csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)

#########################################
## Division Power #######################
#########################################
div1_name <- as.character(division.page[[2]][9,1])
div2_name <- as.character(division.page[[2]][1,1])

div1_total_points <- total_division[total_division$DIV == 1, "PF"]
div1_total_points <- sum(as.numeric(levels(div1_total_points))[div1_total_points])

div2_total_points <- total_division[total_division$DIV == 2, "PF"]
div2_total_points <- sum(as.numeric(levels(div2_total_points))[div2_total_points])

div_power <- rbind(c(div1_name, div2_name)
                   ,c(round((100 * (div1_total_points/(div1_total_points+div2_total_points))), 2),  
                      round((100 * (div2_total_points/(div1_total_points+div2_total_points))), 2)))

write.table(div_power, file = paste("~/data/weekly_scrape_files/2018/div_power_week", week_stat, ".csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)


#########################################
## Top Scorers ##########################
#########################################
#Codes:
#QB: 0
#RB: 2
#WR: 4
#TE: 6
#IDP: 15

slot_category <- c(0, 2, 4, 6, 15)

top_players <- htmlParse(paste("http://games.espn.com/ffl/leaders?leagueId=45342&seasonId=2018&scoringPeriodId=",
                               week_stat,
                               sep = ""))
top_players <- readHTMLTable(top_players,stringAsFactors = FALSE)

positions <- c("S", "EDR", "QB", "RB", "WR", "TE", "LB",  "DE", "DT", "CB")

clean_top_players <- function(df) {
  df <- fix_headers(df)
  for (position in positions) {
    df[grepl(position, df$`PLAYER, TEAM POS`, ignore.case = FALSE) == TRUE, "POS"] <- gsub("[$]", "", position)
  }
  df$PLAYER <- gsub("^([^,]*),[^,]*(.*)$", "\\1", df$`PLAYER, TEAM POS`)
  df$PLAYER <- str_replace_all(df$PLAYER, "[^[:alnum:][:blank:]+./\\-]", "")
  df$TEAM <- gsub("WA \\(Wed\\)","FA",df$TYPE) 
  
  df$final <- paste(df$PLAYER, " - ", df$PTS, " (", df$TEAM, ")", sep = "")
  df$PTS <- as.numeric(levels(df$PTS))[df$PTS]
  
  df <- df[1:5,c("final", "PTS", "POS")]
  return(df)
}

top_offense <- top_players[[2]]
top_defense <- top_players[[3]]

top_overall <- rbind(clean_top_players(top_offense), clean_top_players(top_defense))
top_overall <- head(top_overall[order(-top_overall$PTS),"final"],5)
top_five <- c("Overall", top_overall)

#For all other positions:
for (slot in slot_category) {
  top_players <- htmlParse(paste("http://games.espn.com/ffl/leaders?leagueId=45342&seasonId=2018&scoringPeriodId=",
                                 week_stat,
                                 "&slotCategoryId=",
                                 slot,
                                 sep = ""))
  top_players <- readHTMLTable(top_players,stringAsFactors = FALSE)
  top_players <- top_players[[2]]
  
  top_5_players <- clean_top_players(top_players)
  
  if (slot == 15) {
    top_5_players$POS <- "IDP"
  }
  top_five <- rbind(top_five, c(unique(top_5_players$POS), top_5_players$final))
}

write.table(top_five, file = paste("~/data/weekly_scrape_files/2018/top_players_week", week_stat, ".csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)

#########################################
## Weekly Awards ########################
#########################################
get_prv_score <- function(week, team) {
  scores[scores$TEAM == team & scores$WEEK == (week_stat - 1), "SCORE"]
}

scores$last_score_diff <- scores$SCOR - mapply(get_prv_score, week = scores$WEEK, team = scores$TEAM)

scores <- scores[order(scores$WEEK, scores$TEAM),]
awards_stats <- scores[scores$WEEK == week_stat,]

#Rank Scores
ranks <- c("12th", "11th", "10th", "9th", "8th", "7th", "6th", "5th", "4th", "3rd", "2nd", "1st")
awards_stats[order(-awards_stats$SCORE), "score_bot_rank"] <- ranks
awards_stats[order(awards_stats$SCORE), "score_top_rank"] <- ranks

#Prev Weeks Score
awards_stats$last_score <- scores[scores$WEEK == (week_stat - 1), "SCORE"]

#Opponents Score
awards_stats <- left_join(awards_stats, awards_stats[,c("TEAM", "SCORE")], by = c("OPPONENT" = "TEAM"), suffix = c("", "_OPP"))

#Margin of Win/Loss
awards_stats$game_margin <- awards_stats$SCORE - awards_stats$SCORE_OPP

#Change since last week
awards_stats$last_score_diff <- awards_stats$SCORE - awards_stats$last_score

#League avg and diff from league average
awards_stats$league_avg <- mean(awards_stats$SCORE) 
awards_stats$league_avg_diff <- awards_stats$SCORE - awards_stats$league_avg

#Assign three bins for score types. 3 is best bin, 1 is worst
awards_stats[order(awards_stats$SCORE), "score_bin"] <- rep(1:3, each = 4)

#Assign projections and proj diff
awards_stats <- left_join(awards_stats, proj_scores, by = c("TEAM" = "TEAM_CODE"))
awards_stats$proj_diff <- awards_stats$SCORE - awards_stats$PROJ_SCORE

#Get schedule scores/ranks
opp_wins <- left_join(scores, scores[,c("TEAM", "WEEK", "SCORE", "win_vs_field")], by = c("OPPONENT" = "TEAM", "WEEK", "WEEK"), suffix = c("", "_OPP"))

total_opp_wins <- opp_wins %>%
  group_by(TEAM) %>%
  summarize(opp_wins = sum(win_vs_field_OPP),
            opp_points = sum(SCORE_OPP))

awards_stats <- left_join(awards_stats, total_opp_wins, by = c("TEAM" = "TEAM"))

#Get week to week variance
team_variance <- scores[scores$last_score_diff != 0,] %>%
  group_by(TEAM) %>%
  summarize(variance = mean(last_score_diff))

awards_stats <- left_join(awards_stats, team_variance, by = c("TEAM" = "TEAM"))

#Opponent Full Name
awards_stats <- left_join(awards_stats, team_lkup, by = c("OPPONENT" = "TEAM_CODE"), suffix = c("", "_OPP"))

#########################################
### Pull out individual awards ##########
#########################################
#Top Dog: Highest score of the week
top_dog <- awards_stats[awards_stats$SCORE == max(awards_stats$SCORE), c("TEAM", "TEAM_NAME", "SCORE")]
top_dog_final <- c(top_dog$TEAM, 
                   "Top Dog", 
                   top_dog$TEAM_NAME, 
                   paste("Led the league with a score of", top_dog$SCORE, "points."))


#Overkill: Defeat the opposing team by 50 points or more.
overkill <- awards_stats[awards_stats$game_margin > 50 & awards_stats$SCORE == max(awards_stats$SCORE),c("TEAM", "TEAM_NAME", "game_margin", "TEAM_NAME_OPP")]
overkill_final <- c(overkill$TEAM, 
                    "Overkill", 
                    overkill$TEAM_NAME, 
                    paste("Defeated", overkill$TEAM_NAME_OPP, "by", overkill$game_margin, "points."))


#I'll Take It: Get a win with a score ranking in the bottom 1/3 (4th, 3rd, or 2nd lowest)
ill_take_it <- awards_stats[awards_stats$score_bin == 1 & awards_stats$WIN == 1, c("TEAM", "TEAM_NAME", "score_bot_rank")]
ill_take_it_final <- c(ill_take_it$TEAM, "I'll Take It", 
                       ill_take_it$TEAM_NAME, 
                       paste("Picked up a win with the", ill_take_it$score_bot_rank, "lowest score."))


#No Luck: Get a loss with a score ranking in the top third (2nd, 3rd, 4th highest)
no_luck <- awards_stats[awards_stats$score_bin == 3 & awards_stats$WIN == 0, c("TEAM", "TEAM_NAME", "score_top_rank")]
no_luck_final <- c(no_luck$TEAM, "No Luck", 
                   no_luck$TEAM_NAME, 
                   paste("Lost with the", no_luck$score_top_rank, "highest score."))


#Nail Biter:	Win with the smallest difference in points that is equal to or less than 6 (one touchdown)
nail_biter <- awards_stats[awards_stats$game_margin > 0 & awards_stats$game_margin < 6, 
                           c("TEAM", "TEAM_NAME", "game_margin", "TEAM_NAME_OPP")]
nail_biter_final <- c(nail_biter$TEAM, 
                      "Nail Biter", 
                      nail_biter$TEAM_NAME, 
                      paste("Defeated", nail_biter$TEAM_NAME_OPP, "by only", round(nail_biter$game_margin, digits = 1), "points."))


#Rise Above: Highest difference between projection and final score
rise_above <- awards_stats[awards_stats$proj_diff == max(awards_stats$proj_diff), 
                           c("TEAM", "TEAM_NAME", "proj_diff")]
rise_above_final <- c(rise_above$TEAM, 
                      "Rise Above", 
                      rise_above$TEAM_NAME, 
                      paste("Scored above projections by", rise_above$proj_diff, "points."))



#HIGH HOPES	Lowest difference between projection and final score
high_hopes <- awards_stats[awards_stats$proj_diff == min(awards_stats$proj_diff), 
                           c("TEAM", "TEAM_NAME", "proj_diff")]
high_hopes_final <- c(high_hopes$TEAM, 
                      "High Hopes", 
                      high_hopes$TEAM_NAME, 
                      paste("Fell below projected score by", abs(high_hopes$proj_diff), "points."))


#Get It Together:	Have the lowest score AND be below the rest of the league’s average by 40 points.
get_it_together <- awards_stats[awards_stats$SCORE == min(awards_stats$SCORE) & awards_stats$league_avg_diff < -40, 
                                c("TEAM", "TEAM_NAME", "SCORE", "league_avg_diff")]
get_it_together_final <- c(get_it_together$TEAM, 
                           "Get It Together", 
                           get_it_together$TEAM_NAME, 
                           paste("Scored", get_it_together$SCORE, "points – which was", abs(get_it_together$league_avg_diff), 
                                 "points below the rest of the league’s average."))


#Beast Mode	Have the highest score AND be above the league’s weekly average by 40 points.
beast_mode <- awards_stats[awards_stats$SCORE == max(awards_stats$SCORE) & awards_stats$league_avg_diff > 40, 
                           c("TEAM", "TEAM_NAME", "SCORE", "league_avg_diff")]
beast_mode_final <- c(beast_mode$TEAM, 
                      "Get It Together", 
                      beast_mode$TEAM_NAME, 
                      paste("Scored", beast_mode$SCORE, "points – which was", abs(beast_mode$league_avg_diff), 
                            "points above the rest of the league’s average."))


#On Point: Have the lowest average point variance equal to or below 6.
on_point <- awards_stats[abs(awards_stats$variance) == min(abs(awards_stats$variance)) & abs(awards_stats$variance) < 6,
                         c("TEAM", "TEAM_NAME", "variance")]
on_point_final <- c(on_point$TEAM, 
                    "On Point", 
                    on_point$TEAM_NAME, 
                    paste("Holding steady with a score variance of", round(abs(on_point$variance), digits = 2), "points."))


#ADHD:	Have the highest average point variance equal to or above 40.
adhd <- awards_stats[abs(awards_stats$variance) == max(abs(awards_stats$variance)) & abs(awards_stats$variance) > 40,
                     c("TEAM", "TEAM_NAME", "variance")]
adhd_final <- c(adhd$TEAM, 
                "ADHD", 
                adhd$TEAM_NAME, 
                paste("Across the board with a score variance of", round(abs(adhd$variance), digits = 2), "points."))


#Bounce Back:	Have the highest increase from last week’s score that’s at or above a 30 point improvement.
bounce_back <- awards_stats[awards_stats$last_score_diff == max(awards_stats$last_score_diff) & awards_stats$last_score_diff > 30,
                            c("TEAM", "TEAM_NAME", "last_score_diff")]
bounce_back_final <- c(bounce_back$TEAM, 
                       "Bounce Back", 
                       bounce_back$TEAM_NAME, 
                       paste("Scored", round(abs(bounce_back$last_score_diff), digits = 2), "points more than last week."))


#Slip Up:	Have the highest decrease from last week’s score that’s at or below a 30 point difference.
slip_up <- awards_stats[awards_stats$last_score_diff == min(awards_stats$last_score_diff) & awards_stats$last_score_diff < -30,
                        c("TEAM", "TEAM_NAME", "last_score_diff")]
slip_up_final <- c(slip_up$TEAM, 
                   "Slip Up", 
                   slip_up$TEAM_NAME, 
                   paste("Scored", round(abs(slip_up$last_score_diff), digits = 2), "points less than last week."))


#Uphill Battle:	Most difficult schedule compared to rest of the teams.
uphill_battle <- awards_stats %>%
  subset(opp_wins == max(opp_wins), select = c("TEAM", "TEAM_NAME", "opp_points")) %>%
  subset(opp_points == max(opp_points))
uphill_battle_final <- c(uphill_battle$TEAM, 
                         "Uphill Battle", 
                         uphill_battle$TEAM_NAME, 
                         "Most difficult schedule of the season so far.")


#Cruise Control: Easiest schedule compared to the rest of the teams
cruise_control <- awards_stats %>%
  subset(opp_wins == min(opp_wins), select = c("TEAM", "TEAM_NAME", "opp_points")) %>%
  subset(opp_points == min(opp_points))
cruise_control_final <- c(cruise_control$TEAM, 
                          "Cruise Control", 
                          cruise_control$TEAM_NAME, 
                          "Easiest schedule of the season so far.")


#########################################
### Stack Awards               ##########
#########################################
all_awards <- ""

awards <- list(top_dog_final,
               overkill_final,
               ill_take_it_final,
               no_luck_final,
               nail_biter_final,
               rise_above_final,
               high_hopes_final,
               get_it_together_final,
               beast_mode_final,
               on_point_final,
               adhd_final,
               bounce_back_final,
               slip_up_final,
               uphill_battle_final,
               cruise_control_final)

for (award in awards) {
  if (length(award) == 4) {
    all_awards <- rbind(all_awards, award)
  } else {
    all_awards <- rbind(all_awards, c("", "", "", ""))
  }
}


write.table(all_awards[-1,], file = paste("~/data/weekly_scrape_files/2018/awards_week", week_stat, ".csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)


#########################################
## Combine All Files ####################
#########################################

col_names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")

format_table <- function(df) {
  df <- df %>%
    as.data.frame() %>%
    mutate_all(as.character)
  names(df) <- col_names[1:length(df)]
  return(df)
}


final_scrape_export <- bind_rows(format_table(final_season_long),
          format_table(final_this_week),
          format_table(final_division),
          format_table(div_power),
          format_table(top_five),
          format_table(all_awards[-1,]))

final_scrape_export[is.na(final_scrape_export)] <- ""

write.table(final_scrape_export, file = paste("~/data/weekly_scrape_files/2018/week", week_stat, "scrape.csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)

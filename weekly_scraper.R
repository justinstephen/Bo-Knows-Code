library("XML")
library("dplyr")

#### This is the motherload
#What week is it?
week_stat <- 2

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
team_lkup <- unique(team_lkup[,2:3])
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

#Real quick apply wins vs fields stat
scores$win_vs_field <- mapply(calc_wins_vs_field, week = scores$WEEK, team = scores$TEAM)
scores$loss_vs_field <- 11 - scores$win_vs_field

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

top_players <- htmlParse("http://games.espn.com/ffl/leaders?leagueId=45342&scoringPeriodId=2&seasonId=2018")
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
          format_table(top_five))

final_scrape_export[is.na(final_scrape_export)] <- ""

write.table(final_scrape_export, file = paste("~/data/weekly_scrape_files/2018/week", week_stat, "scrape.csv", sep= ""),
            sep=",",
            quote = FALSE,
            row.names=FALSE,
            col.names=FALSE)

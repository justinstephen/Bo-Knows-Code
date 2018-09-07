library("XML")
library("dplyr")
library("tibble")
library("tidyr")
library("stringr")


##FULL DATA CAN ONLY BE SCRAPED FROM 2017- SEASONS
if (exists("full_stats")) {
  rm(full_stats)
}

fixRows <- function(df) {
  df[df$V3 == "** BYE **",][, 5:ncol(df)] <- df[df$V3 == "** BYE **",][, 4:(ncol(df)-1)]
  return(df)
}

cleanTable <- function(df) {
  df <- mutate_if(df, is.factor, as.character)
  df <- fixRows(df)
  names(df) <- if (df[1,1] == 'SLOT') {as.character(unlist(df[1,]))} else as.character(unlist(df[2,]))
  df <- if (df[1,1] == 'SLOT') {df[c(2:nrow(df)),]} else df[c(3:nrow(df)),]
  return(df)
}

team_lkup <- as.data.frame(read.csv("~/data/boknows/team_lkup.csv", header = TRUE))

leagueId <- 45342
seasonId <- 2017
weeks <- 13
team_list <- team_lkup[team_lkup$SEASON == seasonId, "TEAM_ID"]

for (week in weeks) {
  scoringPeriodId <- week
  
  for (team in team_list) {
    #Build URL
    teamId <- team
    url = paste("http://games.espn.com/ffl/boxscorescoring?leagueId=", leagueId, "&teamId=", teamId, "&scoringPeriodId=", scoringPeriodId, "&seasonId=", seasonId, "&view=scoringperiod&version=scoring", sep = "")
  
    #Pull in detailed stats page:
    breakdown <- htmlParse(url)
    breakdown.page <- readHTMLTable(breakdown, stringAsFactors = FALSE)
    breakdown.page <- breakdown.page[-1]
    
    tables <- vector(mode = "list", length = length(breakdown.page))
    names(tables) <- names(breakdown.page)
    
    #Classify the subtables based on size
    for(table in seq_along(tables)) {
      tables[table] <- if (length(breakdown.page[[table]]) == 34) {"offense"} 
                       else if (length(breakdown.page[[table]]) == 28) {"defense"}
                       else "NA"
    }
    
    #Clean each table
    for(table in seq_along(tables)) {
      breakdown.page[[table]] <- cleanTable(breakdown.page[[table]])
    }
    
    #Build the empty dfs for the scraped data
    offense_df <- breakdown.page[[grep("offense", tables)[1]]][FALSE,]
    defense_df <- breakdown.page[[grep("defense", tables)[1]]][FALSE,]
    offense_count <- 0
    
    for(table in seq_along(tables)) {
      if (tables[[table]] == "offense") {
        #We only want the top 2 offense tables + 1-2 defense tables
        if (offense_count > 1) break
        offense_df <- rbind(offense_df, breakdown.page[[table]])
        offense_count <- offense_count + 1}
      if (tables[[table]] == "defense") {
        defense_df <- rbind(defense_df, breakdown.page[[table]])}
    }
    
    #Rename Offensive Columns
    names(offense_df) <- c("SLOT", "PLAYER, TEAM POS", "OPP", "STATUS ET", "", 
                   #Passing Stats
                   "PASS_YDS", "PASS_TD", "PASS_2PC", "PASS_INT", 
                   #Rushing Stats
                   "RUSH_YDS", "RUSH_TD", "RUSH_2PR", 
                   #Receiving Stats
                   "REC_YDS", "REC_TD", "REC_2PRE", 
                   #Misc
                   "REC", "FFTD", "FUML", 
                   #Kicking Stats
                   "FG50", "FG40", "FG0", "FGM", "PAT", 
                   #Special Teams Stats
                   "KTD", "PRTD", "FTD", "KR", "PR", 
                   #Punting Stats
                   "IN20", "PTA44", "PTA42", "PTA40", 
                   #Misc
                   "", "PTS" )
    
    #Combine Offense and Defense Stats
    final <- bind_rows(offense_df, defense_df[,c("SLOT", "PLAYER, TEAM POS", "OPP", "STATUS ET", "PTS")])
    
    #Add in Team, Week, and Season Data
    final <- add_column(final, TEAM_ID = teamId, WEEK = scoringPeriodId, SEASON = seasonId)
    
    #Check if the top level table exists, if not create
    if (exists("full_stats")) {
      full_stats <- rbind(full_stats, final)
    } else {
      full_stats <- final[FALSE,]
      full_stats <- rbind(full_stats, final)
    }
    
    Sys.sleep(runif(1, 5, 15))
    print(url)
  }
}

full_stats$POS <- NA

positions <- c("QB", "RB", "WR", "TE", "D/ST")

for (position in positions) {
  full_stats[grepl(position, full_stats$`PLAYER, TEAM POS`, ignore.case = FALSE) == TRUE, "POS"] <- gsub("[$]", "", position)
}

full_stats$PLAYER <- gsub("(.*),.*", "\\1", full_stats$`PLAYER, TEAM POS`)
full_stats$PLAYER <- str_replace_all(full_stats$PLAYER, "[^[:alnum:][:blank:]+./\\-]", "")
full_stats$PLAYER <- sub(" D/ST", "", full_stats$PLAYER)
full_stats <- full_stats[, !names(full_stats) %in% c("Var.5", "Var.28")]

#Add Team  Name
full_stats <- left_join(full_stats, team_lkup, by = c("TEAM_ID" = "TEAM_ID", "SEASON" = "SEASON"))

#Add in acqusition
acq.url = "http://games.espn.com/ffl/leaguerosters?leagueId=45342"

rosters <- htmlParse(acq.url)
rosters.page <- readHTMLTable(rosters, stringAsFactors = FALSE)

player.acq <- data.frame(SLOT = character(),
                         PLAYER = character(),
                         ACQ = character())

for (i in 3:14) {
  names(rosters.page[[i]]) <- names(player.acq)
  player.acq <- bind_rows(player.acq, rosters.page[[i]][-1,])
}

player.acq$PLAYER <- gsub("(.*),.*", "\\1", player.acq$PLAYER)
player.acq$PLAYER <- str_replace_all(player.acq$PLAYER, "[^[:alnum:][:blank:]+./\\-]", "")
player.acq$PLAYER <- sub(" D/ST", "", player.acq$PLAYER)

player.acq <- player.acq[,2:3]

full_stats <- left_join(full_stats, player.acq, by = c("PLAYER" = "PLAYER"))


#Rearrange Columns
final_stats <- full_stats[,c(
                       #Main
                       "SEASON", "WEEK", "TEAM_NAME", "TEAM_CODE", "PLAYER", "POS", "SLOT", "PTS", 
                       #MISC
                       "ACQ",  "PLAYER, TEAM POS", "OPP", "STATUS ET", 
                       #Passing Stats
                       "PASS_YDS", "PASS_TD", "PASS_2PC", "PASS_INT", 
                       #Rushing Stats
                       "RUSH_YDS", "RUSH_TD", "RUSH_2PR", 
                       #Receiving Stats
                       "REC_YDS", "REC_TD", "REC_2PRE", 
                       #Misc
                       "REC", "FFTD", "FUML", 
                       #Special Teams Stats
                       "KTD", "PRTD", "FTD", "KR", "PR")]

write.csv(final_stats, file = paste("~/data/boknows/", seasonId, "_player_stats.csv", sep = ""), 
          row.names=FALSE, 
          fileEncoding="Windows-1252")
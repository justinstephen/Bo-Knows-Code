library("XML")
library("dplyr")
library("svMisc")

#Parse teams and divisions:
division.page <- htmlParse("http://games.espn.com/ffl/standings?leagueId=45342&seasonId=2017")
division.page <- readHTMLTable(division.page,stringAsFactors = FALSE)

div.table <- as.data.frame(division.page[2])

divisions <- data.frame(division = 1, team = div.table[3:8,1])
divisions <- rbind(divisions, data.frame(division = 2, team = div.table[11:16,1]))
divisions$team <- as.character(divisions$team)

#Parse schedule and scores:
boknows <- htmlParse("http://games.espn.com/ffl/schedule?leagueId=45342")
boknows.table <- readHTMLTable(boknows,stringAsFactors = FALSE)
fullschedule <- boknows.table[2]
bo.df <- as.data.frame(fullschedule)
colnames(bo.df) <- c("AWAY_TEAM", "OWNER", "AT", "HOME_TEAM", "OWNER", "RESULT")

week1 <- bo.df[ 2:7,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week1$WEEK <- "week1"
week2 <- bo.df[ 11:16,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week2$WEEK <- "week2"
week3 <- bo.df[ 20:25,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week3$WEEK <- "week3"
week4 <- bo.df[ 29:34,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week4$WEEK <- "week4"
week5 <- bo.df[ 38:43,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week5$WEEK <- "week5"
week6 <- bo.df[ 47:52,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week6$WEEK <- "week6"
week7 <- bo.df[ 56:61,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week7$WEEK <- "week7"
week8 <- bo.df[ 65:70,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week8$WEEK <- "week8"
week9 <- bo.df[ 74:79,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week9$WEEK <- "week9"
week10 <- bo.df[ 83:88,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week10$WEEK <- "week10"
week11 <- bo.df[ 92:97,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week11$WEEK <- "week11"
week12 <- bo.df[ 101:106,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week12$WEEK <- "week12"
week13 <- bo.df[ 110:115,c("AWAY_TEAM", "HOME_TEAM", "RESULT")]
week13$WEEK <- "week13"
weeks <- list(week1=week1, week2=week2, week3=week3, week4=week4, week5=week5, week6=week6, week7=week7, week8=week8, week9=week9, 
              week10=week10, week11=week11, week12=week12, week13=week13)

#This is the section that will loop over and create a data frame for each week, then clean each up and merge them into one list
week.summary <- lapply(weeks, function(df)
{
  df$AWAY_SCORE <- gsub("-.*", "", df$RESULT)
  df$HOME_SCORE <- gsub(".*-", "", df$RESULT)
  df <- df[,c("AWAY_TEAM","AWAY_SCORE","HOME_TEAM","HOME_SCORE")]
  df$AWAY_TEAM <- gsub("\\s\\(.*", "", df$AWAY_TEAM)
  df$HOME_TEAM <- gsub("\\s\\(.*", "", df$HOME_TEAM)
  df.scores <- df
  colnames(df.scores) <- c("TEAM","RESULT","TEAM","RESULT")
  df.scores <- rbind(df.scores[1:2],df.scores[3:4])
})

#This loop will replace the generic "RESULT" from earlier with the column of the week name
for (week in 1:13) {
  names(week.summary[[week]]) <- c("TEAM", paste("WEEK", week, sep=""))
}

#This loop will take the list of data frames and turn it into one data frame with all the point totals for all teams
all.scores <- week.summary[[1]]
for (week in 2:13) {
  all.scores <- left_join(all.scores, week.summary[[week]], by = "TEAM")
}

#Strips off the named column and adds it as a data frame row name as well as converts all data to numeric
new.all.scores <- all.scores[,-1]
new.all.scores <- sapply(new.all.scores, as.numeric)
row.names(new.all.scores) <- all.scores$TEAM

#Creates the stats data frame to be used to simulate games
stats <- data.frame(Mean = rowMeans(new.all.scores, na.rm = TRUE), SD = apply(new.all.scores, 1, sd, na.rm=TRUE))
stats <- data.frame(team = row.names(stats), mean = stats$Mean, sd = stats$SD)

#This is the function that will simulate a score when given a team name based on the "stats" table
sim.score <- function(x) {
  x.mean <- stats[stats$team == x, "mean"]
  x.sd <- stats[stats$team == x, "sd"]
  score <- rnorm(1, mean = x.mean, sd = x.sd)
}

#Loops over the list of week dataframes to clean off unnecessary characters and format
schedule <- lapply(weeks, function(df)
{
  #df$WEEK <- as.character(deparse(substitute(df)))
  #df$WEEK <- paste("week", gsub("L\\]\\]", "",gsub("X\\[\\[","",df$WEEK)), sep = "")
  df$AWAY_SCORE <- gsub("-.*", "", df$RESULT)
  df$HOME_SCORE <- gsub(".*-", "", df$RESULT)
  df$AWAY_TEAM <- gsub("\\s\\(.*", "", df$AWAY_TEAM)
  df$HOME_TEAM <- gsub("\\s\\(.*", "", df$HOME_TEAM)
  df <- df[,c("WEEK","AWAY_TEAM","AWAY_SCORE","HOME_TEAM","HOME_SCORE")]
  df$AWAY_SCORE <- ifelse(df$AWAY_SCORE == "Preview", df$AWAY_SCORE <- NA, 
                          ifelse(df$AWAY_SCORE == "Box", df$AWAY_SCORE <- NA, as.numeric(df$AWAY_SCORE)))
  df$HOME_SCORE<- ifelse(df$HOME_SCORE == "Preview", df$HOME_SCORE <- NA, 
                         ifelse(df$HOME_SCORE == "Box", df$HOME_SCORE <- NA, as.numeric(df$HOME_SCORE)))
  df <- as.data.frame(df)
})

#Groups all the dataframes into one
schedule = Reduce(function(...) merge(..., all=T), schedule)

if (exists("full.standings")) {
  rm("full.standings")
}

###################################################
###                 BEGIN LOOP                 ####
###################################################

sim.games <- 10000
games = 0

while (games < sim.games) {
  games = games + 1
  progress(games, max.value = sim.games, progress.bar = TRUE)
  scoreboard <- schedule
  
  #This simulates a score to replace all the NA's
  for (i in 1:nrow(scoreboard)) {
    ifelse(is.na(scoreboard$AWAY_SCORE[i]), 
           scoreboard$AWAY_SCORE[i] <- sim.score(scoreboard$AWAY_TEAM[i]),
           scoreboard$AWAY_SCORE[i] <- scoreboard$AWAY_SCORE[i])
  }
  for (i in 1:nrow(scoreboard)) {
    ifelse(is.na(scoreboard$HOME_SCORE[i]), 
           scoreboard$HOME_SCORE[i] <- sim.score(scoreboard$HOME_TEAM[i]),
           scoreboard$HOME_SCORE[i] <- scoreboard$HOME_SCORE[i])
  }
  
  #This creates a new column with TRUE and FALSE for winners
  scoreboard$WINNERS <- scoreboard$AWAY_SCORE > scoreboard$HOME_SCORE
  
  #Changes the TRUE / FALSE values to the name of the team that won
  scoreboard[scoreboard$WINNERS == FALSE,]$WINNERS <- as.character(scoreboard[scoreboard$WINNERS == FALSE,]$HOME_TEAM)
  scoreboard[scoreboard$WINNERS == TRUE,]$WINNERS <- as.character(scoreboard[scoreboard$WINNERS == TRUE,]$AWAY_TEAM)
  wins.total <- as.data.frame(table(scoreboard$WINNERS))
  colnames(wins.total) <- c("team", "wins")
  
  #Build the standings DF.
  standings <- divisions
  standings <- merge(x = standings, y = wins.total, by = "team", all.x=TRUE)
  
  #Populate a table of total points of all teams and then append it to the standings table
  # points <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  # team <- all.scores$TEAM
  # total.points <- data.frame(team,points)
  
  #Something might be  wrong here
  # for (i in team) {
  #   total.points[team == i,]$points <- sum(scoreboard[scoreboard$AWAY_TEAM == i,]$AWAY_SCORE) + sum(scoreboard[scoreboard$HOME_TEAM == i,]$AWAY_SCORE)
  # }
  away.scores <- scoreboard[,2:3]
  home.scores <- scoreboard[,4:5]
  names(home.scores) <- names(away.scores)
  
  stacked.scoreboard <- rbind(home.scores, away.scores)
  
  total.points <- stacked.scoreboard %>%
    group_by(team = AWAY_TEAM) %>%
    summarize(points = sum(AWAY_SCORE))
  
  standings <- merge(x = standings, y = total.points, by = "team", all.x=TRUE)
  
  #Clean up the standings tables
  standings[is.na(standings)] <- 0
  standings<- standings[with(standings, order(wins, points, decreasing=TRUE)),]
  
  standings$seed <- 0
  
  ##1 Seed Division Winner 
  standings$seed[1] <- 1
  
  ##2 Seed Division Winner
  standings[standings$seed == 0 & standings$division != standings[standings$seed == 1, "division"], "seed"][1] <- 2
  
  ##3 Seed Division Runner Up
  standings[standings$seed == 0, "seed"][1] <- 3
  
  ##4 Seed Division Runner Up
  standings[standings$seed == 0 & standings$division != standings[standings$seed == 3, "division"], "seed"][1] <- 4
  
  ##5 Wild Card 1
  standings[standings$seed == 0, "seed"][1] <- 5
  
  ##6 Wild Card 2
  standings[standings$seed == 0, "seed"][1] <- 6
  
  if (exists("full.standings")) {
    full.standings <- rbind(full.standings, standings)
  } else {
    full.standings <- standings[FALSE,]
    full.standings <- rbind(full.standings, standings)
  }
}

###################################################
###                  END LOOP                  ####
###################################################

po.odds <- full.standings %>%
  group_by(team) %>%
  summarize(avg = mean(points),
            wins = mean(wins),
            one_seed = sum(seed[seed == 1]),
            two_seed = sum(seed[seed == 2]) / 2,
            three_seed = sum(seed[seed == 3]) / 3,
            four_seed = sum(seed[seed == 4]) / 4,
            five_seed = sum(seed[seed == 5]) / 5,
            six_seed = sum(seed[seed == 6]) / 6) %>%
  mutate(total_odds = rowSums(.[4:9]))

po.odds[,4:10] <- po.odds[,4:10] / games * 100

po.odds <- po.odds[with(po.odds, order(total_odds, one_seed, two_seed, three_seed, 
                                       four_seed, five_seed, six_seed, decreasing = TRUE)),]

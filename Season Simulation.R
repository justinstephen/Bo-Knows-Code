library("XML")
library("plyr")


#Pull all data from ESPN and clean up
boknows <- htmlParse("http://games.espn.go.com/ffl/schedule?leagueId=45342")
boknows.table <- readHTMLTable(boknows,stringAsFactors = FALSE)
fullschedule <- boknows.table[2]
bo.df <- ldply(fullschedule, data.frame)
colnames(bo.df) <- c("NULL","AWAY_TEAM", "OWNER", "AT", "HOME_TEAM", "OWNER", "RESULT")

week1 <- bo.df[ 2:7,c(2, 5, 7)]
week2 <- bo.df[ 11:16,c(2, 5, 7)]
week3 <- bo.df[ 20:25,c(2, 5, 7)]
week4 <- bo.df[ 29:34,c(2, 5, 7)]
week5 <- bo.df[ 38:43,c(2, 5, 7)]
week6 <- bo.df[ 47:52,c(2, 5, 7)]
week7 <- bo.df[ 56:61,c(2, 5, 7)]
week8 <- bo.df[ 65:70,c(2, 5, 7)]
week9 <- bo.df[ 74:79,c(2, 5, 7)]
week10 <- bo.df[ 83:88,c(2, 5, 7)]
week11 <- bo.df[ 92:97,c(2, 5, 7)]
week12 <- bo.df[ 101:106,c(2, 5, 7)]
week13 <- bo.df[ 110:115,c(2, 5, 7)]
week14 <- bo.df[ 119:124,c(2, 5, 7)]
weeks <- list(week1=week1, week2=week2, week3=week3, week4=week4, week5=week5, week6=week6, week7=week7, week8=week8, week9=week9, 
              week10=week10, week11=week11, week12=week12, week13=week13, week14=week14)

#This is the section that will loop over and create a data frame for each week, then clean each up and merge them into one list
week.summary <- lapply(weeks, function(df)
{
  df$AWAY_SCORE <- gsub("-.*", "", df$RESULT)
  df$HOME_SCORE <- gsub(".*-", "", df$RESULT)
  df <- df[,c("AWAY_TEAM","AWAY_SCORE","HOME_TEAM","HOME_SCORE")]
  df$AWAY_TEAM <- gsub("\\(.*", "", df$AWAY_TEAM)
  df$HOME_TEAM <- gsub("\\(.*", "", df$HOME_TEAM)
  df.scores <- df
  colnames(df.scores) <- c("TEAM","RESULT","TEAM","RESULT")
  df.scores <- rbind(df.scores[1:2],df.scores[3:4])
})

#This loop will replace the generic "RESULT" from earlier with the column of the week name
for (week in 1:14) {
  names(week.summary[[week]]) <- c("TEAM", paste("WEEK", week, sep=""))
}

#This loop will take the list of data frames and turn it into one data frame with all the point totals for all teams
all.scores <- week.summary[[1]]
for (week in 2:14) {
  all.scores <- join(all.scores, week.summary[[week]], by = "TEAM")
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
  df$WEEK <- as.character(deparse(substitute(df)))
  df$WEEK <- paste("week", gsub("L\\]\\]", "",gsub("X\\[\\[","",df$WEEK)), sep = "")
  df$AWAY_SCORE <- gsub("-.*", "", df$RESULT)
  df$HOME_SCORE <- gsub(".*-", "", df$RESULT)
  df$AWAY_TEAM <- gsub("\\(.*", "", df$AWAY_TEAM)
  df$HOME_TEAM <- gsub("\\(.*", "", df$HOME_TEAM)
  df <- df[,c("WEEK","AWAY_TEAM","AWAY_SCORE","HOME_TEAM","HOME_SCORE")]
  df$AWAY_SCORE <- ifelse(df$AWAY_SCORE == "Preview", df$AWAY_SCORE <- NA, 
                          ifelse(df$AWAY_SCORE == "Box", df$AWAY_SCORE <- NA, as.numeric(df$AWAY_SCORE)))
  df$HOME_SCORE<- ifelse(df$HOME_SCORE == "Preview", df$HOME_SCORE <- NA, 
                         ifelse(df$HOME_SCORE == "Box", df$HOME_SCORE <- NA, as.numeric(df$HOME_SCORE)))
  df <- as.data.frame(df)
})

#Groups all the dataframes into one
schedule = Reduce(function(...) merge(..., all=T), schedule)

#This initializes the list of teams that will make playoffs and will be OUTSIDE the loop
playoff.teams.total <- character()


###################################################
###                 BEGIN LOOP                 ####
###################################################

games = 0

while (games < 100000) {
  games = games + 1
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
  
  #Build the standings DF. This needs to be cleaned up later to built the team list automatically
  team <- all.scores$TEAM
  division <- c(1,2,2,1,2,1,1,2,2,2,1,1)
  standings <- data.frame(division, team)
  standings <- merge(x = standings, y = wins.total, by = "team", all.x=TRUE)
  
  #Populate a table of total points of all teams and then append it to the standings table
  points <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  total.points <- data.frame(team,points)
  for (i in team) {
    total.points[team == i,]$points <- sum(scoreboard[scoreboard$AWAY_TEAM == i,]$AWAY_SCORE) + sum(scoreboard[scoreboard$HOME_TEAM == i,]$AWAY_SCORE)
  }
  standings <- merge(x = standings, y = total.points, by = "team", all.x=TRUE)
  
  #Clean up the standings tables
  standings[is.na(standings)] <- 0
  standings<- standings[with(standings, order(wins, points, decreasing=TRUE)),]
  
  #This will determine the two division winners
  div1.winner <- as.character(standings[standings$division ==1, "team"][1])
  div2.winner <- as.character(standings[standings$division ==2, "team"][1])
  
  #This will determine the wildcard teams
  wildcard <- subset(standings, team != div1.winner & team != div2.winner)
  wc1 <- as.character(wildcard[1,"team"])
  wc2 <- as.character(wildcard[2,"team"])
  
  #List of playoff teams
  playoff.teams <- c(div1.winner, div2.winner, wc1, wc2)
  playoff.teams.total <- append(playoff.teams.total, playoff.teams)
}
###################################################
###                  END LOOP                  ####
###################################################

#Puts data into a data.frame and formats
po.freq <- as.data.frame(table(playoff.teams.total))
colnames(po.freq) <- c("team", "count")
po.freq <- po.freq[with(po.freq, order(po.freq$count, decreasing = TRUE)),]
po.freq

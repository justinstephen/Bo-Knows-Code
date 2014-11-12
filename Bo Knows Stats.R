library("XML")
library("plyr")

boknows <- htmlParse("http://games.espn.go.com/ffl/schedule?leagueId=45342")
boknows.table <- readHTMLTable(boknows,stringAsFactors = FALSE)
fullschedule <- boknows.table[2]
bo.df <- ldply(fullschedule, data.frame)
colnames(bo.df) <- c("NULL","AWAY_TEAM", "OWNER", "AT", "HOME_TEAM", "OWNER", "RESULT")

week1 <- bo.df[ 2:6,c(2, 5, 7)]
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
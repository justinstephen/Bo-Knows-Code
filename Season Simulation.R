library(tidyverse)
library(httr)
library(jsonlite)

#This is the function that will simulate a score when given a team name based on the "stats" table
# sim.score <- function(x) {
#   x.mean <- as.numeric(stats[stats$TEAM == x, "mean"])
#   x.sd <- as.numeric(stats[stats$TEAM == x, "sd"]) + 25
#   score <- rnorm(1, mean = x.mean, sd = x.sd) #+ rnorm(1, mean = 0, sd = 25)
# }

sim.score <- function(x) {
  #Team mean and sd
  x.mean <- mean(unlist(scores %>% filter(TEAM == x, !is.na(SCORE)) %>% select(SCORE)))
  x.sd <- sd(unlist(scores %>% filter(TEAM == x, !is.na(SCORE)) %>% select(SCORE)))
  
  #League mean and sd
  league.mean <- mean(unlist(scores %>% filter(!is.na(SCORE)) %>% select(SCORE)))
  #league.sd <- sd(unlist(scores %>% filter(!is.na(SCORE)) %>% select(SCORE))) 
  
  new.mean <- x.mean * .75 + league.mean * .25
  
  #Generate random score 
  score <- rnorm(1, new.mean, x.sd)
}

################################
#Go get all the Division Data
################################
path <- "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/45342"

request <- GET(url = path,
               query = list(
                 view = "standings"
               ))

request$status_code

response <- content(request, as = "text", encoding = "UTF-8")

page <- fromJSON(response, flatten = TRUE)

divisions <- bind_cols(TeamCode = page$teams$abbrev,
                       TeamName = paste(page$teams$location, page$teams$nickname),
                       Division = page$teams$divisionId,
                       Wins = page$teams$record.overall.wins,
                       PointsFor = page$teams$record.overall.pointsFor)

divisions <- divisions %>% 
  arrange(Division, desc(Wins), desc(PointsFor)) %>%
  mutate(DivRank = c(1:6, 1:6))

################################
#Get scores and create agg tables
################################

scores <- as.data.frame(read.csv("~/data/boknows/full_schedule.csv", header = TRUE, na.strings = NA))

#Create agg table
stats <- scores %>%
  filter(!is.na(WIN)) %>%
  group_by(TEAM) %>%
  summarize(mean = mean(SCORE),
            sd = sd(SCORE))

################################
#Beginning of the Simulator
################################
#Drop table if re-running
if (exists("full.standings")) {
  rm("full.standings")
}

###################################################
###                 BEGIN LOOP                 ####
###################################################
#Variables for more/less looping
sim.games <- 1000
games = 0
progress <- progress_estimated(sim.games)

#Fire up the loop
while (games < sim.games) {
  games = games + 1
  progress$tick()$print()
  scoreboard <- scores
  
  #This simulates a score to replace all the NA's
  for (i in 1:nrow(scoreboard)) {
    ifelse(is.na(scoreboard$SCORE[i]), 
           scoreboard$SCORE[i] <- sim.score(scoreboard$TEAM[i]),
           scoreboard$SCORE[i] <- scoreboard$SCORE[i])
  }
  
  scoreboard$OPPO_SCORE <- scoreboard %>% inner_join(select(scoreboard, WEEK, OPPONENT, SCORE), by = c("WEEK" = "WEEK", "TEAM" = "OPPONENT")) %>% select(SCORE.y) 
  
  #This creates a new column with TRUE and FALSE for winners
  scoreboard$WIN <- ifelse(scoreboard$SCORE > scoreboard$OPPO_SCORE, 1, 0)
  
  #Count Wins/Losses and total points
  wins.total <- scoreboard %>%
    group_by(team = TEAM) %>%
    summarize(wins = sum(WIN),
              points = sum(SCORE))
  
  #Build the standings DF.
  standings <- divisions
  standings <- merge(x = standings, y = wins.total, by.x = "TeamCode", by.y = "team", all.x=TRUE)
  standings <- standings[with(standings, order(wins, points, decreasing=TRUE)),]
  
  standings$seed <- 0
  
  ##1 Seed Division Winner 
  standings$seed[1] <- 1
  
  ##2 Seed Division Winner
  standings[standings$seed == 0 & standings$Division != standings[standings$seed == 1, "Division"], "seed"][1] <- 2
  
  ##3 Seed Division Runner Up
  standings[standings$seed == 0, "seed"][1] <- 3
  
  ##4 Seed Division Runner Up
  standings[standings$seed == 0 & standings$Division != standings[standings$seed == 3, "Division"], "seed"][1] <- 4
  
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
  group_by(TeamCode) %>%
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

#Optional: Write Playoffs CSV
#po.odds.file <- po.odds[,c("team", "total_odds")]

#write.csv(po.odds.file, file = "~/data/boknows/playoff.csv", row.names=FALSE)

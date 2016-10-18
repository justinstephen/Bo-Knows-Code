library(dplyr)
library(reshape2)

scores <- as.data.frame(read.csv("allscores.csv", header = TRUE))

merged.scores <- left_join(scores, scores, by = c("Opponent" = "Team", "Season" = "Season", "Week" = "Week"))
merged.scores <- na.omit(merged.scores[1:7])

merged.scores$Win.x <- ifelse(merged.scores$Score.x > merged.scores$Score.y, 1, 0)

opp.record <- merged.scores %>%
                group_by(Team, Opponent) %>%
                summarize(Wins = sum(Win.x),
                          Games = n()) 

records <- merged.scores %>%
  group_by(Team, Opponent) %>%
  summarize(Record = paste(sum(Win.x), "-", n()-sum(Win.x))) 

record.matrix <- acast(records, Team~Opponent, value.var="Record")

total.record <- merged.scores %>%
                  group_by(Team) %>%
                  summarize(Record = paste(sum(Win.x), "-", n()-sum(Win.x)),
                            Win.Per = paste(formatC(sum(Win.x)/n() * 100), "%"),
                            Avg.Points.For = formatC(mean(Score.x)),
                            Avg.Points.Against = formatC(mean(Score.y)))

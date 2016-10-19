library(googlesheets)
library(reshape2)
library(dplyr)

sheet <- gs_key("18ha8LKIP1GM_m9drhEVTVlS7-8iK62X6xlIzD092MiU")
scores <- as.data.frame(sheet %>% gs_read(ws = "Scores"))

#scores <- as.data.frame(read.csv("allscores.csv", header = TRUE))

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


##Power Rankings for 2016 Season
ranks <- as.data.frame(read.csv("boranks/data/pwrranking.csv", header = TRUE))

ggplot(ranks, aes(y = Rank, x = Week, color = as.factor(Team))) + 
  geom_line() +
  scale_y_reverse(lim=c(12,1), breaks = 1:12) +
  scale_x_continuous(breaks = 1:max(ranks$Week))

plot_ly(ranks, x = ~Week, y = ~Rank, color = ~Team) %>%
  add_lines() %>%
  layout(yaxis = list(autorange = "reversed"))

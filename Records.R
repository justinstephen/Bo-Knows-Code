library(googlesheets)
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)

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

margin.wins <- merged.scores[merged.scores$Win.x == 1,] %>%
  group_by(Team, Season) %>%
  summarize(Margin = mean(abs(Score.x - Score.y))) %>%
  arrange(Margin)

margin.losses <- merged.scores[merged.scores$Win.x == 0,] %>%
  group_by(Team, Season) %>%
  summarize(Margin = mean(abs(Score.x - Score.y))) %>%
  arrange(Margin)

win.margin.plot <- ggplot(margin.wins[margin.wins$Season == 2016,], aes(x = reorder(Team, Margin), y = Margin)) + 
  geom_bar(stat="identity")

loss.margin.plot <- ggplot(margin.losses[margin.losses$Season == 2016,], aes(x = reorder(Team, Margin), y = Margin)) + 
  geom_bar(stat="identity")

##Power Rankings for 2016 Season
ranks <- as.data.frame(read.csv("boranks/data/pwrranking.csv", header = TRUE))

ggplot(ranks, aes(y = Rank, x = Week, color = as.factor(Team))) + 
  geom_line() +
  scale_y_reverse(lim=c(12,1), breaks = 1:12) +
  scale_x_continuous(breaks = 1:max(ranks$Week))

plot_ly(ranks, x = ~Week, y = ~Rank, color = ~Team) %>%
  add_lines() %>%
  layout(yaxis = list(autorange = "reversed"))

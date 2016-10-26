library(ggplot2)
library(ggthemes)
library(gridExtra)
library(directlabels)

#plot_ly(ranks, x = ~Week, y = ~Rank, color = ~Team) %>%
#  add_lines() %>%
#  layout(yaxis = list(autorange = "reversed"))

ranks <- as.data.frame(read.csv("pwrranking.csv", header = TRUE))

top4 <- ranks$Team[ranks$Rank < 5 & ranks$Week == max(ranks$Week)]
mid4 <- ranks$Team[ranks$Rank > 4 & ranks$Rank < 9 & ranks$Week == max(ranks$Week)]
bot4 <- ranks$Team[ranks$Rank > 8 & ranks$Week == max(ranks$Week)]

top4.plot <- ggplot(ranks[ranks$Team %in% top4,], aes(y = Rank, x = Week, color = Team)) + 
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_reverse(lim=c(12,1), breaks = 1:12) +
  scale_x_continuous(breaks = 1:max(ranks$Week), expand = c(0,1)) +
  geom_hline(yintercept=seq(.5, 11, by=4), linetype="dashed") +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  theme_minimal() +
  scale_colour_tableau("colorblind10") +
  theme(legend.position="none",
        axis.title.x=element_blank())

mid4.plot <- ggplot(ranks[ranks$Team %in% mid4,], aes(y = Rank, x = Week, color = Team)) + 
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_reverse(lim=c(12,1), breaks = 1:12) +
  scale_x_continuous(breaks = 1:max(ranks$Week), expand = c(0,1)) +
  geom_hline(yintercept=seq(.5, 11, by=4), linetype="dashed") +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  theme_minimal() +
  scale_colour_tableau("colorblind10") +
  theme(legend.position="none",
        axis.title.x=element_blank())

bot4.plot <- ggplot(ranks[ranks$Team %in% bot4,], aes(y = Rank, x = Week, color = Team)) + 
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_reverse(lim=c(12,1), breaks = 1:12) +
  scale_x_continuous(breaks = 1:max(ranks$Week), expand = c(0,1)) +
  geom_hline(yintercept=seq(.5, 11, by=4), linetype="dashed") +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Team, fontface = "bold"), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  theme_minimal() +
  scale_colour_tableau("colorblind10") +
  theme(legend.position="none")

grid.arrange(top4.plot, mid4.plot, bot4.plot, nrow=3)
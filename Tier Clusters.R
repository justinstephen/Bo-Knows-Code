## Load Libraries
library(ggplot2)
library(dplyr)

## Load and clean Data
scores <- as.data.frame(read.csv("allscores.csv", header = TRUE))
scores <- scores[complete.cases(scores),]

#Aggregate Data
current.season <- scores[scores$Season == 2016,] %>%
  group_by(Team) %>%
  summarize(Average = mean(Score),
            Wins = sum(Win))

#########################################
######## Tiered Clustering###############
#########################################

##Scale Wins and Points on a 0 to 1 scale
adj.season <- apply(current.season[,2:3], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

##Create Clusters
scoreCluster <- kmeans(adj.season, 4, nstart = 10000)

##Add clusters back to dataframe
current.season$tier <- scoreCluster$cluster

##Scatter Plot
##Save Size 600x350
ggplot(current.season, aes(x = Wins, y = Average, label = Team)) +
  geom_label(aes(fill = as.factor(tier)), colour = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 1:max(current.season$Wins)) +
  theme(legend.position="none", panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(1.75, max(current.season$Wins)+.25))

##Bar Chart
ggplot(current.season, aes(reorder(Team, -Average), y = Average)) +
  geom_bar(stat="identity", aes(fill = as.factor(tier))) +
  geom_text(aes(label = Average), hjust = -.1, vjust = .4, angle = 270) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = c(60, 70, 80, 90, 100, 110, 120, 130, 140), 
                     minor_breaks = c(88, 92, 94, 96, 98, 102, 104, 106, 108)) +
  theme(legend.position="none", axis.text.x = element_text(angle = 270, hjust = 0, vjust = .4),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor = element_line(linetype = "longdash", size = .5)) +
  labs(list(x = "Team", y = "Average Score")) +
  coord_cartesian(ylim=c(70,120))

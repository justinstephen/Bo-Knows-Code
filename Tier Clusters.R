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
######## Tiered Clusting#################
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
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  theme(legend.position="none", panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(1.75, 6.25))

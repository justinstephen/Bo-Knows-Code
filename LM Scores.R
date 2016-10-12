library(GGally)
library(MASS)

scores <- as.data.frame(read.csv("scores2.csv", header = TRUE))
scores <- scores[,2:ncol(scores)]

ggpairs(scores, alpha = 0.4)

initial.fit <- lm(Score~.,scores)
final.fit <- stepAIC(initial.fit)
par(mfrow=c(2,2))
plot(initial.fit)
plot(final.fit)
summary(final.fit)

modified.fit <- lm(Score ~ X4WeekAvg + Max + Min, scores)

test.scores <- scores[c(4, 5, 6)]
scores$prediction <- predict(modified.fit, newdata = test.scores)

plot(scores$Score, scores$prediction)
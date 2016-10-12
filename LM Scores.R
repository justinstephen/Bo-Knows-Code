library(GGally)
library(MASS)

scores <- as.data.frame(read.csv("scores.csv", header = TRUE))
scores <- scores[,2:ncol(scores)]

M <- cor(scores)
corrplot(M, method="number")
ggpairs(scores, alpha = 0.4)

initial.fit <- lm(Score~.,scores)
final.fit <- stepAIC(initial.fit)
par(mfrow=c(2,2))
plot(initial.fit)
plot(final.fit)
summary(final.fit)
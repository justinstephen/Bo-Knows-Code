library(corrplot)
library(MASS)

scores <- as.data.frame(read.csv("scores.csv", header = TRUE))
scores <- scores[,2:ncol(scores)]

M <- cor(scores)
corrplot(M, method="number")

pairs(scores)

initial.fit <- lm(Score~.,scores)
final.fit <- stepAIC(initial.fit)
par(mfrow=c(2,2))
plot(initial.fit)
plot(final.fit)
summary(final.fit)

test.scores <- scores[c(3, 4, 5, 6)]
scores$prediction <- predict(final.fit, newdata = test.scores)
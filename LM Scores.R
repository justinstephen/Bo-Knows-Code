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

library(caret)

#Reload the dataset
scores <- as.data.frame(read.csv("scores2.csv", header = TRUE))
scores <- scores[,2:ncol(scores)]

#Train a new linear model with all features
caret.fit <- train(Score~., data = scores, method = "lm")

#Plot importance of each feature
importance <- varImp(caret.fit, scale = FALSE)
plot(importance)

#Check out some feature selection
set.seed(10)
subsets <- c(1:5, 10, 15, 20, 25)

x <- scores[1:ncol(scores)-1]
y <- scores[ncol(scores)][,1]

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

trellis.par.set(caretTheme())

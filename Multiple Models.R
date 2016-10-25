library(mlbench)
library(caret)

set.seed(1234)

data <- as.data.frame(read.csv("scores3.csv", header = TRUE))
data$Win <- as.factor(data$Win)

control <- trainControl(method="repeatedcv", number=10, repeats=3)

#Models
modelSVML <- train(Win~., data=data, method='svmLinearWeights', trControl=control)
modelGbm <- train(Win~., data=data, method="gbm", trControl=control, verbose=FALSE)
#modelbinda <- train(Win~., data=data, method="binda", trControl=control)
#modellogreg <- train(Win~., data=data, method="logreg", trControl=control)


# collect resamples
results <- resamples(list("SVML" = modelSVML, "GBM" = modelGbm))

summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

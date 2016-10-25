library(dplyr)

all.scores <- as.data.frame(read.csv("allscores.csv", header = TRUE))

avg.scores <- as.data.frame(read.csv("scores2.csv", header = TRUE))
avg.scores <- avg.scores[,1:ncol(avg.scores)-1]

#Create key for all.scores
all.scores$ID <- with(all.scores, paste(Team, "-", Season, "-", Week, sep=""))

combined.data <- left_join(avg.scores, all.scores, by = c("ID" = "ID"))

combined.data <- combined.data[c(5, 6, 7, 13)]

model <- glm(Win ~., family = binomial, data = combined.data)

combined.data$pred <- predict(model, newdata = combined.data, type = "response")


#Now try model with Power Rankings
power <- as.data.frame(read.csv("scores3.csv", header = TRUE))

pwr.model <- glm(Win ~., family = binomial, data = data)
power$pred <- predict(pwr.model, newdata = power, type = "response")

# Caret Models
library(caret)

power$Win <- ifelse(power$Win == 1, 'Win', 'Loss')
power$Win <- as.factor(power$Win)

predictorsNames <- names(power)[names(power) != "Win"]
outcomeName <- "Win"

set.seed(1234)
splitIndex <- createDataPartition(power[,"Win"], p = .75, list = FALSE, times = 1)
trainDF <- power[ splitIndex,]
testDF  <- power[-splitIndex,]

objControl <- trainControl(method='cv', number=100, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(trainDF[,predictorsNames], trainDF[,"Win"], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)

predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
print(postResample(pred=predictions, obs=as.factor(testDF[,"Win"])))

library(pROC)
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')

auc <- roc(ifelse(testDF[,"Win"]=="Win",1,0), predictions[[2]])
print(auc$auc)

#Glmnet Modeling
set.seed(1234)
splitIndex <- createDataPartition(power[,"Win"], p = .75, list = FALSE, times = 1)
trainDF <- power[ splitIndex,]
testDF  <- power[-splitIndex,]

objControl <- trainControl(method='cv', number=100, returnResamp='none')
objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], method='glmnet',  metric = "RMSE", trControl=objControl)

predictions <- predict(object=objModel, testDF[,predictorsNames])
auc <- roc(testDF[,outcomeName], predictions)
print(auc$auc)

plot(varImp(objModel,scale=F))
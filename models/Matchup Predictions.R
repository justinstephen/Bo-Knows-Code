##Predict Weekly Matchups

#Load Packages
library(zoo)
library(GGally)
library(dplyr)
library(caret)

#Scaling function
cust.scale <- function(x) {(x - min(x)) / (max(x) - min(x))}

#Read data sets
scores <- as.data.frame(read.csv("2016.csv", header = TRUE))
scores2 <- as.data.frame(read.csv("2015.csv", header = TRUE))

#Add Key for joins
scores$Key <- with(scores, paste(Team, "-", Season, "-", Week, sep = ""))
scores2$Key <- with(scores2, paste(Team, "-", Season, "-", Week, sep = ""))

#Set target score
scores$Target.Win[scores$Week < max(scores$Week)] <- scores$Win[scores$Week > 1]
scores2$Target.Win[scores2$Week < max(scores2$Week)] <- scores2$Win[scores2$Week > 1]

wins <- scores$Target.Win
wins2 <- scores2$Target.Win

#Build team list for loop
team.list <- unique(scores$Team)
team.list2 <- unique(scores2$Team)

#Initialize dataframes for different time periods
three.weeks <- scores
four.weeks <- scores

three.weeks2 <- scores2
four.weeks2 <- scores2


#Build Features
##2016
for(team in team.list) {
  #Three week features
  three.weeks$Mean[three.weeks$Team == team & three.weeks$Week > 2] <- rollmean(three.weeks$Score[three.weeks$Team == team], 3)
  three.weeks$Max[three.weeks$Team == team & three.weeks$Week > 2] <- rollmax(three.weeks$Score[three.weeks$Team == team], 3)
  three.weeks$Min[three.weeks$Team == team & three.weeks$Week > 2] <- rollapply(three.weeks$Score[three.weeks$Team == team], 3, min)
  
  #Four week features
  four.weeks$Mean[four.weeks$Team == team & four.weeks$Week > 3] <- rollmean(four.weeks$Score[four.weeks$Team == team], 4)
  four.weeks$Max[four.weeks$Team == team & four.weeks$Week > 3] <- rollmax(four.weeks$Score[four.weeks$Team == team], 4)
  four.weeks$Min[four.weeks$Team == team & four.weeks$Week > 3] <- rollapply(four.weeks$Score[four.weeks$Team == team], 4, min)
}

three.weeks <- three.weeks[complete.cases(three.weeks),]
four.weeks <- four.weeks[complete.cases(four.weeks),]

three.weeks.scaled <- as.data.frame(apply(three.weeks[,c("Mean", "Max", "Min", "Rank")], 2, cust.scale))
four.weeks.scaled <- as.data.frame(apply(four.weeks[,c("Mean", "Max", "Min", "Rank")], 2, cust.scale))

##2015
for(team in team.list2) {
  #Three week features
  three.weeks2$Mean[three.weeks2$Team == team & three.weeks2$Week > 2] <- rollmean(three.weeks2$Score[three.weeks2$Team == team], 3)
  three.weeks2$Max[three.weeks2$Team == team & three.weeks2$Week > 2] <- rollmax(three.weeks2$Score[three.weeks2$Team == team], 3)
  three.weeks2$Min[three.weeks2$Team == team & three.weeks2$Week > 2] <- rollapply(three.weeks2$Score[three.weeks2$Team == team], 3, min)
  
  #Four week features
  four.weeks2$Mean[four.weeks2$Team == team & four.weeks2$Week > 3] <- rollmean(four.weeks2$Score[four.weeks2$Team == team], 4)
  four.weeks2$Max[four.weeks2$Team == team & four.weeks2$Week > 3] <- rollmax(four.weeks2$Score[four.weeks2$Team == team], 4)
  four.weeks2$Min[four.weeks2$Team == team & four.weeks2$Week > 3] <- rollapply(four.weeks2$Score[four.weeks2$Team == team], 4, min)
}

three.weeks2 <- three.weeks2[complete.cases(three.weeks2),]
four.weeks2 <- four.weeks2[complete.cases(four.weeks2),]

three.weeks2.scaled <- as.data.frame(apply(three.weeks2[,c("Mean", "Max", "Min", "Rank")], 2, cust.scale))
four.weeks2.scaled <- as.data.frame(apply(four.weeks2[,c("Mean", "Max", "Min", "Rank")], 2, cust.scale))


three.weeks <- cbind(three.weeks[,c("Key", "Target.Win")], three.weeks.scaled)
four.weeks <- cbind(four.weeks[,c("Key", "Target.Win")], four.weeks.scaled)
three.weeks2 <- cbind(three.weeks2[,c("Key", "Target.Win")], three.weeks2.scaled)
four.weeks2 <- cbind(four.weeks2[,c("Key", "Target.Win")], four.weeks2.scaled)


three.weeks.final <- as.data.frame(rbind(three.weeks, three.weeks2))
four.weeks.final <- rbind(four.weeks, four.weeks2)

#Build model to compute team score
three.weeks.final$Target.Win <- ifelse(three.weeks.final$Target.Win == 1, 'Win', 'Loss')
three.weeks.final$Target.Win <- as.factor(three.weeks.final$Target.Win)

#Build model to predict win
predictorsNames <- names(three.weeks.final)[3:6]
outcomeName <- "Target.Win"

set.seed(1234)
splitIndex <- createDataPartition(three.weeks.final[,"Target.Win"], p = .75, list = FALSE, times = 1)
trainDF <- three.weeks.final[ splitIndex, 2:ncol(three.weeks.final)]
testDF  <- three.weeks.final[-splitIndex, 2:ncol(three.weeks.final)]

objControl <- trainControl(method='cv', number=10, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)

predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
print(postResample(pred=predictions, obs=as.factor(testDF[,"Target.Win"])))





#Predict Wins
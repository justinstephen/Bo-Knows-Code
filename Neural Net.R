set.seed(500)

#Read in data
power <- as.data.frame(read.csv("pwr.csv", header = TRUE))

#Split data into training and test sets
index <- sample(1:nrow(power),round(0.75*nrow(power)))
train <- power[index,]
test <- power[-index,]

#Fit linear model for comparison
lm.fit <- glm(Win~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Win)^2)/nrow(test)

#Fit neural net
library(neuralnet)
n <- names(train)

f <- as.formula(paste("Win ~", paste(n[!n %in% "Win"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=F)

pr.nn <- compute(nn,test[,2:ncol(test)])

pr.nn_ <- pr.nn$net.result*(max(power$Win)-min(power$Win))+min(power$Win)
test.r <- (test$Win)*(max(power$Win)-min(power$Win))+min(power$Win)

#Compare MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

#Cross Validation
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(power),round(0.9*nrow(power)))
  train.cv <- power[index,]
  test.cv <- power[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)

  pr.nn <- compute(nn,test.cv[,2:ncol(test.cv)])
  pr.nn_ <- pr.nn$net.result*(max(power$Win)-min(power$Win))+min(power$Win)
  
  test.cv.r <- (test.cv$Win)*(max(power$Win)-min(power$Win))+min(power$Win)
  
  cv.error[i] <- sum((test.cv.r - pr.nn_)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
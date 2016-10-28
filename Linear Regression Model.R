library(dplyr)

#Define what time period you want to use for rolling stats:
period <- 7

#Linear Regression to predict scores
all.scores <- as.data.frame(read.csv("allscores.csv", header = TRUE))

#Break each season into it's own dataframe so we can compute rolling averages
seasons <- unique(all.scores$Season)

all.seasons <- list()

for(s in seasons) {
  df = all.scores[all.scores$Season == s,]
  df <- assign(paste("season", s, sep=""), df)
  all.seasons <- c(all.seasons, paste("season", s, sep=""))
}

#Function to add new factors for rolling stats
roll.stats <- function(df, period) {
  df$Target[df$Week < max(df$Week)] <- df$Score[df$Week > 1]
  
  team.list = unique(df$Team)
  
  for(team in team.list) {
    df$Mean[df$Team == team & df$Week > (period - 1)] <- rollmean(df$Score[df$Team == team], period)
    df$Max[df$Team == team & df$Week > (period - 1)] <- rollmax(df$Score[df$Team == team], period)
    df$Min[df$Team == team & df$Week > (period - 1)] <- rollapply(df$Score[df$Team == team], period, min)
  }
  
  df <- df[complete.cases(df),c("Target", "Mean", "Max", "Min")]
  return(df)
}

new.scores <- data.frame(Target=numeric(),
                         Mean=numeric(), 
                         Max=numeric(), 
                         Min=numeric(), 
                         stringsAsFactors=FALSE)

for(s in all.seasons) {
  df = get(s)
  new.scores <- rbind(new.scores, roll.stats(df, period))
}


#########################
### Modeling ############
#########################
library(caret)

ctrl <- trainControl(method = "cv", number = 50, repeats = 100)

model <- train(Target~.
               , data = new.scores
               , method = 'lm'
               , trControl = ctrl
               , metric ="Rsquared"
               , maximize= FALSE)

#Model Evaluation:
#model$results 
predictions <- predict(object=model, new.scores[,c("Mean", "Max", "Min")], type='raw')
confusionMatrix(as.numeric(predictions), new.scores$Target)
#plot(predictions, new.scores$Target)
#plot(varImp(model,scale=F))

saveRDS(model, "points_model.rds")



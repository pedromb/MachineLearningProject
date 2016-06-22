library(xgboost)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()

load("bookingTrain.RData")
source("functions.R")
bookingTrain[is.na(bookingTrain)] <- -1
bookingTrain2013 <- as.h2o(subset(bookingTrain, dt_year == 2013))
bookingTrain2014 <- as.h2o(subset(bookingTrain, dt_year == 2014))
dfResults <- data.frame(size=numeric(),scoreTrain=numeric(),scoreValid=numeric())
possibleRatios <- c(.08,.09,0.1,.11,.12)
for(ratio in possibleRatios){
  cat("Starting: ",ratio)
  splitTrain <- h2o.splitFrame(bookingTrain2013, ratios=ratio,seed=-1)
  splitValid <- h2o.splitFrame(bookingTrain2014, ratios=.03,seed=-1)
  
  train <- as.data.frame(splitTrain[1][[1]])
  valid <- as.data.frame(splitValid[1][[1]])
  
  trainLabel <- train$hotel_cluster
  train$hotel_cluster <- NULL
  validLabel <- valid$hotel_cluster
  valid$hotel_cluster <- NULL
  
  trainMatrix <- data.matrix(train)
  validMatrix <- data.matrix(valid)
  dtrain <-xgb.DMatrix(trainMatrix, label=trainLabel)
  
  nrounds <- 100
  eta <- 0.03
  col_sampling <- 0.6
  max_depth <- 6
  
  param <- list(objective           = "multi:softprob", 
                booster             = "gbtree",
                num_class           = 100,
                eta                 = eta,
                max_depth           = max_depth,
                colsample_bytree    = col_sampling)
  xgbModel <- xgb.train(param, dtrain,nrounds=100)
  predictionsArrayTrain <- predict(xgbModel,trainMatrix)
  predictionsMatrixTrain <- matrix(predictionsArrayTrain, nrow=nrow(trainMatrix), ncol=100, byrow=T)
  predictionsArrayValid <- predict(xgbModel, validMatrix)
  predictionsMatrixValid <- matrix(predictionsArrayValid, nrow=nrow(validMatrix), ncol=100, byrow=T)
  predsTrain <- list()
  predsValid <- list()
  for(i in 1:nrow(predictionsMatrixTrain)){
    predictions <- order(-as.numeric(predictionsMatrixTrain[i,1:100]))-1
    predsTrain <- c(predsTrain, list(predictions))
  }
  for(i in 1:nrow(predictionsMatrixValid)){
    predictions <- order(-as.numeric(predictionsMatrixValid[i,1:100]))-1
    predsValid <- c(predsValid, list(predictions))
  }
  scoreTrain <- mapk(5,trainLabel,predsTrain)
  scoreValid <- mapk(5,validLabel,predsValid)
  tmpResult <- data.frame(size=nrow(train),scoreTrain=scoreTrain,scoreValid=scoreValid)
  dfResults <- rbind(dfResults, tmpResult)
  cat(" - Finished")
  write.csv(dfResults,file = "improvedXgb.csv",row.names = F, quote = F)
}
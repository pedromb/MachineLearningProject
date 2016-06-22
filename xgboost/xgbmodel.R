library(xgboost)
##library(h2o)
##h2o.init(nthreads = -1, max_mem_size = "4G")
##h2o.removeAll()

##load("bookingTrain.RData")

##bookingTrain[is.na(bookingTrain)] <- -1
##bookingTrain2013 <- as.h2o(subset(bookingTrain, dt_year == 2013))
##bookingTrain2014 <- as.h2o(subset(bookingTrain, dt_year == 2014))

##splitTrain <- h2o.splitFrame(bookingTrain2013, ratios=.08,seed=-1)
##splitValid <- h2o.splitFrame(bookingTrain2014, ratios=.03,seed=-1)

##train <- as.data.frame(splitTrain[1][[1]])
##valid <- as.data.frame(splitValid[1][[1]])

load("trainAndValid.RData")
source("functions.R")

trainLabel <- train$hotel_cluster
train$hotel_cluster <- NULL
validLabel <- valid$hotel_cluster
valid$hotel_cluster <- NULL

trainMatrix <- data.matrix(train)
validMatrix <- data.matrix(valid)
dtrain <-xgb.DMatrix(trainMatrix, label=trainLabel)

nrounds <- 100
eta <- 0.03
row_sampling <- c(.5,.75,1)
col_sampling <- c(.4,.6,.8,1)
max_depth <- c(4,6,8,10)

searchGrid <- expand.grid("nrounds"=nrounds,"eta"=eta,
                          "row_sampling"=row_sampling,"col_sampling"=col_sampling,
                          "max_depth"=max_depth)
dfResults <- as.data.frame(searchGrid)
dfResults$modelIndex <- 0
dfResults$scoreValid <- 0
dfResults$scoreTrain <- 0
for(k in 1:48) {
  cat("\nStarting model: ",k)
  param <- list(objective           = "multi:softprob", 
                booster             = "gbtree",
                num_class           = 100,
                eta                 = searchGrid[k,]$eta,
                max_depth           = searchGrid[k,]$max_depth,
                subsample           = searchGrid[k,]$row_sampling,
                colsample_bytree    = searchGrid[k,]$col_sampling)
  
  xgbModel <- xgb.train(param, dtrain,nrounds=searchGrid[k,]$nrounds)
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
  dfResults[k,]$modelIndex <- k
  dfResults[k,]$scoreTrain <- scoreTrain
  dfResults[k,]$scoreValid <- scoreValid
  cat(" - Finished")
  write.csv(dfResults,file = "xgbParamTuning.csv",row.names = F, quote = F)
}

results <- read.csv("xgbParamTuning.csv")
results <- results[with(results, order(-scoreValid, -scoreTrain)), ]
write.csv(results,file = "resultsXgb.csv",row.names = F, quote = F)


##Best Model {eta=0.03, row_sampling=1, col_sampling=0.6, max_depth=6}

bestParam <-list(objective           = "multi:softprob", 
                 booster             = "gbtree",
                 num_class           = 100,
                 eta                 = 0.03,
                 max_depth           = 6,
                 subsample           = 1,
                 colsample_bytree    = 0.6)
xgbModel <- xgb.train(bestParam, dtrain,nrounds=100)

##Check results predicting on clusters by search destination id
load("clustersById.RData")
trainSrchIds <- train$srch_destination_id
validSrchIds <- valid$srch_destination_id

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

possibleClustersTrain <- list()
possibleClustersValid <- list()

for(i in trainSrchIds){
  possibleClustersTrain <- c(possibleClustersTrain,list(returnPossibleClusters(i,10,clustersById)))
}

for(i in validSrchIds){
  possibleClustersValid <- c(possibleClustersValid,list(returnPossibleClusters(i,10,clustersById)))
}

clustersResultTrain <- returnClusters(predsTrain,possibleClustersTrain)
clustersResultValid <- returnClusters(predsValid,possibleClustersValid)
mapTrainSub <- mapk(5,trainLabel,clustersResultTrain)
mapValidSub <- mapk(5,validLabel,clustersResultValid)

write.csv(data.frame("mapTrainSub"=mapTrainSub,"mapValidSub"=mapValidSub),file="resultsBySrchId.csv",
          row.names = F,quote = F)

##Submission without leaked data
##load("bookingTrain.RData")
##bookingTrain[is.na(bookingTrain)] <- -1
##set.seed(1234)
##trainSample <-bookingTrain[sample(1:nrow(bookingTrain), 100000, replace=FALSE),]

##trainLabel <- trainSample$hotel_cluster
##trainSample$hotel_cluster <- NULL

##trainMatrix <- data.matrix(trainSample)
##dtrain <-xgb.DMatrix(trainMatrix, label=trainLabel)

##bestParam <-list(objective           = "multi:softprob", 
##                 booster             = "gbtree",
##                 num_class           = 100,
##                 eta                 = 0.03,
##                 max_depth           = 6,
##                 subsample           = 1,
##                 colsample_bytree    = 0.6)

##xgbModel <- xgb.train(bestParam, dtrain,nrounds=100)
##xgb.save(fname="mainModelXgb",model=xgbModel)
library(snow)
load("testChunks.RData")
load("testChunksPossibleClusters.RData")
source("functions.R")

xgbModel <- xgb.load("mainModelXgb")
clusters <- data.frame(hotel_cluster=as.character(),stringsAsFactors = F)
for(k in 1:length(testChunks)){
  currentChunk <- testChunks[k][[1]]
  currentChunk[is.na(currentChunk)] <- -1
  currentChunkMatrix <- data.matrix(currentChunk)
  predictionsArray <- predict(xgbModel,currentChunkMatrix)
  predictionsMatrix <- matrix(predictionsArray, nrow=nrow(currentChunkMatrix), ncol=100, byrow=T)
  preds <- list()
  cat("\n\nFinished making predictions...")
  for(j in 1:nrow(predictionsMatrix)){
    predictions <- order(-as.numeric(predictionsMatrix[j,1:100]))-1
    preds <- c(preds, list(predictions))
  }
  cat("\nFinished organizing predictions...")
  clustersResult <- returnClusters(preds,chunksPossibleClusters[[k]])
  cat("\nFinished getting clusters...")
  cl <- makeCluster(8, type = "SOCK")
  clusterExport(cl, "returnDfCluster")
  dfClusters <- do.call(rbind, parLapply(cl,clustersResult, function(x) returnDfCluster(x)))
  cat("\nFinished making clusters data.frame...")
  stopCluster(cl)
  clusters <- rbind(clusters, dfClusters)
  write.csv(clusters,"predictionsXgb.csv", row.names=F, quote=F)
  cat("\n Finished chunk ",k)
}

##Submission with leaked data
load("finalLeakedData.RData")
clusters <- read.csv("predictionsXgb.csv")
clusters$hotel_cluster <- as.character(clusters$hotel_cluster)
clusters$id <- seq(0,2528242)

newClusters <- subset(clusters, !id %in% dfResult$id)
newClusters <- rbind(newClusters,dfResult)

write.csv(clusters,"submissionLeakedData.csv", row.names=F, quote=F)


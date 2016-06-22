library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()

load("trainAndValid.RData")
source("functions.R")

pTrain <- as.h2o(train)
pValid <- as.h2o(valid)
trainDf <- as.data.frame(pTrain)
trainLabel <- trainDf$hotel_cluster
pTrain$hotel_cluster <- NULL 
validDf <- as.data.frame(pValid)
validLabel <- validDf$hotel_cluster
pValid$hotel_cluster <- NULL

train$hotel_cluster <- as.factor(train$hotel_cluster)
train <- as.h2o(train)
valid <- as.h2o(valid)
y <- "hotel_cluster"
x <- setdiff(names(train), y)

nTrees <- 500
max_depth <- c(10,20,30)
mtries <- c(4,5,6)
searchGrid <- expand.grid("ntrees"=nTrees,"max_depth"=max_depth,
                          "mtries"=mtries)
dfResults <- as.data.frame(searchGrid)
dfResults$modelIndex <- 0
dfResults$scoreValid <- 0
dfResults$scoreTrain <- 0

for(k in 1) {
  rfModel <- h2o.randomForest(
    training_frame = train,
    x=x,
    y=y,
    ntrees=searchGrid[k,]$nTrees,
    stopping_rounds=2,
    score_each_iteration = T,
    seed=1,
    max_depth=searchGrid[k,]$max_depth,
    mtries = searchGrid[k,]$mtries
  )
  
  probsTrain <- as.data.frame(predict(rfModel, pTrain))
  probsValid <- as.data.frame(predict(rfModel, pValid))
  predsTrain <- list()
  predsValid <- list()

  for(i in 1:nrow(probsTrain)){
    predictions <- order(-as.numeric(probsTrain[i,2:101]))-1
    predsTrain <- c(predsTrain, list(predictions))
  }
  for(i in 1:nrow(probsValid)){
    predictions <- order(-as.numeric(probsValid[i,2:101]))-1
    predsValid <- c(predsValid, list(predictions))
  }
  scoreTrain <- mapk(5,trainLabel,predsTrain)
  scoreValid <- mapk(5,validLabel,predsValid)
  dfResults[k,]$modelIndex <- k
  dfResults[k,]$scoreTrain <- scoreTrain
  dfResults[k,]$scoreValid <- scoreValid
  write.csv(dfResults,file = "rfParamTuning.csv",row.names = F, quote = F)
}

##Best Model params {max_depth = 10, mtries=6}

rfModel <- h2o.randomForest(
  training_frame = train,
  x=x,
  y=y,
  ntrees=500,
  stopping_rounds=2,
  score_each_iteration = T,
  seed=1,
  max_depth=10,
  mtries = 6
)

##Check results predicting on clusters by search destination id
load("clustersById.RData")
trainSrchIds <- trainDf$srch_destination_id
validSrchIds <- validDf$srch_destination_id

probsTrain <- as.data.frame(predict(rfModel, pTrain))
probsValid <- as.data.frame(predict(rfModel, pValid))
predsTrain <- list()
predsValid <- list()

for(i in 1:nrow(probsTrain)){
  predictions <- order(-as.numeric(probsTrain[i,2:101]))-1
  predsTrain <- c(predsTrain, list(predictions))
}

for(i in 1:nrow(probsValid)){
  predictions <- order(-as.numeric(probsValid[i,2:101]))-1
  predsValid <- c(predsValid, list(predictions))
}

possibleClustersTrain <- list()
possibleClustersValid <- list()

for(i in trainSrchIds){
  possibleClustersTrain <- c(possibleClustersTrain,list(returnPossibleClusters(i,-1,clustersById)))
}

for(i in validSrchIds){
  possibleClustersValid <- c(possibleClustersValid,list(returnPossibleClusters(i,-1,clustersById)))
}

clustersResultTrain <- returnClusters(predsTrain,possibleClustersTrain)
clustersResultValid <- returnClusters(predsValid,possibleClustersValid)
mapTrainSub <- mapk(5,trainLabel,clustersResultTrain)
mapValidSub <- mapk(5,validLabel,clustersResultValid)

write.csv(data.frame("mapTrainSub"=mapTrainSub,"mapValidSub"=mapValidSub),file="resultsRfBySrchId.csv",
          row.names = F,quote = F)
library(h2o)
library(ggplot2)

h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()

##Probably best to run these sources before running this script
##If you try to run all at once you probably won't have enough memory
##source("preProcessData.R")
##source("generateClustersSplit.R")
##source("functions.R")

load("bookingTrain.RData")
load("clustersById.RData")

##Fill na with -1 for now
bookingTrain[is.na(bookingTrain)] <- -1
bookingTrain$hotel_cluster <- as.factor(bookingTrain$hotel_cluster)


##Convert to h2o type
train <- as.h2o(bookingTrain)

##Gets 2% of data to train and 2% to validate
splitsTrain <- h2o.splitFrame(train, ratios=c(.02,.02),seed=-1)

subTrain <- splitsTrain[1][[1]]
subValid <- splitsTrain[2][[1]]

##Sets response variable and feature variables
y <- "hotel_cluster"
x <- setdiff(names(bookingTrain), y)
rm(bookingTrain)
gc()

##Grid to look for parameters
lambda <- c(0,0.0005,0.001,0.002,0.004)
alpha <- c(0,.5,1)

##Creates a model for each combination of alpha and lambda
modelsList <- list()
results <- data.frame()
for(a in alpha){
  for(l in lambda){
    model_id <- paste("glm_",a,"_",l,sep = "")
    model <-  h2o.glm(training_frame = subTrain, 
                      x=x,
                      y=y,
                      family='multinomial',
                      standardize = T,
                      model_id = model_id,
                      alpha=a,
                      lambda=l,
                      max_iterations = 300)
    df <- data.frame("model_id"=model_id,"alpha"=a,"lambda"=l)
    results <- rbind(results, df)
    modelsList <- c(modelsList,list(model))
  }
}

##Gets the response and the srch_destination_ids for train and valid part
subValidDf <- as.data.frame(subValid)
subValidClusters <- subValidDf$hotel_cluster
subValidSrchIds <- subValidDf$srch_destination_id
subValid$hotel_cluster <- NULL 

subTrainDf <- as.data.frame(subTrain)
subTrainClusters <- subTrainDf$hotel_cluster
subTrainSrchIds <- subTrainDf$srch_destination_id
subTrain$hotel_cluster <- NULL


##Predicts on all clusters for each model for train and valid
modelsTrainPredictions <- list()
modelsValidPredictions <- list()
for(modelIndex in 1:length(modelsList)){
  probsTrain <- as.data.frame(predict(modelsList[modelIndex][[1]], subTrain))
  probsValid <- as.data.frame(predict(modelsList[modelIndex][[1]], subValid))
  predsTrain <- list()
  predsValid <- list()
  for(i in 1:nrow(probsTrain)){
    predictions <- order(as.numeric(probsTrain[i,2:101]))-1
    predsTrain <- c(predsTrain, list(predictions))
  }
  for(i in 1:nrow(probsValid)){
    predictions <- order(as.numeric(probsValid[i,2:101]))-1
    predsValid <- c(predsValid, list(predictions))
  }
  modelsTrainPredictions <- c(modelsTrainPredictions,list(predsTrain))
  modelsValidPredictions <- c(modelsValidPredictions,list(predsValid))
}


##Gets the map (mean average precision) for each model when predicting on all clusters
results$mapTrainAll <- 0
results$mapValidAll <- 0
for(i in 1:length(modelsTrainPredictions)) {
  results[i,]$mapTrainAll <- mapk(5,subTrainClusters,modelsTrainPredictions[i][[1]])
  cat("\nFinished model: ", i)
}
for(i in 1:length(modelsValidPredictions)) {
  results[i,]$mapValidAll <- mapk(5,subValidClusters,modelsValidPredictions[i][[1]])
  cat("\nFinished model: ", i)
}

##Trys to do better by predicting on clusters by srch_destination_id
results$mapTrainSub <- 0
results$mapValidSub <- 0
possibleClustersTrain <- list()
possibleClustersValid <- list()
for(i in subTrainSrchIds){
  possibleClustersTrain <- c(possibleClustersTrain,list(returnPossibleClusters(i,-1,clustersById)))
}
for(i in 1:length(modelsTrainPredictions)) {
  clustersResult <- returnClusters(modelsTrainPredictions[i][[1]],possibleClustersTrain)
  results[i,]$mapTrainSub <- mapk(5,subTrainClusters,clustersResult)
  cat("\nFinished model: ", i)
}
for(i in subValidSrchIds){
  possibleClustersValid <- c(possibleClustersValid,list(returnPossibleClusters(i,-1,clustersById)))
}
for(i in 1:length(modelsValidPredictions)) {
  clustersResult <- returnClusters(modelsValidPredictions[i][[1]],possibleClustersValid)
  results[i,]$mapValidSub <- mapk(5,subValidClusters,clustersResult)
  cat("\nFinished model: ", i)
}

##Trains best model for Ridge, Lasso and Elastic Net with all sample to submit
newSplit <- h2o.splitFrame(train, ratios=.1,seed=-1)
trainData <- newSplit[1][[1]]
##Ridge
ridgeModel <- returnH2oModel(trainData,x,y,0,0.0005)
##Lasso
lassoModel <- returnH2oModel(trainData,x,y,1,0.0005)
##Elastic Net
elasticModel <- returnH2oModel(trainData,x,y,0.5,0.0005)

##Save models
h2o.saveModel(ridgeModel, path = "h2oModels/ridgeModel")
h2o.saveModel(lassoModel, path = "h2oModels/lassoModel")
h2o.saveModel(elasticModel, path = "h2oModels/elasticModel")

#####Generate graphs
splitResults <- split(results, results$alpha)
##Ridge Regresion
ridge <- splitResults[[1]]
gridge <- returnGraph(ridge,"Ridge Regression",0)
##Lasso
lasso <- splitResults[[3]]
glasso <- returnGraph(lasso,"LASSO",0)
##Elastic Net
elastic <- splitResults[[2]]
gelastic <- returnGraph(elastic,"Elastic Net",0)

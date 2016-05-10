apk <- function(k, actual, predicted){
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

mapk <- function (k, actual, predicted) {
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}

returnPossibleClusters <- function(destinationId, k, clustersDf){
  possibleClusters <- subset(clustersDf, srch_destination_id==destinationId)$hotel_cluster
  if(k==-1 || k >= length(possibleClusters)){
    return(possibleClusters)
  }
  else if(k < length(possibleClusters)){
    return(possibleClusters[1:k])
  }
  else{
    return(numeric(0))
  }
}

returnClusters <- function(p,pClusters){
  fullResults <- list()
  for(i in 1:length(p)){
    if(length(pClusters[[i]]) == 0){
      result <- p[[i]][1:5]
      fullResults <- c(fullResults,list(result))
    }
    else{
      pC <- pClusters[[i]]
      indexes <- p[[i]] %in% pC
      newAdds <- list()
      if(sum(indexes==T) < 5){
        for (j in 1:length(indexes)){
          if(indexes[j]==F){
            newAdds <- c(newAdds,p[[i]][j])
          }
          if(length(newAdds) + sum(indexes==T) == 5){
            newAdds <- unlist(newAdds)
            break
          }
        }
      }
      breakIndex <- if(sum(indexes==T) >= 5) 5 else sum(indexes==T)
      result <- p[[i]][indexes][1:breakIndex]
      if(length(newAdds)!=0){
        result <- c(result, newAdds)
      }
      fullResults <- c(fullResults,list(result))
    }
  }
  return(fullResults)
}

returnGraph <- function(entryData, title, kaggleScore){
  library(ggplot2)
  g <- ggplot()
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapTrainAll ,color="Train MAP All Clusters"))
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapValidAll,color="Validation MAP All Clusters"))
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapTrainSub, color="Train MAP Clusters by id"))
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapValidSub,color="Valid MAP Clusters by id"))
  g <- g + xlab("Lambda") + ylab("Precision")
  g <- g + scale_color_manual("Metrics",values=c("blue","black","red","purple"))
  g <- g + ggtitle(paste(title, " - Kaggle Public Score = ",kaggleScore,sep=""))
  return(g)
}

returnH2oModel <- function(trainData,x,y,a,l){
  model <- h2o.glm(training_frame = trainData, 
                   x=x,
                   y=y,
                   family='multinomial',
                   standardize = T,
                   alpha=a,
                   lambda=l,
                   max_iterations = 300)
  return(model)
}

predictOnTestH2o <- function(model){
  load("testChunks.RData")
  for (chunk in testChunks) {
    chunk[is.na(chunk)] <- -1
    chunk <- as.h2o(chunk)
    probs <- as.data.frame(predict(model, chunk))
    chunk <-  as.data.frame(chunk)
    chunkSrchIds <- chunk$srch_destination_id
    preds <- list()
    for(i in 1:nrow(probs)){
      predictions <- order(as.numeric(probs[i,2:101]))-1
      preds <- c(preds, list(predictions))
    }
    clustersResult <- returnClusters(preds,possibleClustersTrain)
    results[i,]$mapTrainSub <- mapk(5,subTrainClusters,clustersResult)
    cat("\nFinished model: ", i)
  }
}


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

returnGraph <- function(entryData, title, kaggleScore, score=0){
  library(ggplot2)
  g <- ggplot()
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapTrainAll,color="Train MAP@5"),size=2)
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapValidAll,color="Validation MAP@5"),size=2)
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapTrainSub, color="Train MAP@5 By Destination"),size=2)
  g <- g + geom_line(data=entryData,aes(x=lambda,y=mapValidSub,color="Validation MAP@5 By Destination"),size=2)
  g <- g + xlab("Lambda") + ylab("Precision")
  g <- g + scale_color_manual("Metrics",values=c("blue","black","red","purple"))
  if(kaggleScore){
    g <- g + ggtitle(paste(title, " - Validation Best Score = ", max(entryData$mapValidSub),"\n",paste("Kaggle Score = ",score,sep=""),sep=""))
  }
  else{
    g <- g + ggtitle(paste(title, " - Validation Best Score = ", max(entryData$mapValidSub),sep=""))
  }
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

predictOnTestH2o <- function(model, title){
  ##load("testChunks.RData")
  ##load("testChunksPossibleClusters.RData")
  for (j in 15:length(testChunks)) {
    cat("\nStarting chunk: ",j)
    testChunks[j][[1]][is.na(testChunks[j][[1]])] <- -1
    chunk <- as.h2o(testChunks[j][[1]])
    probs <- as.data.frame(predict(model, chunk))
    preds <- list()
    for(i in 1:nrow(probs)){
      predictions <- order(as.numeric(probs[i,2:101]))-1
      preds <- c(preds, list(predictions))
    }
    clustersResult <- returnClusters(preds,chunksPossibleClusters[[j]])
    save(file=paste(title,j,".RData",sep=""),clustersResult)    
    cat("\nFinished chunk: ", j)
  }
}

returnDfCluster <- function (x){
  data.frame(hotel_cluster=paste(x,collapse =" "), stringsAsFactors = F)    
}

getTestResults <- function(title){
  library(snow)
  chunksPred <- c(paste(title,1,".RData",sep=""),paste(title,2,".RData",sep=""),paste(title,3,".RData",sep=""),
                  paste(title,4,".RData",sep=""),paste(title,5,".RData",sep=""),paste(title,6,".RData",sep=""),
                  paste(title,7,".RData",sep=""),paste(title,8,".RData",sep=""),paste(title,9,".RData",sep=""),
                  paste(title,10,".RData",sep=""),paste(title,11,".RData",sep=""),paste(title,12,".RData",sep=""),
                  paste(title,13,".RData",sep=""),paste(title,14,".RData",sep=""),paste(title,15,".RData",sep=""))
  results <- data.frame(id=seq(from = 0, to = 2528242, by = 1))
  totalClusters <- data.frame(hotel_cluster=character(0))
  cl <- makeCluster(8)
  clusterExport(cl, "returnDfCluster")
  for(i in chunksPred){
    cat("\nStarting chunk: ", i)
    load(i)
    clusters <- do.call(rbind, parLapply(cl,clustersResult, function(x) returnDfCluster(x)))
    totalClusters <- rbind(totalClusters,clusters)
    cat("\nTotal rows: ",nrow(totalClusters))
    cat("\nFinished chunk: ", i)
  }
  stopCluster(cl)
  submission <- cbind(results, totalClusters)
  submission
}


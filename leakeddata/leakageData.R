leakedData <- read.csv("leakeddata.csv")
dfResult <- data.frame(id=as.numeric(), hotel_cluster=as.character())
k <- 1
while(k < nrow(leakedData)){
  currentValue <- subset(leakedData, id==leakedData[k,]$id)
  if(nrow(currentValue)>1){
    hotel_clusters <- paste(currentValue$hotel_cluster,collapse =" ")
  }
  else{
    hotel_clusters <- currentValue$hotel_cluster
  }
  dfResult <- rbind(dfResult, data.frame(id=currentValue[1,]$id,hotel_cluster=hotel_clusters))
  if(k %% 10000 == 0){
    cat("\n Finsied ",k)
  }
  k <- k+nrow(currentValue)
}

library(snow)
load("test.RData")
load("clustersById.RData")
load("leakedDataClusters.RData")
test$id <- seq(0,2528242)
leakedSrchDestId <- subset(test, id %in% dfResult$id, select="srch_destination_id")
subClusters <- subset(clustersById, srch_destination_id %in% leakedSrchDestId$srch_destination_id)

##Merge leaked data with leakedSrchDestId
##Sort hotel_clusters using clustersById and add in more if less than 5
##nums[order(match(nums,testa))]

dfResult$srchDestId <- leakedSrchDestId$srch_destination_id

organizeClusters <- function(leakedRow){
  possibleClusters <-  subset(subClusters, srch_destination_id==leakedRow$srchDestId)$hotel_cluster
  if(length(strsplit(as.character(leakedRow$hotel_cluster),split=" ")[[1]]) > 1){
    originalClusters <- as.numeric(strsplit(leakedRow$hotel_cluster,split=" ")[[1]])
    newCluster <- originalClusters[order(match(originalClusters,possibleClusters))]
  }
  else{
    newCluster <- leakedRow$hotel_cluster
  }
  if(length(newCluster) < 5){
    aux <- possibleClusters[!possibleClusters %in% newCluster]
    needed <- 5-length(newCluster)
    if(length(aux) > 0){
      if(length(aux) <= needed){
        newCluster <- c(newCluster,aux)
      }
      else{
        newCluster <- c(newCluster,aux[1:needed])
      }
    }
  }
  else{
    newCluster <- newCluster[1:5]
  }
  return(paste(newCluster,collapse=" "))
}
allClusters <- list()
for(i in 37283:nrow(dfResult)){
  allClusters <- c(allClusters,list(organizeClusters(dfResult[i,])))
  if(i %% 10000 == 0){
    cat("\n Finished ",i)
  }
}

dfResult$srchDestId <- NULL
dfResult$hotel_cluster <- unlist(allClusters)
save(file="finalLeakedData.RData", dfResult)

##Take leaked data all of raw data

load("test.RData")
load("finalLeakedData.RData")
test$id <- seq(0,2528242)
notLeaked <- subset(test, !id %in% dfResult$id)

notLeakedChunk1 <- notLeaked[1:170000,]
notLeakedChunk2 <- notLeaked[170001:340000,]
notLeakedChunk3 <- notLeaked[340001:510000,]
notLeakedChunk4 <- notLeaked[510001:680000,]
notLeakedChunk5 <- notLeaked[680001:850000,]
notLeakedChunk6 <- notLeaked[850001:1020000,]
notLeakedChunk7 <- notLeaked[1020001:1190000,]
notLeakedChunk8 <- notLeaked[1190001:1360000,]
notLeakedChunk9 <- notLeaked[1360001:1530000,]
notLeakedChunk10 <- notLeaked[1530001:1700000,]
notLeakedChunk11 <- notLeaked[1700001:1870000,]
notLeakedChunk12 <- notLeaked[1870001:2040000,]
notLeakedChunk13 <- notLeaked[2040001:nrow(notLeaked),]

notLeakedChunks <- list(notLeakedChunk1,notLeakedChunk2,notLeakedChunk3,
                      notLeakedChunk4,notLeakedChunk5,notLeakedChunk6,
                      notLeakedChunk7,notLeakedChunk8,notLeakedChunk9,
                      notLeakedChunk10,notLeakedChunk11,notLeakedChunk12,
                      notLeakedChunk13)

load("clustersById.RData")
source("functions.R")
library(snow)

returnPossibleClustersTest <- function(chunk){
  chunkSrchIds <- chunk$srch_destination_id
  possibleClusters <- list()
  for(i in chunkSrchIds){
    possibleClusters <- c(possibleClusters,list(returnPossibleClusters(i,-1,clustersById)))
  }
  possibleClusters
}

cl <- makeCluster(8)

clusterExport(cl,"returnPossibleClustersTest")
clusterExport(cl,"clustersById")
clusterExport(cl,"returnPossibleClusters")

notLeakedChunksPossibleClusters <- parLapply(cl, notLeakedChunks,function(x) returnPossibleClustersTest(x))

save(file="notLeaked.RData", notLeakedChunks)
save(file="notLeakedPossibleClusters.RData", notLeakedChunksPossibleClusters)
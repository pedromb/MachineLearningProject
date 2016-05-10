library(data.table)
library(plyr)

train <- fread("train.csv", header=T, sep=",",
               stringsAsFactors = F)

train <- subset(train, select=c("srch_destination_id", "hotel_cluster"))

trainClusters <- split(train, train$srch_destination_id)

rm(train)
gc()

clustersById <- data.frame()
for(i in trainClusters){
  df <- count(i)
  df <- df[with(df,order(-freq)),]
  clustersById <- rbind(clustersById, df)
}

save(file="clustersById.RData", clustersById)
library(data.table)
library(lubridate)
library(dplyr)

train <- fread("train.csv", header=T, sep=",",
               stringsAsFactors = F)

bookingTrain <- subset(train, is_booking==1)

rm(train)
gc()

##Drop is_booking
bookingTrain$is_booking <- NULL

test <- fread("test.csv", header=T, sep=",", 
              stringsAsFactors = F)

##Convert date_time/srch_ci/srch_co to date
bookingTrain$date_time <- as.Date(bookingTrain$date_time)
bookingTrain$srch_ci <- as.Date(bookingTrain$srch_ci)
bookingTrain$srch_co <- as.Date(bookingTrain$srch_co)
test$date_time <- as.Date(test$date_time)
test$srch_ci <- as.Date(test$srch_ci)
test$srch_co <- as.Date(test$srch_co)

##Create some features from date

##Booked nights
bookingTrain$booked_nights <- as.integer(bookingTrain$srch_co-bookingTrain$srch_ci)
test$booked_nights <- as.integer(test$srch_co-test$srch_ci)

##Year, month, day and quarter of date_time, srch_ci and src_co
bookingTrain$dt_year <- as.integer(format(bookingTrain$date_time, "%Y"))
bookingTrain$dt_month <- as.integer(format(bookingTrain$date_time, "%m"))
bookingTrain$dt_day <- as.integer(format(bookingTrain$date_time, "%d"))
bookingTrain$dt_quarter <- as.integer(quarter(bookingTrain$date_time))

bookingTrain$ci_year <- as.integer(format(bookingTrain$srch_ci, "%Y"))
bookingTrain$ci_month <- as.integer(format(bookingTrain$srch_ci, "%m"))
bookingTrain$ci_day <- as.integer(format(bookingTrain$srch_ci, "%d"))
bookingTrain$ci_quarter <- as.integer(quarter(bookingTrain$srch_ci))

bookingTrain$co_year <- as.integer(format(bookingTrain$srch_co, "%Y"))
bookingTrain$co_month <- as.integer(format(bookingTrain$srch_co, "%m"))
bookingTrain$co_day <- as.integer(format(bookingTrain$srch_co, "%d"))
bookingTrain$co_quarter <- as.integer(quarter(bookingTrain$srch_co))

test$dt_year <- as.integer(format(test$date_time, "%Y"))
test$dt_month <- as.integer(format(test$date_time, "%m"))
test$dt_day <- as.integer(format(test$date_time, "%d"))
test$dt_quarter <- as.integer(quarter(test$date_time))

test$ci_year <- as.integer(format(test$srch_ci, "%Y"))
test$ci_month <- as.integer(format(test$srch_ci, "%m"))
test$ci_day <- as.integer(format(test$srch_ci, "%d"))
test$ci_quarter <- as.integer(quarter(test$srch_ci))

test$co_year <- as.integer(format(test$srch_co, "%Y"))
test$co_month <- as.integer(format(test$srch_co, "%m"))
test$co_day <- as.integer(format(test$srch_co, "%d"))
test$co_quarter <- as.integer(quarter(test$srch_co))

##Get rid of date_time, srch_ci and srch_co

bookingTrain$date_time <- NULL
bookingTrain$srch_ci <- NULL
bookingTrain$srch_co <- NULL

test$date_time <- NULL
test$srch_ci <- NULL
test$srch_co <- NULL

test$id <- NULL

newTestChunk1 <- newTest[1:170000]
newTestChunk2 <- newTest[170001:340000]
newTestChunk3 <- newTest[340001:510000]
newTestChunk4 <- newTest[510001:680000]
newTestChunk5 <- newTest[680001:850000]
newTestChunk6 <- newTest[850001:1020000]
newTestChunk7 <- newTest[1020001:1190000]
newTestChunk8 <- newTest[1190001:1360000]
newTestChunk9 <- newTest[1360001:1530000]
newTestChunk10 <- newTest[1530001:1700000]
newTestChunk11 <- newTest[1700001:1870000]
newTestChunk12 <- newTest[1870001:2040000]
newTestChunk13 <- newTest[2040001:2210000]
newTestChunk14 <- newTest[2210001:2380000]
newTestChunk15 <- newTest[2380001:nrow(newTest)]

newTestChunks <- list(newTestChunk1,newTestChunk2,newTestChunk3,
                   newTestChunk4,newTestChunk5,newTestChunk6,
                   newTestChunk7,newTestChunk8,newTestChunk9,
                   newTestChunk10,newTestChunk11,newTestChunk12,
                   newTestChunk13,newTestChunk14,newTestChunk15)

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

chunksPossibleClusters <- parLapply(cl, testChunks,function(x) returnPossibleClustersTest(x))

##Save for later use
save(file = "bookingTrain.RData", bookingTrain)
save(file = "test.RData", test)
save(file = "testChunks.RData", testChunks)
save(file = "testChunksPossibleClusters.RData",chunksPossibleClusters)

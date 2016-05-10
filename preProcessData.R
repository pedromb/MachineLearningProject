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

testChunk1 <- test[1:170000]
testChunk2 <- test[170001:340000]
testChunk3 <- test[340001:510000]
testChunk4 <- test[510001:680000]
testChunk5 <- test[680001:850000]
testChunk6 <- test[850001:1020000]
testChunk7 <- test[1020001:1190000]
testChunk8 <- test[1190001:1360000]
testChunk9 <- test[1360001:1530000]
testChunk10 <- test[1530001:1700000]
testChunk11 <- test[1700001:1870000]
testChunk12 <- test[1870001:2040000]
testChunk13 <- test[2040001:2210000]
testChunk14 <- test[2210001:2380000]
testChunk15 <- test[2380001:nrow(test)]

testChunks <- list(testChunk1,testChunk2,testChunk3,
                   testChunk4,testChunk5,testChunk6,
                   testChunk7,testChunk8,testChunk9,
                   testChunk10,testChunk11,testChunk12,
                   testChunk13,testChunk14,testChunk15)

chunkPossibleClusters <- list()
load("clustersById.RData")
for(chunk in testChunks){
  chunkSrchIds <- chunk$srch_destination_id
  possibleClusters <- list()
  for(i in chunkSrchIds){
    possibleClusters <- c(possibleClusters,list(returnPossibleClusters(i,-1,clustersById)))
  }
  chunkPossibleClusters <- c(chunkPossibleClusters,list(possibleClusters))
}

for(i in subTrainSrchIds){
  possibleClustersTrain <- c(possibleClustersTrain,list(returnPossibleClusters(i,-1,clustersById)))
}

##Save for later use
save(file = "bookingTrain.RData", bookingTrain)
save(file = "test.RData", test)
save(file = "testChunks.RData", testChunks)

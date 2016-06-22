library(ggplot2)

results <- read.csv("resultsXgb.csv")

dfResultsM6 <- subset(results, max_depth==6,select=c("col_sampling","scoreValid","scoreTrain"))
dfResultsM4 <- subset(results, max_depth==4,select=c("col_sampling","scoreValid","scoreTrain"))


g <- ggplot()
g <- g + geom_line(data=dfResultsM6,aes(x=col_sampling,y=scoreTrain,color="Train MAP@5 - M6"),size=2)
g <- g + geom_line(data=dfResultsM6,aes(x=col_sampling,y=scoreValid,color="Valid MAP@5 - M6"),size=2)
g <- g + geom_line(data=dfResultsM4,aes(x=col_sampling,y=scoreTrain,color="Train MAP@5 - M4"),size=2)
g <- g + geom_line(data=dfResultsM4,aes(x=col_sampling,y=scoreValid,color="Valid MAP@5 - M4"),size=2)
g <- g + xlab("ColSample") + ylab("Precision")
g <- g + scale_color_manual("Metrics",values=c("blue","black","red","purple"))
g <- g + ggtitle("Tree Boosting - Predicted on all clusters")
ggsave(filename="xgbgraph.png", g, height=7, width=7)

results <- read.csv("improvedXgb.csv")
g <- ggplot()
g <- g + geom_line(data=results,aes(x=size,y=scoreTrain,color="Train MAP@5 - M6"),size=2)
g <- g + geom_line(data=results,aes(x=size,y=scoreValid,color="Valid MAP@5 - M6"),size=2)
g <- g + xlab("Size") + ylab("Precision")
g <- g + scale_color_manual("Metrics",values=c("blue","black"))
g <- g + ggtitle("Tree Boosting")
ggsave(filename="xgbgraph2.png", g, height=7, width=7)
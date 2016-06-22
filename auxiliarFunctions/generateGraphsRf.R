library(ggplot2)

results <- read.csv("resultsRf.csv")

dfResultsMaxDepth1 <- subset(results, max_depth==10 ,select=c("mtries","scoreValid","scoreTrain"))
dfResultsMaxDepth2 <- subset(results, max_depth==20 ,select=c("mtries","scoreValid","scoreTrain"))
dfResultsMaxDepth3 <- subset(results, max_depth==30 ,select=c("mtries","scoreValid","scoreTrain"))

g <- ggplot()
g <- g + geom_line(data=dfResultsMaxDepth1,aes(x=mtries,y=scoreTrain,color="Train MAP@5 - MaxDepth=10"),size=2)
g <- g + geom_line(data=dfResultsMaxDepth1,aes(x=mtries,y=scoreValid,color="Valid MAP@5 - MaxDepth=10"),size=2)
g <- g + geom_line(data=dfResultsMaxDepth2,aes(x=mtries,y=scoreTrain,color="Train MAP@5 - MaxDepth=20"),size=2)
g <- g + geom_line(data=dfResultsMaxDepth2,aes(x=mtries,y=scoreValid,color="Valid MAP@5 - MaxDepth=20"),size=2)
g <- g + geom_line(data=dfResultsMaxDepth3,aes(x=mtries,y=scoreTrain,color="Train MAP@5 - MaxDepth=30"),size=2)
g <- g + geom_line(data=dfResultsMaxDepth3,aes(x=mtries,y=scoreValid,color="Valid MAP@5 - MaxDepth=30"),size=2)
g <- g + xlab("Mtries") + ylab("Precision")
g <- g + scale_color_manual("Metrics",values=c("blue","black","red","purple","gray","green"))
g <- g + ggtitle("Random Forest - Predicted on all clusters")
g <- g + scale_x_continuous(breaks=seq(1:9))

ggsave(filename="rdgraph.png", g, height=7, width=7)

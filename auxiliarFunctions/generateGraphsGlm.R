results <- read.csv("results.csv")
#####Generate graphs
splitResults <- split(results, results$alpha)
##Ridge Regresion
ridge <- splitResults[[1]]
gridge <- returnGraph(ridge,"Ridge Regression",F)
ggsave(filename="ridge.png", gridge, height=7, width=8)
##Lasso
lasso <- splitResults[[3]]
glasso <- returnGraph(lasso,"LASSO",F)
ggsave(filename="lasso.png", glasso, height=7, width=8)
##Elastic Net
elastic <- splitResults[[2]]
gelastic <- returnGraph(elastic,"Elastic Net",T, 0.09292)
ggsave(filename="elastic.png", gelastic, height=7, width=8)

##Class blanace
load("bookingTrain.RData")
balance <- ggplot()
balance <- balance + geom_histogram(aes(x=bookingTrain$hotel_cluster))
balance <- balance + xlab("Cluster") + ylab("Frequência") + ggtitle("Balanceamento das classes")
ggsave(filename="balance.png", balance)

##Pearson
bookingTrain[is.na(bookingTrain)] <- -1
x <- subset(bookingTrain, select=setdiff(names(bookingTrain),"hotel_cluster"))
y <- bookingTrain$hotel_cluster
correlationMatrix <- cor(x,y,method="pearson")
correlationDf <- data.frame(feature=1:32, values=correlationMatrix[1:32])
g <- ggplot(correlationDf, aes(feature, values)) + geom_point()
g <- g + xlab("Índice") + ylab("Valor do coeficiente") + ggtitle("Coeficiente de correlação de Pearson")
ggsave(filename="pearson.png", g, height=7, width=7)

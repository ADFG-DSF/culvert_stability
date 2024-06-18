## just looking at design variables

source("R/2_InstabilityScore_PCA.R")

ndata <- colSums(!is.na(Designs))
plot(sort(ndata))
abline(h=60)
abline(v=which.max(sort(ndata)>60))
sum(ndata >= 60)


Designs_subset <- Designs[, ndata >= 60]

lm_all <- function(x, y, maxnumber = 5) {
  # y is response (vector)
  # x is matrix or data frame
  # maxnumber is the max number of explanatory variables considered

  the_lms <- list()
  for(i in 1:ncol(x)) {
    thedata <- as.data.frame(cbind(y, x[,i]))
    the_lms[[i]] <- lm(y ~ ., data=thedata)
  }
  i_list <- length(the_lms)
  if(maxnumber >= 2) {
    for(i_number in 2:maxnumber) {
      combnmat <- combn(1:ncol(x), i_number)
      for(i_col in 1:ncol(combnmat)) {
        thedata <- as.data.frame(cbind(y, x[,combnmat[,i_col]]))
        i_list <- i_list+1
        the_lms[[i_list]] <- lm(y ~ ., data=thedata)
      }
    }
  }
  return(the_lms)
}
aa <- lm_all(y=Vnew[,1], x=Designs_subset[,-c(1,2,3,9,27,28,31,32)])
theAIC <- sapply(aa, AIC)
summary(aa[[which.min(theAIC)]])
par(mfrow=c(2,2))
plot(aa[[which.min(theAIC)]])


library(rpart)
library(rpart.plot)
thing <- rpart(Vnew[,1] ~ Designs_subset[,4] +
                 Designs_subset[,5] +
                 Designs_subset[,6] +
                 Designs_subset[,7] +
                 Designs_subset[,8]+
                 Designs_subset[,10]+
                 Designs_subset[,11]+
                 Designs_subset[,12]+
                 Designs_subset[,13]+
                 Designs_subset[,14]+
                 Designs_subset[,15]+
                 Designs_subset[,16]+
                 Designs_subset[,17]+
                 Designs_subset[,18]+
                 Designs_subset[,19]+
                 Designs_subset[,20])
summary(thing)
rpart.plot(thing)

treedata <- cbind(y=Vnew[,1], Designs_subset[,-c(1,2,3,9,27,28,31,32)])

rpart(Vnew$`Interior Channel Width` ~
        treedata$`Lake Outlet` +
        treedata$`Straight or Meandering` +
        treedata$`Banks y/N...9` +
        treedata$`Features Y/n` +
        treedata$`One Size Material Used for All` +
        treedata$`Reach 3 Structure Width` +
        treedata$`Reach3 Design CR` +
        treedata$`Reach 3 Total Length` +
        treedata$`Reach 3 Gradient...50` +
        treedata$`Reach 3 Average inlet and Outlet Height` +
        treedata$`Reach 3 \r\nInlet H` +
        # treedata$`Reach 3 \r\nInlet W`  +
        treedata$`Reach 3 \r\nOutlet H` +
        treedata$`Reach 3 \r\nOutlet W` +
        treedata$`Reach 3 \r\nGeo Channel Type` +
        treedata$`Reach 3 \r\nChannel Form` +
        treedata$`Reach 3 \r\nNo of Meanders` +
        treedata$`Reach 3 \r\nInterior Banks Y/N` +
        treedata$`Bank Design Type` +
        treedata$`3 Width Straight` +
        treedata$`Design CR`) %>% rpart.plot

rpart(Vnew$`Interior Gradient` ~
        treedata$`Lake Outlet` +
        treedata$`Straight or Meandering` +
        treedata$`Banks y/N...9` +
        treedata$`Features Y/n` +
        treedata$`One Size Material Used for All` +
        treedata$`Reach 3 Structure Width` +
        treedata$`Reach3 Design CR` +
        treedata$`Reach 3 Total Length` +
        treedata$`Reach 3 Gradient...50` +
        treedata$`Reach 3 Average inlet and Outlet Height` +
        treedata$`Reach 3 \r\nInlet H` +
        # treedata$`Reach 3 \r\nInlet W`  +
        treedata$`Reach 3 \r\nOutlet H` +
        treedata$`Reach 3 \r\nOutlet W` +
        treedata$`Reach 3 \r\nGeo Channel Type` +
        treedata$`Reach 3 \r\nChannel Form` +
        treedata$`Reach 3 \r\nNo of Meanders` +
        treedata$`Reach 3 \r\nInterior Banks Y/N` +
        treedata$`Bank Design Type` +
        treedata$`3 Width Straight` +
        treedata$`Design CR`) %>% rpart.plot

rpart(Vnew$Height ~
        treedata$`Lake Outlet` +
        treedata$`Straight or Meandering` +
        treedata$`Banks y/N...9` +
        treedata$`Features Y/n` +
        treedata$`One Size Material Used for All` +
        treedata$`Reach 3 Structure Width` +
        treedata$`Reach3 Design CR` +
        treedata$`Reach 3 Total Length` +
        treedata$`Reach 3 Gradient...50` +
        treedata$`Reach 3 Average inlet and Outlet Height` +
        treedata$`Reach 3 \r\nInlet H` +
        # treedata$`Reach 3 \r\nInlet W`  +
        treedata$`Reach 3 \r\nOutlet H` +
        treedata$`Reach 3 \r\nOutlet W` +
        treedata$`Reach 3 \r\nGeo Channel Type` +
        treedata$`Reach 3 \r\nChannel Form` +
        treedata$`Reach 3 \r\nNo of Meanders` +
        treedata$`Reach 3 \r\nInterior Banks Y/N` +
        treedata$`Bank Design Type` +
        treedata$`3 Width Straight` +
        treedata$`Design CR`) %>% rpart.plot

rpart(pca_out$PCA_banks_1 ~
        treedata$`Lake Outlet` +
        treedata$`Straight or Meandering` +
        treedata$`Banks y/N...9` +
        treedata$`Features Y/n` +
        treedata$`One Size Material Used for All` +
        treedata$`Reach 3 Structure Width` +
        treedata$`Reach3 Design CR` +
        treedata$`Reach 3 Total Length` +
        treedata$`Reach 3 Gradient...50` +
        treedata$`Reach 3 Average inlet and Outlet Height` +
        treedata$`Reach 3 \r\nInlet H` +
        # treedata$`Reach 3 \r\nInlet W`  +
        treedata$`Reach 3 \r\nOutlet H` +
        treedata$`Reach 3 \r\nOutlet W` +
        treedata$`Reach 3 \r\nGeo Channel Type` +
        treedata$`Reach 3 \r\nChannel Form` +
        treedata$`Reach 3 \r\nNo of Meanders` +
        treedata$`Reach 3 \r\nInterior Banks Y/N` +
        treedata$`Bank Design Type` +
        treedata$`3 Width Straight` +
        treedata$`Design CR`) %>% rpart.plot

library(randomForest)
ytry <- pca_out$PCA_banks_1[!rowSums(is.na(treedata))]
treedata <- na.omit(treedata)
rf <- randomForest(ytry ~
        treedata$`Lake Outlet` +
        treedata$`Straight or Meandering` +
        treedata$`Banks y/N...9` +
        treedata$`Features Y/n` +
        treedata$`One Size Material Used for All` +
        treedata$`Reach 3 Structure Width` +
        treedata$`Reach3 Design CR` +
        treedata$`Reach 3 Total Length` +
        treedata$`Reach 3 Gradient...50` +
        treedata$`Reach 3 Average inlet and Outlet Height` +
        treedata$`Reach 3 \r\nInlet H` +
        # treedata$`Reach 3 \r\nInlet W`  +
        treedata$`Reach 3 \r\nOutlet H` +
        treedata$`Reach 3 \r\nOutlet W` +
        treedata$`Reach 3 \r\nGeo Channel Type` +
        treedata$`Reach 3 \r\nChannel Form` +
        treedata$`Reach 3 \r\nNo of Meanders` +
        treedata$`Reach 3 \r\nInterior Banks Y/N` +
        treedata$`Bank Design Type` +
        treedata$`3 Width Straight` +
        treedata$`Design CR`)
plot(rf)
importance(rf)
varImpPlot(rf)

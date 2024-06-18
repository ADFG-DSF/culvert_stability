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

lm_all_data <- Designs_subset[,-c(1,2,3,9,27,28,29,31,32)]
lm_all_y <- cbind(Vnew[,1:3], pca_out$PCA_banks_1)

all_lm_all <- list()
for(i in 1:4) {
  the_lm_all_call <- lm_all(y=lm_all_y[,i], x=lm_all_data, maxnumber=5)
  all_lm_all[[i]] <- the_lm_all_call[[which.min(sapply(the_lm_all_call, AIC))]]
}
names(all_lm_all)[1:3] <- names(Vnew)[1:3]
names(all_lm_all)[4] <- "PCAbanks1"

# the_lm_all_call <- lm_all(pca_out$PCA_banks_1, x=lm_all_data, maxnumber=5)
# all_lm_all[[4]] <- the_lm_all_call[[which.min(sapply(the_lm_all_call, AIC))]]


for(i in 1:length(all_lm_all)) {
  print(names(all_lm_all)[i])
  print(summary(all_lm_all[[i]]))
}


library(rpart)
library(rpart.plot)
trees <- list()
for(i in 1:4) {
  trees[[i]] <- rpart(lm_all_y[,i] ~ lake_outlet +
          straight_or_meandering +
          banks_y_n_9 +
          features_y_n +
          one_size_material_used_for_all +
          reach_3_structure_width +
          reach3_design_cr +
          reach_3_total_length +
          reach_3_gradient_50 +
          reach_3_average_inlet_and_outlet_height +
          reach_3_inlet_h +
          reach_3_inlet_w +
          reach_3_outlet_h +
          reach_3_outlet_w +
          reach_3_geo_channel_type +
          reach_3_channel_form +
          reach_3_no_of_meanders +
          low_flow_y_n_60 +
          x3_feature_numer +
          reach_3_interior_banks_y_n +
          bank_design_type +
          x3_width_straight +
          design_cr, data=lm_all_data)
}
# trees[[4]] <- rpart(pca_out$PCA_banks_1 ~ lake_outlet +
#         straight_or_meandering +
#         banks_y_n_9 +
#         features_y_n +
#         one_size_material_used_for_all +
#         reach_3_structure_width +
#         reach3_design_cr +
#         reach_3_total_length +
#         reach_3_gradient_50 +
#         reach_3_average_inlet_and_outlet_height +
#         reach_3_inlet_h +
#         reach_3_inlet_w +
#         reach_3_outlet_h +
#         reach_3_outlet_w +
#         reach_3_geo_channel_type +
#         reach_3_channel_form +
#         reach_3_no_of_meanders +
#         low_flow_y_n_60 +
#         x3_feature_numer +
#         reach_3_interior_banks_y_n +
#         bank_design_type +
#         x3_width_straight +
#         design_cr, data=lm_all_data)

par(mfrow=c(1,2))
for(i in 1:4) {
  plot(fitted(trees[[i]]), lm_all_y[,i])
  # plot(fitted(all_lm_all[[i]]), lm_all_y[,i])
}


##### idea: compare candidate models (regtree included) in terms of
##### loocv predictive accuracy (rmse of predicted vs actual)

## formula(all_lm_all[[1]]) gets you the formula


lm_all_InteriorChannelWidth <- lm_all(y=Vnew$`Interior Channel Width`,
             x=lm_all_data)
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

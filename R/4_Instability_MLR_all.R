# Using Multple Linear Regression to look for simultaneous effects of multiple variables

# First, a subset of columns is taken for consideration
# Then, an algorithm is defined to fit ALL POSSIBLE COMBINATIONS of variables
# - this is cool but time-consuming, even with nvariables = 12
# Then, an algorithm is defined to "grow" a model, one variable at a time, using
#   LOOCV RMSE as a model-comparison criterion
# Finally, an algorithm is defined to try regression trees with rpart using a
#   sequence of possible tuning parameters



source("R/3_Instability_EDA.R")

# data subset - CURRENTLY ALL NON-MISSING ROWS
dsub <- Designs_medhigh[, colSums(is.na(Designs_medhigh)) == 0]  %>% # can make this less stringent
  rename(culvert_shape = culvert_shape_at_waterline_straight_sides_or_curved)




# ---------------- defining a bunch of functions

# fitting ALL POSSIBLE COMBINATIONS of variables, up to a certain maxnumber
lm_all <- function(x, y, maxnumber = 5) {
  # y is response (vector)
  # x is matrix or data frame
  # maxnumber is the max number of explanatory variables considered

  nn <- sum(choose(ncol(x), 1:maxnumber))
  pb = txtProgressBar(min = 0, max = nn, initial = 0)

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
        setTxtProgressBar(pb, i_list)
      }
    }
  }
  return(the_lms)
}

# Fitting binomial GLM for ALL POSSIBLE COMBINATIONS of variables
glm_all <- function(x, y, maxnumber = 5) {
  # y is response (vector)
  # x is matrix or data frame
  # maxnumber is the max number of explanatory variables considered

  nn <- sum(choose(ncol(x), 1:maxnumber))
  pb = txtProgressBar(min = 0, max = nn, initial = 0)

  the_lms <- list()
  for(i in 1:ncol(x)) {
    thedata <- as.data.frame(cbind(y, x[,i]))
    the_lms[[i]] <- glm(as.numeric(y) ~ ., data=thedata, family="binomial")
    # the_lms[[i]] <- glm(as.numeric(thedata$y) ~ factor(thedata$V2), family="binomial")
  }
  i_list <- length(the_lms)
  if(maxnumber >= 2) {
    for(i_number in 2:maxnumber) {
      combnmat <- combn(1:ncol(x), i_number)
      for(i_col in 1:ncol(combnmat)) {
        # thedata <- as.data.frame(cbind(y, x[,combnmat[,i_col]]))
        thedata <- cbind(y, x[,combnmat[,i_col]])
        i_list <- i_list+1
        the_lms[[i_list]] <- glm(y ~ ., data=thedata, family="binomial")
        setTxtProgressBar(pb, i_list)
      }
    }
  }
  return(the_lms)
}

# Returning LOOCV RMSE from all possible combinations (time-consuming!!)
# - this only returns a vector of LOOCV RMSE, and depends on the trial algorithm
#   being the same as lm_all() for consistency
lm_all_loocv <- function(x, y, maxnumber = 5) {
  # y is response (vector)
  # x is matrix or data frame
  # maxnumber is the max number of explanatory variables considered

  nn <- sum(choose(ncol(x), 1:maxnumber))
  pb = txtProgressBar(min = 0, max = nn, initial = 0)

  # the_lms <- list()
  the_rmse <- the_r2 <- NA
  for(i in 1:ncol(x)) {
    thedata <- as.data.frame(cbind(y, x[,i]))
    ypred <- rep(NA, length(y))
    for(iy in 1:length(y)) {
      ypred[iy] <- predict(lm(y ~ ., data=thedata[-iy,]), newdata=thedata[iy,])
    }
    the_rmse[i] <- sqrt(mean((y-ypred)^2))#, na.rm=TRUE
    the_r2[i] <- cor(y,ypred)^2#, na.rm=TRUE
    # the_lms[[i]] <- lm(y ~ ., data=thedata)
  }
  # i_list <- length(the_lms)
  i_list <- length(the_rmse)
  if(maxnumber >= 2) {
    for(i_number in 2:maxnumber) {
      combnmat <- combn(1:ncol(x), i_number)
      for(i_col in 1:ncol(combnmat)) {
        thedata <- as.data.frame(cbind(y, x[,combnmat[,i_col]]))
        i_list <- i_list+1
        ypred <- rep(NA, length(y))
        for(iy in 1:length(y)) {
          ypred[iy] <- predict(lm(y ~ ., data=thedata[-iy,]), newdata=thedata[iy,])
        }
        the_rmse[i_list] <- sqrt(mean((y-ypred)^2))#, na.rm=TRUE
        the_r2[i_list] <- cor(y,ypred)^2#, na.rm=TRUE
        # the_lms[[i_list]] <- lm(y ~ ., data=thedata)
        setTxtProgressBar(pb, i_list)
      }
    }
  }
  return(list(the_rmse=the_rmse, the_r2=the_r2))
}

# Returning a matrix of which variables are present in lm_all
# -  depends on the trial algorithm
#   being the same as lm_all() for consistency
lm_varpresent <- function(x, y, maxnumber = 5) {
  # y is response (vector)
  # x is matrix or data frame
  # maxnumber is the max number of explanatory variables considered

  nn <- sum(choose(ncol(x), 1:maxnumber))

  present <- matrix(FALSE, nrow=nn, ncol=ncol(x))

  for(i in 1:ncol(x)) {
    present[i,i] <- TRUE
  }
  i_list <- i
  if(maxnumber >= 2) {
    for(i_number in 2:maxnumber) {
      combnmat <- combn(1:ncol(x), i_number)
      for(i_col in 1:ncol(combnmat)) {
        # thedata <- as.data.frame(cbind(y, x[,combnmat[,i_col]]))
        i_list <- i_list+1
        present[i_list, combnmat[,i_col]] <- TRUE
        # the_lms[[i_list]] <- lm(y ~ ., data=thedata)
        # setTxtProgressBar(pb, i_list)
      }
    }
  }
  return(present)
}




## --------------- trying all possible variables for a given column

maxnumber <- 5   # max number of variables
whichVdir <- 1   # which column of dataframe Vdir

sum(choose(ncol(dsub), 1:maxnumber))

aa <- lm_all(x=dsub, y=Vdir[,whichVdir], maxnumber=maxnumber)
length(aa)
aa_aic <- sapply(aa, AIC)
aa_r2 <- sapply(aa, \(x) summary(x)$r.squared)
aa_nparam <- sapply(aa, \(x) length(x$coefficients))

{
tstart <- Sys.time()
theresults <- lm_all_loocv(x=dsub, y=Vdir[,whichVdir], maxnumber=maxnumber)
aa_rmse <- theresults$the_rmse
aa_predr2 <- theresults$the_r2
Sys.time() - tstart   # 139 sec (2.3 min) with maxnumber = 5
}

aa_present <- lm_varpresent(x=dsub, y=Vdir[,1], maxnumber=maxnumber)


### plotting all model-fitting criteria wrt each other

par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
cols <- adjustcolor(rainbow(max(aa_nparam)), green.f=.85, red.f=.85, blue.f=.85)
plot(aa_aic, aa_rmse, col=cols[aa_nparam])
abline(v=min(aa_aic)+2)
plot(aa_r2, aa_rmse, col=cols[aa_nparam])
plot(aa_r2, aa_aic, col=cols[aa_nparam])
abline(h=min(aa_aic)+2)
plot(aa_predr2, aa_rmse, col=cols[aa_nparam])

summary(aa[[which.min(aa_aic)]])
summary(aa[[which.min(aa_rmse)]])

# measure of relative importance of each variable??
# - avg rank (of models containing each variable)
# - avg rmse
# - avg aic
# - min rmse, min r2

avgrank <- avgrmse <- avgaic <- minrmse <- minr2 <- rep(NA, ncol(dsub))
allrmse <- allaic <- allr2 <- matrix(ncol=ncol(dsub), nrow=sum(aa_present[,1]))
allrmserank <- allaicrank <- matrix(ncol=ncol(dsub), nrow=sum(aa_present[,1]))
for(i in 1:ncol(dsub)) {
  allrmse[,i] <- aa_rmse[aa_present[,i]]
  allaic[,i] <- aa_aic[aa_present[,i]]
  allr2[,i] <- aa_r2[aa_present[,i]]
  allrmserank[,i] <- rank(aa_rmse)[aa_present[,i]]
  allaicrank[,i] <- rank(aa_aic)[aa_present[,i]]
}
boxplot(allrmse)
boxplot(allaic)
boxplot(allr2)
boxplot(allrmserank)
colSums(allrmserank < 50)
apply(allrmserank, 2, median)

which(aa_present[which.min(aa_aic),])
which(aa_present[which.min(aa_rmse),])




### ------------- more efficient idea: growing a model one variable at a time

## defining a function...

# show rmse of all variables by themselves
# pick the lowest rmse
# then show rmse of all variables with the first plus new ones
# pick the lowest
rmse <- function(x1, x2) sqrt(mean((x1-x2)^2))
lm_grow <- function(x, y, xnames=NULL, plotpreds=TRUE) {
  if(is.null(xnames)) xnames <- 1:ncol(x)

  x <- x[!is.na(y),]
  y <- y[!is.na(y)]

  # rmse no-model
  ypred <- rep(NA, length(y))
  for(idata in 1:length(y)) {
    ypred[idata] <- mean(y[-i])
  }
  lowest_rmse <- rmse(y,ypred)

  vars_grow <- NULL
  continue <- TRUE
  while(continue) {
    rmse_iter <- rep(NA, ncol(x))
    for(ivar in (1:ncol(x))[!(1:ncol(x)) %in% vars_grow]) {
      # lmtrial <- lm(y~., data=as.data.frame(cbind(y, x[, c(vars_grow, ivar)])))
      ivar_data <- as.data.frame(cbind(y, x[, c(vars_grow, ivar)]))
      ypred <- rep(NA, length(y))
      for(idata in 1:length(y)) {
        ypred[idata] <- predict(lm(y~., data=ivar_data[-idata,]),
                                newdata=ivar_data[idata,])
      }
      rmse_iter[ivar] <- rmse(y, ypred)
    }

    minrmse <- min(rmse_iter, na.rm=TRUE)
    if(minrmse >= min(lowest_rmse)) {
      continue <- FALSE
    } else {
      plot(rmse_iter, pch=ifelse(rmse_iter==minrmse, 16, 1),
           xlab="", ylab="LOOCV RMSE", xaxt="n", main=paste(xnames[vars_grow],"+"))
      axis(side=1, at=seq_along(rmse_iter), labels=xnames, las=2)
      vars_grow <- c(vars_grow, which.min(rmse_iter))
      lowest_rmse[length(vars_grow)+1] <- minrmse

      if(plotpreds) {
        thedata <- as.data.frame(cbind(y, x[, vars_grow]))
        ypred <- rep(NA, length(y))
        for(idata in 1:length(y)) {
          ypred[idata] <- predict(lm(y~., data=thedata[-idata,]),
                                  newdata=thedata[idata,])
        }
        plot(ypred, y, xlab="LOOCV Predicted y", ylab="True y",
             main=xnames[vars_grow])
        legend("topleft", paste("R^2 =", round(cor(ypred, y)^2, 3)), bg=NA)
        abline(0,1)
      }
    }


    if(length(vars_grow) >= ncol(x)) continue <- FALSE
  }
  if(!is.null(vars_grow)) {
    plot(0:length(vars_grow), lowest_rmse, xaxt="n", xlab="", ylab="LOOCV RMSE", type="b")
    axis(side=1, at=0:length(vars_grow), labels=c(0, paste("+",xnames[vars_grow])), las=2)

    lmfinal <- lm(y~., data=thedata)
    plot(predict(lmfinal), y, xlab="Fitted y", ylab="True y")
    legend("topleft", paste("R^2 =", round(summary(lmfinal)$r.squared, 3)), bg=NA)
    abline(0,1)
  }

  return(vars_grow)
}
par(mar=c(12,4,4,2))
dsub_names <- names(dsub)
# dsub_names[1] <- "culvert_shape"

par(mfcol=c(2,5))
lm_grow(x=dsub, y=Vdir[,1], xnames=dsub_names)




##### -----    TRYING GLM FOR ALL COMBINATIONS (currently abandoned)


# maxnumber <- 12
# sum(choose(ncol(dsub), 1:maxnumber))
#
# aa <- glm_all(x=dsub, y=1*(Vdir_cat[,1]=="+Positive"), maxnumber=maxnumber)
# length(aa)
# aa_aic <- sapply(aa, AIC)
# # aa_r2 <- sapply(aa, \(x) summary(x)$r.squared)
# aa_nparam <- sapply(aa, \(x) length(x$coefficients))
#
# # {
# #   tstart <- Sys.time()
# #   aa_rmse <- lm_all_loocv(x=dsub, y=Vdir[,1], maxnumber=maxnumber)
# #   Sys.time() - tstart   # 139 sec (2.3 min) with maxnumber = 5
# # }
#
# aa_present <- lm_varpresent(x=dsub, y=Vdir[,1], maxnumber=maxnumber)
#
# # par(mfrow=c(2,2))
# # par(mar=c(4,4,4,2))
# cols <- adjustcolor(rainbow(max(aa_nparam)), green.f=.85, red.f=.85, blue.f=.85)
# # plot(aa_aic, aa_rmse, col=cols[aa_nparam])
# # abline(v=min(aa_aic)+2)
# # plot(aa_r2, aa_rmse, col=cols[aa_nparam])
# # plot(aa_r2, aa_aic, col=cols[aa_nparam])
# # abline(h=min(aa_aic)+2)
#
# summary(aa[[which.min(aa_aic)]])
# plot(aa_aic, col=cols[aa_nparam])
# # summary(aa[[which.min(aa_rmse)]])
#
#
# # glm does not seem to work?


##### -------------------- TRYING REGRESSION TREE

library(rpart)
library(rpart.plot)

# dsub <- rename(dsub, culvert_shape = culvert_shape_at_waterline_straight_sides_or_curved)

yy <- Vdir[,1]

## Defining a function that displays the fit of a regression tree.
## A sequence of values for argument minsplit= are tried, and compared using
## loocv RMSE.  Plots are constructed for
## - loocv prediction RMSE vs choice of minsplit=
## - variable importance in the final selected tree
## - the tree itself
## - y vs loocv predicted y
## - y vs fitted y
build_rpart <- function(yy, x) {   # CAREFUL: the data.fram dsub is pulled from environment!!!
  thedata <- cbind(yy, x)[!is.na(yy),]
  rmses <- rsqds <- NA

  # deciding which value of minsplit to use
  splits <- 5:30
  for(isplit in seq_along(splits)) {
    ypred <- rep(NA, nrow(thedata))
    for(i in 1:nrow(thedata)) {
      ypred[i] <- predict(rpart(yy~., data=thedata[-i,],
                                control=rpart.control(minsplit=splits[isplit])),
                          newdata=thedata[i,])
    }
    rmses[isplit] <- rmse(thedata$yy,ypred)
    rsqds[isplit] <- cor(thedata$yy,ypred)^2
  }

  # plotting minsplit
  par(mar=c(4,4,4,2))
  plot(splits,rmses, ylab="prediction RMSE", xlab="minsplit", type="b")#, ylim=range(rmses, rsqds)
  abline(v=splits[which.min(rmses)])

  # fitting the best model
  thepart <- rpart(yy~., data=thedata, minsplit=splits[which.min(rmses)]) #, minsplit=27

  # calculating loocv predicted values & R^2
  ypred <- rep(NA, nrow(thedata))
  for(i in 1:nrow(thedata)) {
    ypred[i] <- predict(rpart(yy~., data=thedata[-i,],
                              minsplit=splits[which.min(rmses)]),
                        newdata=thedata[i,])
  }

  # plotting y vs loocv predicted y, and y vs fitted y
  par(mar=c(12,4,4,2))
  thepart$variable.importance %>% barplot(las=3, main="variable importance")
  rpart.plot(thepart)
  par(mar=c(4,4,4,2))
  plot(ypred, thedata$yy, xlab="LOOCV predicted y", ylab="true y")
  legend("topleft", legend=paste("R^2 =", round(cor(ypred,thedata$yy)^2, 3)))
  abline(0,1)
  plot(predict(thepart),thedata$yy, xlab="Fitted y", ylab="true y")
  legend("topleft", legend=paste("R^2 =", round(cor(predict(thepart),thedata$yy)^2, 3)))
  abline(0,1)
}





#### need to wrap MLE and rpart things as functions of y

whichone <- 1

par(mfcol=c(2,5))
par(mar=c(10,4,4,2))
lm_grow(x=dsub, y=Vdir[,whichone], xnames=names(dsub))

par(mfrow=c(2,3))
build_rpart(x=dsub, yy=Vdir[,whichone])

# ---

whichone <- 2

par(mfcol=c(2,2))
par(mar=c(10,4,4,2))
lm_grow(x=dsub, y=Vdir[,whichone], xnames=names(dsub))

par(mfrow=c(2,3))
build_rpart(x=dsub, yy=Vdir[,whichone])

# ---

whichone <- 3

par(mfcol=c(2,3))
par(mar=c(10,4,4,2))
lm_grow(x=dsub, y=Vdir[,whichone], xnames=names(dsub))

par(mfrow=c(2,3))
build_rpart(x=dsub, yy=Vdir[,whichone])

# ---

whichone <- 4

par(mfcol=c(2,4))
par(mar=c(10,4,4,2))
lm_grow(x=dsub, y=Vdir[,whichone], xnames=names(dsub))

par(mfrow=c(2,3))
build_rpart(x=dsub, yy=Vdir[,whichone])

# ---







# ideas:
# - use prediction R2 instead of prediction RMSE - NO, leave this alone, not monotonic
# - organize toward a markdown document with 4 pages (only consider Vdir) - could also do Vabs, come to think of it
#   - a figure describing MLR model growth & fit
#   - a figure describing tree model & fit
#   - comparison between prediction R2 and fit R2 for both methods
#   - a necessary writeup explaining how inter-related explanatory variables behave!!

# GLM vs rpart categorical?  can take mean of prediction accuracy

source("R/2_InstabilityScore_PCA.R")

## Objects created in the source() call that we care about:
# Designs
# importancevec
# Vabs
# Vdir



# converting from numeric to factor
Designs$bank_design_type <- as.character(Designs$bank_design_type)

# creating a new dataset where importance is "high"
Designs_high <- Designs[, importancevec %in% "high"]

# making a categorical version of design cr
Designs_high$reach3_design_cr_cat <- cut(Designs_high$reach3_design_cr, c(0,1.1,1.3,3))
dim(Designs_high)
par(mfrow=c(1,1))
# plot(Designs_high)
summary(Designs_high)

# how many missing values?
image(is.na(Designs_high))
colMeans(is.na(Designs_high))
colSums(is.na(Designs_high))
sum(colSums(is.na(Designs_high)) == 0)
sum(colSums(is.na(Designs_high)) <= 1)



# creating a new dataset where importance is "high" or "medium"
Designs_medhigh <- Designs[, importancevec %in% c("medium","high")]
Designs_medhigh$reach3_design_cr_cat <- Designs_high$reach3_design_cr_cat
dim(Designs_medhigh)
par(mfrow=c(1,1))
# plot(Designs_medhigh)
summary(Designs_medhigh)

# how many missing values?
image(is.na(Designs_medhigh))
colMeans(is.na(Designs_medhigh))
colSums(is.na(Designs_medhigh))
sum(colSums(is.na(Designs_medhigh)) == 0)
sum(colSums(is.na(Designs_medhigh)) <= 1)


## Lots of missing values.  Will explore effects each variable separately, then
## only do MLR/tree on dataset with sufficient data.
apply(Designs_medhigh, 2, \(x) length(unique(x)))



## creating a plotting function to plot ALL instability scores vs ALL variables
magicplot <- function(x, y, main,...) {
  # assuming y is numeric
  pval <- summary(lm(y~x))$coefficients[2,4]
  if(class(x) %in% c("factor","character")) {
    x[is.na(x)] <- "_NA_"
    thetab <- table(x)
    theplotnames <- boxplot(y~x, plot=FALSE)$names
    namestoplot <- rep(NA, length(theplotnames))
    for(inames in seq_along(namestoplot)) {
      namestoplot[inames] <- paste0(theplotnames[inames], " (n = ",
                                    sum(x==theplotnames[inames]), ")")
    }
    boxplot(y ~ x,
            main=c(main, paste("Anova pval =", round(pval, 4))),
            xlab="",
            names = namestoplot,
            # names = paste0(names(thetab), " (n = ", thetab, ")"),
            las=2,
            ...=...)
  } else {
    NAval <- min(x, na.rm=TRUE) - 0.2*diff(range(x, na.rm=TRUE))
    x[is.na(x)] <- NAval
    plot(y ~ x,
         main=c(main, paste("Reg pval =", round(pval, 4))),
         xlab="", col=1+(x==NAval),
         ...=...)
    axis(side=1, at=NAval, labels=paste0("NA (n=", sum(x==NAval),")"), las=2)
  }
}

# considering absolute instability
for(j in 1:4) {
  par(mfrow=c(3,4))
  par(mar=c(8,5,4,2))
  for(i in 1:ncol(Designs_medhigh)) {
    magicplot(x=Designs_medhigh[,i],
              y=Vabs[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vabs)[j])
  }
}

# considering directional instability
for(j in 1:4) {
  par(mfrow=c(3,4))
  par(mar=c(8,5,4,2))
  for(i in 1:ncol(Designs_medhigh)) {
    magicplot(x=Designs_medhigh[,i],
              y=Vdir[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vdir)[j])
  }
}  # looks like things are either very negative, very positive, or zero!



# # look for obvious thresholds in RESPONSES to reach3_design_cr
# # look for obvious thresholds in Vdir (and maybe Vabs?)
#
# par(mfrow=c(2,2))
# for(i in 1:4) {
#   hist(Vabs[,i])
#   abline(v=c(-1,1)*log(1.5))
# }
# for(i in 1:4) {
#   hist(Vdir[,i])
#   abline(v=c(-1,1)*log(1.5))
# }
# for(i in 1:4) {
#   plot(Vabs[,i])
#   abline(h=c(-1,1)*log(1.5))
#   table(ifelse(Vabs[,i] < log(1.5), "Stable", "Unstable")) %>% print
# }
# for(i in 1:4) {
#   plot(Vdir[,i])
#   abline(h=c(-1,1)*log(1.5))
#   table(ifelse(Vdir[,i] < -log(1.5), "Negative",
#                ifelse(Vdir[,i] > log(1.5),"Positive", "Stable"))) %>% print
# }
# # for(i in 1:4) {
# #   plot(Vdir[,i],Vabs[,i])
# #   abline(h=c(-1,1)*log(1.5))
# #   abline(v=c(-1,1)*log(1.5))
# # }



## defining new instability scores as categorical, in which stability vs instability
## is defined using a threshold value of a ratio of 1.5

Vabs_cat <- Vdir_cat <- NA*Vabs
for(i in 1:4) {
  # Vabs_cat[,i] <- ifelse(Vabs[,i] < log(1.5), "Stable", "Unstable")
  # vscores were standardized by sd, therefore not fully consistent abs vs dir

  Vabs_cat[,i] <- ifelse(abs(Vdir[,i]) < log(1.5), "Stable", "Unstable")
  Vdir_cat[,i] <- ifelse(Vdir[,i] < -log(1.5), "-Negative",
                         ifelse(Vdir[,i] > log(1.5), "+Positive", " Stable"))
}
# for(i in 1:4) {
#   table(Vabs_cat[,i],Vdir_cat[,i]) %>% print
# }

# for(i in 1:4) {
#   plot(Designs_medhigh$reach3_design_cr, Vabs[,i])
#   abline(v=c(1.1,1.3))
#   abline(h=c(-1,1)*log(1.5))
# }
# for(i in 1:4) {
#   plot(Designs_medhigh$reach3_design_cr, Vdir[,i])
#   abline(v=c(1.1,1.3))
#   abline(h=c(-1,1)*log(1.5))
# }
# breaks <- c(0,1.1,1.3,3)
# for(i in 1:4) {
#   mosaicplot(table(cut(Designs_medhigh$reach3_design_cr, breaks),
#                    Vabs_cat[,i]),
#              col=c(3,2))
# }
# for(i in 1:4) {
#   mosaicplot(table(cut(Designs_medhigh$reach3_design_cr, breaks),
#                    Vdir_cat[,i]),
#              col=c(4,3,2))
# }



## defining a new plotting function to plot (categorical) instability vs all variables

magicplot_cat <- function(x, y, main,...) {
  # assuming y is categorical

  if(class(x) %in% c("factor","character")) {
    # mosaicplot with addl NA and sample sizes for columns
    thetable1 <- table(x,y)
    pval <- chisq.test(thetable1)$p.value
    x <- as.character(x)
    x[is.na(x)] <- "_NA_"
    thetable2 <- table(x,y)
    dimnames(thetable2)[[1]] <- paste0(dimnames(thetable2)[[1]],
                                       " (n = ", table(x),")")
    mosaicplot(thetable2,
               main=c(main, paste("chi^2 pval =", round(pval, 4))),
               ...=...)

  } else {
    #
    # NAval <- min(x, na.rm=TRUE) - 0.2*diff(range(x, na.rm=TRUE))
    # x[is.na(x)] <- NAval
    # plot(y ~ x,
    #      main=c(main, paste("Reg pval =", round(pval, 4))),
    #      xlab="", col=1+(x==NAval),
    #      ...=...)
    # axis(side=1, at=NAval, labels=paste0("NA (n=", sum(x==NAval),")"), las=2)
    pval <- summary(lm(x~y))$coefficients[2,4]
    y <- as.character(y)
    # y[is.na(y)] <- "_NA_"
    thetab <- table(y)
    theplotnames <- boxplot(x~y, plot=FALSE)$names
    namestoplot <- rep(NA, length(theplotnames))
    for(inames in seq_along(namestoplot)) {
      namestoplot[inames] <- paste0(theplotnames[inames], " (n = ",
                                    sum(y==theplotnames[inames]), ")")
    }
    boxplot(x ~ y,
            main=c(main, paste("Anova pval =", round(pval, 4))),
            xlab="",
            names = namestoplot,
            # names = paste0(names(thetab), " (n = ", thetab, ")"),
            las=2,
            ...=...)
  }
}

# considering absolute instability
for(j in 1:4) {
  par(mfrow=c(3,4))
  par(mar=c(8,5,4,2))
  for(i in 1:ncol(Designs_medhigh)) {
    magicplot_cat(x=Designs_medhigh[,i],
              y=Vabs_cat[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vabs_cat)[j],
              col=c(3,2))
  }
}

# considering directional instability
for(j in 1:4) {
  par(mfrow=c(3,4))
  par(mar=c(8,5,4,2))
  for(i in 1:ncol(Designs_medhigh)) {
    magicplot_cat(x=Designs_medhigh[,i],
              y=Vdir_cat[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vdir_cat)[j],
              col=c(4,3,2))
  }
}

## from here:
# look for good stories
# MLR on subset with data (dredge routine!)
# regression tree, maybe use subset and see if it matters
# - see if I can find a better plot method
# logistic regression: stable/unstable, unstable negative, unstable positive (again dredge)
# whatever I find, check out loocv predictive power!!


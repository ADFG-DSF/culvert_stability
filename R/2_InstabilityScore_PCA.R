# Culvert Stability
# Calculating instability scores and aggregating with PCA

# running the data import script
source("R/1_culvert_data.R")


library(jagshelper)  # only for random color function
library(dsftools)    # for plotting correlation


# extracting measurements & designs (consistent by index)
measurements <- VTable[, c(2, 6, 11, 15, 18, 21)]
designs <- VTable[, c(3, 7, 12, 16, 19, 22)]

# imputing zero for NA values in design for banks
designs[,4:6][is.na(designs[,4:6])] <- 0


# defining a function to visualize a candidate instability score
plotimage <- function(x, y, fun, main="",...) {
  # x is a vector of design numbers
  # y is a vector of measurement numbers
  # fun is a function(x,y)

  xgrid <- seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), length.out=100)
  ygrid <- seq(from=min(y, na.rm=TRUE), to=max(y, na.rm=TRUE), length.out=100)
  xx <- matrix(xgrid,
               nrow=100, ncol=100)
  yy <- matrix(ygrid,
               nrow=100, ncol=100, byrow=TRUE)
  zz <- fun(xx, yy)
  image(zz, x=xgrid, y=ygrid, xlab="design", ylab="measured", main=main)
  points(x,y, ...=...)
}

# defining a set of candidate functions to calculate instability score
v1 <- function(des, meas) {
  meas/des
}
abs_v1 <- function(des, meas) {
  1-(meas/des)
}
log_v1 <- function(des, meas) {
  meas <- meas + 0.01*diff(range(des, na.rm = TRUE))
  des <- des + 0.01*diff(range(des, na.rm = TRUE))
  log(meas/des)
}
abslog_v1 <- function(des, meas) {   ### I like this one the best
  meas <- meas + 0.01*diff(range(des, na.rm = TRUE))
  des <- des + 0.01*diff(range(des, na.rm = TRUE))
  abs(log(meas/des))
}



# Visualizing them all!
cols <- rcolors(nrow(designs))
par(mfrow=c(2,3))
for(i in 1:6) {
  plotimage(x=designs[,i], y=measurements[,i], fun=v1,
            bg=cols, pch=21,
            main=names(designs)[i])
}
for(i in 1:6) plot(designs[,i],
                   v1(designs[,i], measurements[,i]),
                   main=names(designs)[i], xlab="design", ylab="measured/design",
                   pch=16, col=cols)

par(mfrow=c(2,3))
for(i in 1:6) {
  plotimage(x=designs[,i], y=measurements[,i], fun=abs_v1,
            bg=cols, pch=21,
            main=names(designs)[i])
}
for(i in 1:6) plot(designs[,i],
                   abs_v1(designs[,i], measurements[,i]),
                   main=names(designs)[i], xlab="design", ylab="1-measured/design",
                   pch=16, col=cols)

par(mfrow=c(2,3))
for(i in 1:6) {
  plotimage(x=designs[,i], y=measurements[,i], fun=log_v1,
            bg=cols, pch=21,
            main=names(designs)[i])
}
for(i in 1:6) plot(designs[,i],
                   log_v1(designs[,i], measurements[,i]),
                   main=names(designs)[i], xlab="design", ylab="log(measured/design)",
                   pch=16, col=cols)

par(mfrow=c(2,3))
for(i in 1:6) {
  plotimage(x=designs[,i], y=measurements[,i], fun=abslog_v1,
            bg=cols, pch=21,
            main=names(designs)[i])
}
for(i in 1:6) plot(designs[,i],
                   abslog_v1(designs[,i], measurements[,i]),
                   main=names(designs)[i], xlab="design", ylab="abs(log(measured/design))",
                   pch=16, col=cols)


# Calculating a new Vscore using the absolute log
# NOTE: this is normalized so all columns have unit variance (all the same scale)
Vabs <- Vdir <- NA*designs
for(j in 1:ncol(designs)) {
  Vabs[,j] <- abslog_v1(designs[,j], measurements[,j])
  Vabs[,j] <- Vabs[,j]/sd(Vabs[,j], na.rm=TRUE)
  Vdir[,j] <- log_v1(designs[,j], measurements[,j])
  Vdir[,j] <- Vdir[,j]/sd(Vdir[,j], na.rm=TRUE)
}

# simplifying names somewhat
names(Vabs) <- names(Vdir) <- c("Interior Channel Width", "Interior Gradient", "Height",
                 "Bank Length", "Bank Height", "Bank Width")

# visualizing all possible pairwise relationships
par(mfrow=c(1,1))
plot(Vabs)
plot(Vdir)

# printing & plotting a correlation matrix
cor(Vabs, use="na.or.complete")
cor(Vdir, use="na.or.complete")

parmar <- par("mar")  # storing the margins in the default graphics state
# setting new margins for the next plot
par(mar=c(10, 10, 4, 2))
plotcor(cor(Vabs, use="na.or.complete"),
        main="Correlation between Instability Scores (absolute)")  # plotting
plotcor(cor(Vdir, use="na.or.complete"),
        main="Correlation between Instability Scores (with direction)")  # plotting
par(mar=parmar) # resetting margins


# Principal Components Analysis of all scores
pc_all <- princomp(na.omit(Vabs))
plot(pc_all)
pc_all$loadings
biplot(pc_all)
summary(pc_all)

# Principal Components Analysis of just banks
pc_banks <- princomp(na.omit(Vabs[,4:6]))
plot(pc_banks)
pc_banks$loadings
biplot(pc_banks)
summary(pc_banks)


# bundling PCA scores with equivalent number of rows as dataset
# - filling NA where appropriate
pca_out <- matrix(nrow=nrow(Vabs),
                  ncol=ncol(pc_all$scores) + ncol(pc_banks$scores))

# filling in scores from first PCA
pca_out[!is.na(rowSums(Vabs)),
        1:ncol(pc_all$scores)] <- pc_all$scores

# filling in scores from second PCA (just banks)
pca_out[!is.na(rowSums(Vabs[,4:6])),
        ncol(pc_all$scores) + 1:ncol(pc_banks$scores)] <- pc_banks$scores

# adding column names
colnames(pca_out) <- c(paste0("PCA_all_", 1:ncol(pc_all$scores)),
                       paste0("PCA_banks_", 1:ncol(pc_banks$scores)))
pca_out <- as.data.frame(pca_out)




## creating a plotting function to plot ALL instability scores vs ALL variables
magicplot <- function(x, y, main,...) {
  # assuming y is numeric
  # pval <- summary(lm(y~x))$coefficients[2,4]

  # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
  pval <- anova(lm(y~x))$`Pr(>F)`[1]

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

## defining a new plotting function to plot (categorical) instability vs all variables
magicplot_cat <- function(x, y, main, ylab, horizontal=FALSE, ...) {
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
    if(horizontal) {
      mosaicplot(thetable2, ylab=ylab, xlab="",# labs are new
                 main=c(main, paste("chi^2 pval =", round(pval, 4))), #was main
                 ...=...)
    } else {
      mosaicplot(thetable2, xlab=main, ylab="",# labs are new
               main=c(ylab, paste("chi^2 pval =", round(pval, 4))), #was main
               ...=...)
    }


  } else {
    #
    # NAval <- min(x, na.rm=TRUE) - 0.2*diff(range(x, na.rm=TRUE))
    # x[is.na(x)] <- NAval
    # plot(y ~ x,
    #      main=c(main, paste("Reg pval =", round(pval, 4))),
    #      xlab="", col=1+(x==NAval),
    #      ...=...)
    # axis(side=1, at=NAval, labels=paste0("NA (n=", sum(x==NAval),")"), las=2)
    # pval <- summary(lm(x~y))$coefficients[2,4]

    # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
    pval <- anova(lm(x~y))$`Pr(>F)`[1]

    y <- as.character(y)
    # y[is.na(y)] <- "_NA_"
    thetab <- table(y)
    theplotnames <- boxplot(x~y, plot=FALSE)$names
    namestoplot <- rep(NA, length(theplotnames))
    for(inames in seq_along(namestoplot)) {
      namestoplot[inames] <- paste0(theplotnames[inames], " (n = ",
                                    sum(y==theplotnames[inames], na.rm=TRUE), ")")
    }
    if(horizontal) {
      boxplot(x ~ y,
              main=c(ylab, paste("Anova pval =", round(pval, 4))), # was main
              # xlab="",
              # ylab=main, # new
              ylab="",
              xlab=main, # new
              horizontal=TRUE,
              names = namestoplot,
              # names = paste0(names(thetab), " (n = ", thetab, ")"),
              las=2,
              ...=...)
    } else {
      boxplot(x ~ y,
              main=c(ylab, paste("Anova pval =", round(pval, 4))), # was main
              xlab="",
              ylab=main, # new
              # ylab="",
              # xlab=main, # new
              # horizontal=TRUE,
              names = namestoplot,
              # names = paste0(names(thetab), " (n = ", thetab, ")"),
              las=2,
              ...=...)
    }
  }
}

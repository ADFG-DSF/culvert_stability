# Culvert Stability
# Calculating instability scores and aggregating with PCA

# running the data import script
source("R/1_culvert_data.R")


# only to use the random color function
library(jagshelper)


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
Vnew <- NA*designs
for(j in 1:ncol(designs)) {
  Vnew[,j] <- abslog_v1(designs[,j], measurements[,j])
  Vnew[,j] <- Vnew[,j]/sd(Vnew[,j], na.rm=TRUE)
}

# simplifying names somewhat
names(Vnew) <- c("Interior Channel Width", "Interior Gradient", "Height",
                 "Bank Length", "Bank Height", "Bank Width")

# visualizing all possible pairwise relationships
par(mfrow=c(1,1))
plot(Vnew)

# printing a correlation matrix
cor(Vnew, use="na.or.complete")

# stealing the guts out of jagshelper::plotcor_jags!
# then visualizing the correlation between scores
plotcor <- function(dfcor, mincor=0, maxn=4, maxcex=1, legend=TRUE, ...) {
  dfnames <- dimnames(dfcor)[[1]] #names(df)
  dfwhich <- sapply(strsplit(dfnames,split="[",fixed=T),FUN="[",1)
  dfhowmany <- rep(NA,length(dfnames))
  for(i in 1:length(dfnames)) dfhowmany[i] <- sum(dfwhich==dfwhich[i])
  dfdim <- cumsum(1/dfhowmany)
  dfdim1 <- c(0, dfdim[-length(dfdim)])

  xmat <- matrix(dfdim, nrow=length(dfdim), ncol=length(dfdim))
  ymat <- matrix(dfdim, nrow=length(dfdim), ncol=length(dfdim), byrow=T)
  xmat1 <- matrix(dfdim1, nrow=length(dfdim), ncol=length(dfdim))
  ymat1 <- matrix(dfdim1, nrow=length(dfdim), ncol=length(dfdim), byrow=T)

  cols <- 0*xmat
  for(i in 1:nrow(xmat)) {
    for(j in 1:i) {
      if(!is.na(dfcor[i,j])) {
        if(dfcor[i,j] > 0) {
          cols[i,j] <- cols[j,i] <- adjustcolor(2,alpha.f=dfcor[i,j])
        }
        if(dfcor[i,j] < 0) {
          cols[i,j] <- cols[j,i] <- adjustcolor(4,alpha.f=-dfcor[i,j])
        }
      }
      if(i==j) cols[i,j] <- 1
    }
  }

  plot(NA, xlim=(1+.1*legend)*range(0,dfdim), ylim=rev(range(0,dfdim)),
       yaxt="n", xaxt="n", ylab="", xlab="", bty='n',...=...)#,yaxs="i", xaxs="i"
  dfwhichunique <- unique(dfwhich)
  axis(side=1, at=1:length(dfwhichunique)-.5, labels=dfwhichunique, las=2)
  axis(side=2, at=1:length(dfwhichunique)-.5, labels=dfwhichunique, las=2)

  rect(xleft=xmat1, xright=xmat, ybottom=ymat1, ytop=ymat, border=cols, col=cols)
  for(i in 1:nrow(xmat)) {
    for(j in 1:i) {
      if((dfhowmany[i]<=maxn) & (dfhowmany[j]<=maxn) & (abs(dfcor[i,j])>=mincor)) {
        text(x=dfdim1[i]+0.5/dfhowmany[i], y=dfdim1[j]+0.5/dfhowmany[j], labels=round(dfcor[i,j],2), cex=maxcex*abs(dfcor[i,j])^.3)
        text(x=dfdim1[j]+0.5/dfhowmany[j], y=dfdim1[i]+0.5/dfhowmany[i], labels=round(dfcor[i,j],2), cex=maxcex*abs(dfcor[i,j])^.3)
      }
    }
  }
  # abline(v=0:length(dfwhichunique))
  segments(x0=rep(0, length(dfwhichunique)+1),
           x1=rep(length(dfwhichunique), length(dfwhichunique)+1),
           y0=0:length(dfwhichunique))
  segments(y0=rep(0, length(dfwhichunique)+1),
           y1=rep(length(dfwhichunique), length(dfwhichunique)+1),
           x0=0:length(dfwhichunique))

  if(legend) {
    legendby <- .25
    legendn <- 2/legendby+1

    legendl <- rep(1.05*max(dfdim),legendn)
    legendr <- rep(1.1*max(dfdim),legendn)
    legendb <- seq(from=.5*max(dfdim), to=0, length.out=legendn+1)[-legendn-1]
    legendt <- seq(from=.5*max(dfdim), to=0, length.out=legendn+1)[-1]

    legendcols <- rep(0, legendn)
    legendcors <- seq(-1,1,length.out=legendn)
    for(i in 1:(1/legendby)) {
      legendcols[i] <- adjustcolor(4, alpha.f=-legendcors[i])
      legendcols[legendn+1-i] <- adjustcolor(2, alpha.f=-legendcors[i])
    }

    rect(xleft=legendl, xright=legendr, ytop=legendt, ybottom=legendb, col=legendcols, border=NA)
    text(x=.5*(legendl+legendr), y=.5*(legendt+legendb), labels=legendcors, cex=.7)
  }
}
parmar <- par("mar")  # storing the margins in the default graphics state
# setting new margins for the next plot
par(mar=c(10, 10, 4, 2))
plotcor(cor(Vnew, use="na.or.complete"), main="Correlation between Instability Scores")  # plotting
par(mar=parmar) # resetting margins


# Principal Components Analysis of all scores
pc_all <- princomp(na.omit(Vnew))
plot(pc_all)
pc_all$loadings
biplot(pc_all)
summary(pc_all)

# Principal Components Analysis of just banks
pc_banks <- princomp(na.omit(Vnew[,4:6]))
plot(pc_banks)
pc_banks$loadings
biplot(pc_banks)
summary(pc_banks)


# bundling PCA scores with equivalent number of rows as dataset
# - filling NA where appropriate
pca_out <- matrix(nrow=nrow(Vnew),
                  ncol=ncol(pc_all$scores) + ncol(pc_banks$scores))

# filling in scores from first PCA
pca_out[!is.na(rowSums(Vnew)),
        1:ncol(pc_all$scores)] <- pc_all$scores

# filling in scores from second PCA (just banks)
pca_out[!is.na(rowSums(Vnew[,4:6])),
        ncol(pc_all$scores) + 1:ncol(pc_banks$scores)] <- pc_banks$scores

# adding column names
colnames(pca_out) <- c(paste0("PCA_all_", 1:ncol(pc_all$scores)),
                       paste0("PCA_banks_", 1:ncol(pc_banks$scores)))
pca_out <- as.data.frame(pca_out)

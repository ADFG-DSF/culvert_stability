library(tidyverse)


data_smasher <- function(df, na_values = c("na", "#DIV/0!")) {
  df <- as.data.frame(df)
  for(j in 1:ncol(df)) {
    df[df[, j] %in% na_values, j] <- NA
    # maybe this should have an as.character in there too
    if(suppressWarnings(sum(!is.na(as.numeric(as.character(df[, j])))) == sum(!is.na(df[, j])))) {
      df[, j] <- as.numeric(df[, j])
    } else {
      # df[, j] <- str_replace_all(df[, j], "[^[:alnum:]]", " ")   # this doesn't work
      for(i in 1:nrow(df)) {
        if(suppressWarnings(is.na(as.numeric(df[i, j])))) {
          df[i, j] <- str_replace_all(df[i, j], "[^[:alnum:]]", " ")
        }
      }
      df[, j] <- tolower(df[, j])
    }
  }
  return(df)
}

letter2num <- function(x) {
  theletters <- tolower(strsplit(x, split="")[[1]])
  thenumbers <- c(which(letters==theletters[1]), which(letters==theletters[2]))
  return(26*thenumbers[1] + thenumbers[2])
}

# ## read all data
# Visual_Stability <- read_csv("flat_data/Visual_Stability.csv",
#                              n_max=66, col_select = 1:12) %>% data_smasher
# VTable <- read_csv("flat_data/VTable.csv",
#                    n_max=66, col_select = 1:24) %>% data_smasher
# Exterior_Data <- read_csv("flat_data/Exterior_Data.csv",
#                           n_max=66, col_select = 1:letter2num("JX")) %>% data_smasher
# Interior_Data <- read_csv("flat_data/Interior_Data.csv",
#                           n_max=66, col_select = 1:letter2num("GW")) %>% data_smasher
# Measured_in_Field <- read_csv("flat_data/Measured_in_Field.csv",
#                               n_max=66, col_select = 1:letter2num("RS")) %>% data_smasher
# Designs <- read_csv("flat_data/Designs.csv",
# n_max=66, col_select = 1:letter2num("DQ")) %>% data_smasher


library(readxl)
Designs <- read_xlsx("Assessment Data June 10 2024.xlsx",
          sheet = "Designs",
          range="A1:DQ67") %>% data_smasher
Visual_Stability <- read_xlsx("Assessment Data June 10 2024.xlsx",
                              sheet = "Visual Stability",
                              range="A1:L67") %>% data_smasher
VTable <- read_xlsx("Assessment Data June 10 2024.xlsx",
                    sheet = "VTable",
                   range="A1:X67") %>% data_smasher
Exterior_Data <- read_xlsx("Assessment Data June 10 2024.xlsx",
                           sheet = "Exterior Data",
                           range="A1:JX67") %>% data_smasher
Interior_Data <- read_xlsx("Assessment Data June 10 2024.xlsx",
                           sheet = "Interior Data",
                           range="A1:GW67") %>% data_smasher
Measured_in_Field <- read_xlsx("Assessment Data June 10 2024.xlsx",
                               sheet = "Measured in Field",
                               range="A1:RS67") %>% data_smasher


## find some threshold value of number of blank fields to eliminate columns
all_tabs <- cbind(Visual_Stability,
                  VTable,
                  Exterior_Data,
                  Interior_Data,
                  Measured_in_Field,
                  Designs)
dim(all_tabs)
# 1133 total columns!!  or 646

ndata <- colSums(!is.na(all_tabs))  # number of nonblank entries for each column
hist(ndata)
plot(sort(ndata))
abline(h=60)
abline(v=which.max(sort(ndata) >= 60))
sum(ndata >= 60)

## choose/make variables



names(VTable)
plot(VTable$`V Interior Channel Width`)
plot(log(VTable$`V Interior Channel Width`))
hist(VTable$`V Interior Channel Width`)
hist(log(VTable$`V Interior Channel Width`))

plotstuff <- function(x,...) {
  x <- x[!is.na(x)]
  x <- x[x!=0]
  par(mfrow=c(3,2))
  plot(x,...=...)
  plot(log(x))
  hist(x)
  hist(log(x))
  qqnorm(x)
  qqline(x)
  qqnorm(log(x))
  qqline(log(x))
}
vnames <- names(VTable)
for(i in seq_along(vnames)) {
  if(substr(vnames[i], 1, 1)=="V") plotstuff(VTable[,i], main=vnames[i])
}
justV <- VTable[, c(4, 9, 13, 17, 20, 23)]
cor(justV, use="na.or.complete")

pc1 <- princomp(na.omit(justV))
pc1_abs <- princomp(na.omit(abs(1-justV)))


justV_subset <- VTable[, c(4, 9, 13)]
cor(justV_subset, use="na.or.complete")

pc1_subset <- princomp(na.omit(justV_subset))
pc1_abs_subset <- princomp(na.omit(abs(1-justV_subset)))
plot(pc1_subset)
pc1_subset$loadings
biplot(pc1_subset)
plot(pc1_abs_subset)
pc1_abs_subset$loadings
biplot(pc1_abs_subset)


# extracting measurements & designs (consistent by index)
measurements <- VTable[, c(2, 6, 11, 15, 18, 21)]
designs <- VTable[, c(3, 7, 12, 16, 19, 22)]

# imputing zero for NA values in design for banks
designs[,4:6][is.na(designs[,4:6])] <- 0

# candidate scoring?
v1 <- measurements/designs
abs_v1 <- 1-v1 # abs(1-v1)
log_v1 <- log(v1+.01)
abslog_v1 <- abs(log(v1+.01))
v2 <- designs - measurements

cols <- jagshelper::rcolors(66)

## plotting measurements vs design
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], measurements[,i],
       xlab=names(designs)[i], ylab=names(measurements)[i],
       col=cols, pch=16)
  abline(0, 1, lty=3)
}

## plotting candidate v scores vs design
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], v1[,i],
       xlab=names(v1)[i], ylab=names(measurements)[i], main="v1",
       col=cols, pch=16)
  abline(h=1, lty=3)
}
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], abs_v1[,i],
       xlab=names(v1)[i], ylab=names(measurements)[i], main="1-v1",
       col=cols, pch=16)
  abline(h=0, lty=3)
}
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], log_v1[,i],
       xlab=names(v1)[i], ylab=names(measurements)[i], main="log(v1)",
       col=cols, pch=16)
  abline(h=0, lty=3)
}
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], abslog_v1[,i],
       xlab=names(v1)[i], ylab=names(measurements)[i], main="abs(log(v1))",
       col=cols, pch=16)
  abline(h=0, lty=3)
}



xx <- matrix(1:25, nrow=25, ncol=25, byrow=TRUE)
yy <- matrix(1:25, nrow=25, ncol=25)
zz <- xx/yy
plot(contour(zz))
image(zz)

xx <- matrix(seq(from=min(designs[,1]), to=max(designs[,1]), length.out=100),
          nrow=100, ncol=100)
yy <- matrix(seq(from=min(measurements[,1]), to=max(measurements[,1]), length.out=100),
          nrow=100, ncol=100, byrow=TRUE)
zz <- yy/xx
plot(contour(zz,
             x=seq(from=min(designs[,1]), to=max(designs[,1]), length.out=100),
             y=seq(from=min(measurements[,1]), to=max(measurements[,1]), length.out=100)))
points(designs[,1], measurements[,1])
image(zz,
             x=seq(from=min(designs[,1]), to=max(designs[,1]), length.out=100),
             y=seq(from=min(measurements[,1]), to=max(measurements[,1]), length.out=100))
points(designs[,1], measurements[,1])

image(abs(yy-xx))
image(log(yy/xx))
image(abs(log(yy/xx)))


par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], v2[,i],
       xlab=names(v1)[i], ylab=names(measurements)[i], main="v2",
       col=cols, pch=16)
  abline(h=0, lty=3)
}
par(mfrow=c(2,3))
for(i in 1:6) {
  plot(designs[,i], abs(v2[,i]),
       xlab=names(v1)[i], ylab=names(measurements)[i], main="abs(v2)",
       col=cols, pch=16)
  abline(h=0, lty=3)
}

par(mfrow=c(1,1))
plot(v1)
plot(abs_v1)
plot(log_v1)
plot(abslog_v1)




plotimage <- function(x, y, fun, main="",...) {
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
plotimage(x=designs[,1], y=measurements[,1], fun=function(x,y) abs(log(y/x)),
          col=cols, pch=16)

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
abslog_v1 <- function(des, meas) {
  meas <- meas + 0.01*diff(range(des, na.rm = TRUE))
  des <- des + 0.01*diff(range(des, na.rm = TRUE))
  abs(log(meas/des))
}

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

Vnew <- NA*designs
for(j in 1:ncol(designs)) {
  Vnew[,j] <- abslog_v1(designs[,j], measurements[,j])
  Vnew[,j] <- Vnew[,j]/sd(Vnew[,j], na.rm=TRUE)
}
par(mfrow=c(1,1))
plot(Vnew)
cor(Vnew, use="na.or.complete")

pc1 <- princomp(na.omit(Vnew))
plot(pc1)
pc1$loadings
biplot(pc1)
summary(pc1)

# pc2 <- prcomp(na.omit(Vnew))
# plot(pc2)
# pc2$rotation
# biplot(pc2)
# summary(pc2)

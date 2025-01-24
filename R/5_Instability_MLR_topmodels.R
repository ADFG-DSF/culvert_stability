## The main purpose of this script was to visualize the effects within the
## multiple regression models, since so far the variables had only been plotted
## one at a time.

## For the top models for each Vscore (directional), plots were produced for each
## variable in turn, but overlayed with MLR effects plots.  In many cases, the
## data plot indicated no evidence of relationship (and p-values are taken from this)
## but the MLR effects are present and evident by the effects plots.

## Effects plots were made by expressing the MLR as an equivalent Bayesian model.

## This was only done for directional Vscores, never with absolute or categorical.
## This script is incomplete, and I think was abandoned at one point.  Still cool though.




### some notes to myself at one point:
# decisions that I want to make:
# * which score?
#   - numeric & directional
#   - numeric & absolute
#   - categorical & directional
#   - categorical & absolute
# * MLR?  Tree?
#   * Measured variables?
#
# follow-up decisions:
# * how to present results visually (meh can think about that later)
# * what to present: top model & important variables
#
#
# what to make for today:
# * EDA plots for JUST top-level important variables
# * caterpillar plots for top models.... would be MUCH better as Bayesian effect plots
# * correlation plot for numeric/directional


source("R/3_Instability_EDA.R")

dsub <- Designs_medhigh[, colSums(is.na(Designs_medhigh)) == 0]  %>%
  rename(culvert_shape = culvert_shape_at_waterline_straight_sides_or_curved) %>%
  rename(banks_y_n = banks_y_n_10) %>%
  rename(reach_3_width = x3_width_straight) %>%
  rename(reach_3_gradient = reach_3_gradient_51) %>%
  rename(reach_3_channel_type = reach_3_geo_channel_type)


# using dsub & Vdir
names(dsub)
# [1] "culvert_shape"           "lake_outlet"             "banks_y_n"
# [4] "features_y_n"            "reach_3_structure_width" "reach3_design_cr"
# [7] "reach_3_total_length"    "reach_3_gradient"        "reach_3_channel_type"
# [10] "bank_design_type"        "reach_3_width"           "reach3_design_cr_cat"

names(Vdir)
# [1] "Interior Channel Width" "Interior Gradient"      "Height"
# [4] "Bank Length"            "Bank Height"            "Bank Width"

Vnames <- names(Vdir)
dnames <- names(dsub)

### these are the top variables by themselves (from the MLR doc):

# Vdir$`Interior Channel Width` ~
#   dsub$banks_y_n +
#   dsub$reach3_design_cr +
#   dsub$bank_design_type +
#   dsub$reach3_design_cr_cat

# Vdir$`Interior Gradient` ~
#   dsub$reach_3_channel_type

# Vdir$Height ~
#   dsub$reach_3_gradient

# Vdir$`Bank Length` ~
#   dsub$banks_y_n +
#   dsub$bank_design_type


### checking to see which variables are significant by themselves
for(iV in 1:4) {
  print(Vnames[iV])
  for(id in 1:ncol(dsub)) {
    lm1 <- lm(Vdir[,iV] ~ dsub[,id])
    if(anova(lm1)$`Pr(>F)`[1] <= 0.05) {
      print(paste(Vnames[iV], "~", dnames[id]))
    }
  }
}

# [1] "Interior Channel Width"

# [1] "Interior Channel Width ~ banks_y_n"
# [1] "Interior Channel Width ~ features_y_n"
# [1] "Interior Channel Width ~ reach3_design_cr"
# [1] "Interior Channel Width ~ reach_3_channel_type"
# [1] "Interior Channel Width ~ bank_design_type"
# [1] "Interior Channel Width ~ reach_3_width"
# [1] "Interior Channel Width ~ reach3_design_cr_cat"
# ---------------

# [1] "Interior Gradient"
# [1] "Interior Gradient ~ reach_3_channel_type"
# ---------------

# [1] "Height"

# [1] "Height ~ banks_y_n"
# [1] "Height ~ reach3_design_cr"
# [1] "Height ~ reach_3_gradient"
# [1] "Height ~ bank_design_type"
# ---------------

# [1] "Bank Length"

# [1] "Bank Length ~ banks_y_n"
# [1] "Bank Length ~ reach3_design_cr"
# [1] "Bank Length ~ reach_3_channel_type"
# [1] "Bank Length ~ bank_design_type"
# [1] "Bank Length ~ reach3_design_cr_cat"



# ## creating a plotting function to plot ALL instability scores vs ALL variables
# magicplot <- function(x, y, main,...) {
#   # assuming y is numeric
#
#   # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
#   pval <- anova(lm(y~x))$`Pr(>F)`[1]
#
#   if(class(x) %in% c("factor","character")) {
#     x[is.na(x)] <- "_NA_"
#     thetab <- table(x)
#     theplotnames <- boxplot(y~x, plot=FALSE)$names
#     namestoplot <- rep(NA, length(theplotnames))
#     for(inames in seq_along(namestoplot)) {
#       namestoplot[inames] <- paste0(theplotnames[inames], " (n = ",
#                                     sum(x==theplotnames[inames]), ")")
#     }
#     boxplot(y ~ x,
#             main=c(main, paste("Anova pval =", round(pval, 4))),
#             xlab="",
#             names = namestoplot,
#             # names = paste0(names(thetab), " (n = ", thetab, ")"),
#             las=2,
#             ...=...)
#   } else {
#     NAval <- min(x, na.rm=TRUE) - 0.2*diff(range(x, na.rm=TRUE))
#     x[is.na(x)] <- NAval
#     plot(y ~ x,
#          main=c(main, paste("Reg pval =", round(pval, 4))),
#          xlab="", col=1+(x==NAval),
#          ...=...)
#     axis(side=1, at=NAval, labels=paste0("NA (n=", sum(x==NAval),")"), las=2)
#   }
# }

### plotting all variables that are significant in themselves
for(iV in 1:4) {
  # print(Vnames[iV])
  par(mfrow=c(3,3))
  for(id in 1:ncol(dsub)) {
    lm1 <- lm(Vdir[,iV] ~ dsub[,id])
    if(anova(lm1)$`Pr(>F)`[1] <= 0.05) {
      # print(paste(Vnames[iV], "~", dnames[id]))
      magicplot(x=dsub[,id], y=Vdir[,iV],
                main=dnames[id], ylab=paste("V", Vnames[iV]))
    }
  }
}



### these are the top models:

# Vdir$`Interior Channel Width` ~
#   dsub$bank_design_type +
#   dsub$reach_3_channel_type +
#   dsub$reach3_design_cr +
#   dsub$features_y_n

# Vdir$`Interior Gradient` ~
#   dsub$reach_3_channel_type

# Vdir$Height ~
#   dsub$reach_3_gradient +
#   dsub$reach3_design_cr

# Vdir$`Bank Length` ~
#   dsub$banks_y_n +
#   dsub$culvert_shape +
#   dsub$reach_3_gradient


### make them Bayesian to make effects plots
library(jagsUI)
library(jagshelper)

# creating a generic model that can be used with any data we are interested in
vmod_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 +
      do_num[1]*bnum1*num1[i] +
      do_num[2]*bnum2*num2[i] +
      do_cat[1]*bcat1[cat1[i]] +
      do_cat[2]*bcat2[cat2[i]] +
      do_cat[3]*bcat3[cat3[i]]
  }
  b0 ~ dnorm(0, 0.001)
  bnum1 ~ dnorm(0, 0.001)
  bnum2 ~ dnorm(0, 0.001)

  for(icat1 in 1:(ncat1-1)) {
    bcat1[icat1] ~ dnorm(0, 0.001)
  }
  bcat1[ncat1] <- -sum(bcat1[1:(ncat1-1)])

  for(icat2 in 1:(ncat2-1)) {
    bcat2[icat2] ~ dnorm(0, 0.001)
  }
  bcat2[ncat2] <- -sum(bcat2[1:(ncat2-1)])

  for(icat3 in 1:(ncat3-1)) {
    bcat3[icat3] ~ dnorm(0, 0.001)
  }
  bcat3[ncat3] <- -sum(bcat3[1:(ncat3-1)])

  tau <- pow(sig, -2)
  sig ~ dunif(0, 10)

}', file=vmod_jags)



# a function to create a standardized data object that is interpretable by the
# generic model above
dothedata <- function(vmod_data) {
  vmod_data$n <- length(vmod_data$y)
  vmod_data$do_num <- 1*c(!is.null(vmod_data$num1), !is.null(vmod_data$num2))
  vmod_data$do_cat <- 1*c(!is.null(vmod_data$cat1), !is.null(vmod_data$cat2), !is.null(vmod_data$cat3))
  if(is.null(vmod_data$num1)) {
    vmod_data$num1 <- rep(1, vmod_data$n)
  }
  if(is.null(vmod_data$num2)) {
    vmod_data$num2 <- rep(1, vmod_data$n)
  }
  if(is.null(vmod_data$cat1)) {
    vmod_data$cat1 <- rep(1, vmod_data$n)
    vmod_data$ncat1 <- 2
  } else {
    vmod_data$cat1 <- as.numeric(as.factor(vmod_data$cat1))
    vmod_data$ncat1 <- max(vmod_data$cat1)
  }
  if(is.null(vmod_data$cat2)) {
    vmod_data$cat2 <- rep(1, vmod_data$n)
    vmod_data$ncat2 <- 2
  } else {
    vmod_data$cat2 <- as.numeric(as.factor(vmod_data$cat2))
    vmod_data$ncat2 <- max(vmod_data$cat2)
  }
  if(is.null(vmod_data$cat3)) {
    vmod_data$cat3 <- rep(1, vmod_data$n)
    vmod_data$ncat3 <- 2
  } else {
    vmod_data$cat3 <- as.numeric(as.factor(vmod_data$cat3))
    vmod_data$ncat3 <- max(vmod_data$cat3)
  }

  return(vmod_data)
}

# a wrapper function to run the generic model
runthemodel <- function(vmod_data) {
  tstart <- Sys.time()
  print(tstart)
  vmod_jags_out <- jagsUI::jags(model.file=vmod_jags, data=vmod_data,
                                parameters.to.save=c("bnum1","bnum2","bcat1","bcat2","bcat3","b0","sig"),
                                n.chains=ncores, parallel=T, n.iter=niter,
                                n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  return(vmod_jags_out)
}
niter <- 10000
# ncores <- 3
ncores <- min(10, parallel::detectCores()-1)

# a function to automagically overlay data plots with model effects plots!
maketheplots <- function(vmod_data, vmod_jags_out) {
  par(mfrow=c(2,2))
  if(vmod_data$do_num[1]) {
    magicplot(x=vmod_data$num1, y=Vdir[,iV],
              ylab=paste("V", Vnames[iV]), main=num1_main)
    for(i in sample(1:niter, 100)) {
      abline(a=vmod_jags_out$sims.list$b0[i],
             b=vmod_jags_out$sims.list$bnum1[i],
             col=adjustcolor(4,alpha.f=.1))
    }
  }
  if(vmod_data$do_num[2]) {
    magicplot(x=vmod_data$num2, y=Vdir[,iV],
              ylab=paste("V", Vnames[iV]), main=num2_main)
    for(i in sample(1:niter, 100)) {
      abline(a=vmod_jags_out$sims.list$b0[i],
             b=vmod_jags_out$sims.list$bnum2[i],
             col=adjustcolor(4,alpha.f=.1))
    }
  }
  if(vmod_data$do_cat[1]) {
    magicplot(x=cat1_levels[vmod_data$cat1], y=Vdir[,iV],
              ylab=paste("V", Vnames[iV]), main=cat1_main,
              col=adjustcolor(1,alpha.f=.1), border=adjustcolor(1,alpha.f=.2))
    caterpillar(vmod_jags_out$sims.list$bcat1 + vmod_jags_out$sims.list$b0,
                # vmod_jags_out$sims.list$bcat1 + mean(vmod_data$y, na.rm=TRUE),
                # vmod_jags_out, p="bcat1",
                main=c(cat1_main,"- effect on -",paste("V", Vnames[iV])),
                xax=cat1_levels, add=TRUE, col=4, lwd=2,
                x=(1:ncol(vmod_jags_out$sims.list$bcat1)) + 0.2)
    abline(h=0, lty=2)
  }
  if(vmod_data$do_cat[2]) {
    magicplot(x=cat2_levels[vmod_data$cat2], y=Vdir[,iV],
              ylab=paste("V", Vnames[iV]), main=cat2_main,
              col=adjustcolor(1,alpha.f=.1), border=adjustcolor(1,alpha.f=.2))
    caterpillar(vmod_jags_out$sims.list$bcat2 + vmod_jags_out$sims.list$b0,
                # vmod_jags_out$sims.list$bcat1 + mean(vmod_data$y, na.rm=TRUE),
                # vmod_jags_out, p="bcat1",
                main=c(cat2_main,"- effect on -",paste("V", Vnames[iV])),
                xax=cat2_levels, add=TRUE, col=4, lwd=2,
                x=(1:ncol(vmod_jags_out$sims.list$bcat2)) + 0.2)
    abline(h=0, lty=2)
  }
  if(vmod_data$do_cat[3]) {
    magicplot(x=cat3_levels[vmod_data$cat3], y=Vdir[,iV],
              ylab=paste("V", Vnames[iV]), main=cat3_main,
              col=adjustcolor(1,alpha.f=.1), border=adjustcolor(1,alpha.f=.2))
    caterpillar(vmod_jags_out$sims.list$bcat3 + vmod_jags_out$sims.list$b0,
                # vmod_jags_out$sims.list$bcat1 + mean(vmod_data$y, na.rm=TRUE),
                # vmod_jags_out, p="bcat1",
                main=c(cat3_main,"- effect on -",paste("V", Vnames[iV])),
                xax=cat3_levels, add=TRUE, col=4, lwd=2,
                x=(1:ncol(vmod_jags_out$sims.list$bcat3)) + 0.2)
    abline(h=0, lty=2)
  }
}

# MODEL:
# Vdir$`Interior Channel Width` ~
#   dsub$bank_design_type +
#   dsub$reach_3_channel_type +
#   dsub$reach3_design_cr +
#   dsub$features_y_n

iV <- 1
vmod_data <- list(y = Vdir[,iV],
                  num1 = dsub$reach3_design_cr - mean(dsub$reach3_design_cr, na.rm=TRUE),
                  num2 = NULL,
                  cat1 = dsub$bank_design_type,
                  cat2 =  dsub$reach_3_channel_type,
                  cat3 = dsub$features_y_n)
num1_main <- "reach3_design_cr"
num2_main <- NULL
cat1_main <- "bank_design_type"
cat2_main <- "reach_3_channel_type"
cat3_main <- "features_y_n"
cat1_levels <- levels(as.factor(dsub$bank_design_type))
cat2_levels <- levels(as.factor(dsub$reach_3_channel_type))
cat3_levels <- levels(as.factor(dsub$features_y_n))

vmod_data <- dothedata(vmod_data)
vmod_jags_out <- runthemodel(vmod_data)
maketheplots(vmod_data, vmod_jags_out)







# MODEL:
# Vdir$`Interior Gradient` ~
#   dsub$reach_3_channel_type

iV <- 2
vmod_data <- list(y = Vdir[,iV],
                  num1 = NULL,
                  num2 = NULL,
                  cat1 = dsub$reach_3_channel_type,
                  cat2 =  NULL,
                  cat3 = NULL)
num1_main <- NULL
num2_main <- NULL
cat1_main <- "reach_3_channel_type"
cat2_main <- NULL
cat3_main <- NULL
cat1_levels <- levels(as.factor(dsub$reach_3_channel_type))
cat2_levels <- NULL
cat3_levels <- NULL

vmod_data <- dothedata(vmod_data)
vmod_jags_out <- runthemodel(vmod_data)
maketheplots(vmod_data, vmod_jags_out)






# MODEL:
# Vdir$Height ~
#   dsub$reach_3_gradient +
#   dsub$reach3_design_cr

iV <- 3
Vnames[iV]
vmod_data <- list(y = Vdir[,iV],
                  num1 = dsub$reach_3_gradient - mean(dsub$reach_3_gradient, na.rm=TRUE),
                  num2 = dsub$reach3_design_cr - mean(dsub$reach3_design_cr, na.rm=TRUE),
                  cat1 = NULL,
                  cat2 = NULL,
                  cat3 = NULL)
num1_main <- "reach_3_gradient"
num2_main <- "reach3_design_cr"
cat1_main <- NULL
cat2_main <- NULL
cat3_main <- NULL
cat1_levels <- NULL
cat2_levels <- NULL
cat3_levels <- NULL

vmod_data <- dothedata(vmod_data)
vmod_jags_out <- runthemodel(vmod_data)
maketheplots(vmod_data, vmod_jags_out)






# MODEL:
# Vdir$`Bank Length` ~
#   dsub$banks_y_n +
#   dsub$culvert_shape +
#   dsub$reach_3_gradient

iV <- 4
Vnames[iV]
vmod_data <- list(y = Vdir[,iV],
                  num1 = dsub$reach_3_gradient - mean(dsub$reach_3_gradient, na.rm=TRUE),
                  num2 = NULL,
                  cat1 = dsub$banks_y_n,
                  cat2 =  dsub$culvert_shape,
                  cat3 = NULL)
num1_main <- "reach_3_gradient"
num2_main <- NULL
cat1_main <- "banks_y_n"
cat2_main <- "culvert_shape"
cat3_main <- NULL
cat1_levels <- levels(as.factor(dsub$banks_y_n))
cat2_levels <- levels(as.factor(dsub$culvert_shape))
cat3_levels <- NULL

vmod_data <- dothedata(vmod_data)
vmod_jags_out <- runthemodel(vmod_data)
maketheplots(vmod_data, vmod_jags_out)


### see if I can generalize this to adapt to Vabs if needed
### also include data plots


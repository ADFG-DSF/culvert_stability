# Creating a dataset that represents the subset of variables we care about
# Creating some additional variables as re-expressions of others
# Performing an exploratory data analysis of the effects of all variables
#   (by themselves) on all possible instability scores


source("R/2_InstabilityScore_PCA.R")

write_output <- FALSE   # whether to write output to an external file

## Objects created in the source() call that we care about:
# Designs
# importancevec
# Vabs
# Vdir


###### -------- data subsetting, formatting, and creation of new variables

# converting from numeric to factor
Designs$bank_design_type <- as.character(Designs$bank_design_type)

# creating a new dataset where importance is "high"
Designs_high <- Designs[, importancevec %in% "high"]

# -- defining a few new variables -- #
# categorical version of design cr
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

# -- defining a few new variables -- #
# categorical version of design cr
Designs_medhigh$reach3_design_cr_cat <- Designs_high$reach3_design_cr_cat

# inconsistency in gradient & width
aa <- 0.2
# Designs_medhigh$grad_diff_2vs3 <- log((aa+Designs_medhigh$reach_2_gradient)/
#                                         (aa+Designs_medhigh$reach_3_gradient_51))
Designs_medhigh$grad_diff_2vs3 <- ifelse(Designs_medhigh$reach_2_gradient/
                                           Designs_medhigh$reach_3_gradient_51 > 1.05, ">greater",
                                         ifelse(Designs_medhigh$reach_3_gradient_51/
                                                  Designs_medhigh$reach_2_gradient > 1.05, "<less",
                                                "=same"))
# plot(Designs_medhigh$grad_diff_2vs3)
# Designs_medhigh$grad_diff_4vs3 <- -log((aa+Designs_medhigh$reach_3_gradient_51)/
#                                           (aa+Designs_medhigh$reach_4gradient))
Designs_medhigh$grad_diff_4vs3 <- ifelse(Designs_medhigh$reach_4gradient/
                                           Designs_medhigh$reach_3_gradient_51 > 1.05, ">greater",
                                         ifelse(Designs_medhigh$reach_3_gradient_51/
                                                  Designs_medhigh$reach_4gradient > 1.05, "<less",
                                                "=same"))
# plot(Designs_medhigh$grad_diff_4vs3)
# Designs_medhigh$width_diff_2vs3 <- log(Designs_medhigh$x2_width_straight/
#                                          Designs_medhigh$x3_width_straight)
Designs_medhigh$width_diff_2vs3 <- ifelse(Designs_medhigh$x2_width_straight/
                                         Designs_medhigh$x3_width_straight > 1.05, ">greater",
                                         ifelse(Designs_medhigh$x3_width_straight/
                                                  Designs_medhigh$x2_width_straight > 1.05,
                                                "<less","=same"))
# Designs_medhigh$width_diff_4vs3 <- -log(Designs_medhigh$x3_width_straight/
#                                          Designs_medhigh$x4_width_straight)
Designs_medhigh$width_diff_4vs3 <- ifelse(Designs_medhigh$x4_width_straight/
                                            Designs_medhigh$x3_width_straight > 1.05, ">greater",
                                          ifelse(Designs_medhigh$x3_width_straight/
                                                   Designs_medhigh$x4_width_straight > 1.05,
                                                 "<less","=same"))

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


# see if any variables are fully repeated (spoiler alert: one is)
repeated <- NULL
for(i in 1:(ncol(Designs_medhigh)-1)) {
  for(j in (i+1):ncol(Designs_medhigh)) {
    if(i != j) {
      if(all(Designs_medhigh[,i]==Designs_medhigh[,j]) & all(is.na(Designs_medhigh[,i])==is.na(Designs_medhigh[,j]))) {
        repeated <- c(repeated, j)
      }
    }
  }
}
repeated
Designs_medhigh <- Designs_medhigh[,-repeated]





#### --------------- EDA for all instability scores vs all variables (NUMERIC)


# ## creating a plotting function to plot ALL instability scores vs ALL variables
# magicplot <- function(x, y, main,...) {
#   # assuming y is numeric
#   # pval <- summary(lm(y~x))$coefficients[2,4]
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

parmar <- par("mar")

# considering absolute instability
par(mfrow=c(3,4))
for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    par(mar=c(8,5,4,2))
    magicplot(x=Designs_medhigh[,i],
              y=Vabs[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vabs)[j])
  }
}

# considering directional instability
par(mfrow=c(3,4))
for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    par(mar=c(8,5,4,2))
    magicplot(x=Designs_medhigh[,i],
              y=Vdir[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vdir)[j])
  }
}



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



#### --------------- EDA for all instability scores vs all variables (CATEGORICAL)


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



# ## defining a new plotting function to plot (categorical) instability vs all variables
#
# magicplot_cat <- function(x, y, main, ylab, ...) {
#   # assuming y is categorical
#
#   if(class(x) %in% c("factor","character")) {
#     # mosaicplot with addl NA and sample sizes for columns
#     thetable1 <- table(x,y)
#     pval <- chisq.test(thetable1)$p.value
#     x <- as.character(x)
#     x[is.na(x)] <- "_NA_"
#     thetable2 <- table(x,y)
#     dimnames(thetable2)[[1]] <- paste0(dimnames(thetable2)[[1]],
#                                        " (n = ", table(x),")")
#     mosaicplot(thetable2, xlab=main, ylab="",# labs are new
#                main=c(ylab, paste("chi^2 pval =", round(pval, 4))), #was main
#                ...=...)
#
#   } else {
#     #
#     # NAval <- min(x, na.rm=TRUE) - 0.2*diff(range(x, na.rm=TRUE))
#     # x[is.na(x)] <- NAval
#     # plot(y ~ x,
#     #      main=c(main, paste("Reg pval =", round(pval, 4))),
#     #      xlab="", col=1+(x==NAval),
#     #      ...=...)
#     # axis(side=1, at=NAval, labels=paste0("NA (n=", sum(x==NAval),")"), las=2)
#     # pval <- summary(lm(x~y))$coefficients[2,4]
#
#     # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
#     pval <- anova(lm(x~y))$`Pr(>F)`[1]
#
#     y <- as.character(y)
#     # y[is.na(y)] <- "_NA_"
#     thetab <- table(y)
#     theplotnames <- boxplot(x~y, plot=FALSE)$names
#     namestoplot <- rep(NA, length(theplotnames))
#     for(inames in seq_along(namestoplot)) {
#       namestoplot[inames] <- paste0(theplotnames[inames], " (n = ",
#                                     sum(y==theplotnames[inames], na.rm=TRUE), ")")
#     }
#     boxplot(x ~ y,
#             main=c(ylab, paste("Anova pval =", round(pval, 4))), # was main
#             xlab="",
#             ylab=main, # new
#             names = namestoplot,
#             # names = paste0(names(thetab), " (n = ", thetab, ")"),
#             las=2,
#             ...=...)
#   }
# }

# # considering absolute instability
# for(j in 1:4) {
#   par(mfrow=c(3,4))
#   par(mar=c(8,5,4,2))
#   for(i in 1:ncol(Designs_medhigh)) {
#     magicplot_cat(x=Designs_medhigh[,i],
#               y=Vabs_cat[,j],
#               main=names(Designs_medhigh)[i],
#               ylab=names(Vabs_cat)[j],
#               col=c(3,2))
#   }
# }
#
# # considering directional instability
# for(j in 1:4) {
#   par(mfrow=c(3,4))
#   par(mar=c(8,5,4,2))
#   for(i in 1:ncol(Designs_medhigh)) {
#     magicplot_cat(x=Designs_medhigh[,i],
#                   y=Vdir_cat[,j],
#                   main=names(Designs_medhigh)[i],
#                   ylab=names(Vdir_cat)[j],
#                   col=c(4,3,2))
#   }
# }

# considering absolute instability
par(mfrow=c(3,4))
par(mar=c(8,5,4,2))
for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    magicplot_cat(x=Designs_medhigh[,i],
                  y=Vabs_cat[,j],
                  main=names(Designs_medhigh)[i],
                  ylab=names(Vdir_cat)[j],
                  col=c(3,2))
  }
}

# considering directional instability
par(mfrow=c(3,4))
par(mar=c(8,5,4,2))
for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    magicplot_cat(x=Designs_medhigh[,i],
                  y=Vdir_cat[,j],
                  main=names(Designs_medhigh)[i],
                  ylab=names(Vdir_cat)[j],
                  col=c(4,3,2))
  }
}




### structuring a little differently

par(mfrow=c(4,4))
for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    par(mar=c(8,5,4,2))
    magicplot(x=Designs_medhigh[,i],
              y=Vdir[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vdir)[j])
    magicplot(x=Designs_medhigh[,i],
              y=Vabs[,j],
              main=names(Designs_medhigh)[i],
              ylab=names(Vdir)[j])
    magicplot_cat(x=Designs_medhigh[,i],
                  y=Vdir_cat[,j],
                  main=names(Designs_medhigh)[i],
                  ylab=names(Vdir_cat)[j],
                  col=c(4,3,2))
    magicplot_cat(x=Designs_medhigh[,i],
                  y=Vabs_cat[,j],
                  main=names(Designs_medhigh)[i],
                  ylab=names(Vdir_cat)[j],
                  col=c(3,2))
  }
}




#### --------------- Creating a summary table of all single test p-values


# maybe summarize this in a single table
# rows = variable (32 total)
# columns = (test type & n, pval ) x (numeric vs cat) x (abs vs dir) x type

magictester <- function(x, y) {
  n <- sum(!is.na(x) & !is.na(y))
  if(class(y) %in% c("character", "factor")) {
    if(class(x) %in% c("character", "factor")) {
      # chi^2
      thetab <- table(x,y, useNA="no")
      # n <- sum(thetab)
      pval <- chisq.test(thetab)$p.value
      method <- "X"
    } else {
      # anova x ~ y
      # pval <- summary(lm(x ~ y))$coefficients[2,4]

      # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
      pval <- anova(lm(x~y))$`Pr(>F)`[1]

      method <- "A"
    }
  } else {
    if(class(x) %in% c("character", "factor")) {
      # anova y ~ x
      # pval <- summary(lm(y ~ x))$coefficients[2,4]

      # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
      pval <- anova(lm(y~x))$`Pr(>F)`[1]

      method <- "A"
    } else {
      # regression y ~ x
      # pval <- summary(lm(y ~ x))$coefficients[2,4]

      # pval <- summary(lm(y~x))$coefficients[2,4]   WRONG!!
      pval <- anova(lm(y~x))$`Pr(>F)`[1]

      method <- "LR"
    }
  }
  return(list(pval=pval, method=paste(method,n)))
}

# p_v1_num_abs <- p_v1_num_dir <- p_v1_cat_abs <- p_v1_cat_dir <-
#   p_v2_num_abs <- p_v2_num_dir <- p_v2_cat_abs <- p_v2_cat_dir <-
#   p_v3_num_abs <- p_v3_num_dir <- p_v3_cat_abs <- p_v3_cat_dir <-
#   p_v4_num_abs <- p_v4_num_dir <- p_v4_cat_abs <- p_v4_cat_dir <- NA
# tn_v1_num_abs <- tn_v1_num_dir <- tn_v1_cat_abs <- tn_v1_cat_dir <-
#   tn_v2_num_abs <- tn_v2_num_dir <- tn_v2_cat_abs <- tn_v2_cat_dir <-
#   tn_v3_num_abs <- tn_v3_num_dir <- tn_v3_cat_abs <- tn_v3_cat_dir <-
#   tn_v4_num_abs <- tn_v4_num_dir <- tn_v4_cat_abs <- tn_v4_cat_dir <- NA
p_num_abs <- p_num_dir <- p_cat_abs <- p_cat_dir <-
  matrix(nrow=ncol(Designs_medhigh), ncol=4)
n_num_abs <- n_num_dir <- n_cat_abs <- n_cat_dir <-
  matrix(nrow=ncol(Designs_medhigh), ncol=4)

for(i in 1:ncol(Designs_medhigh)) {
  for(j in 1:4) {
    t1 <- magictester(x=Designs_medhigh[,i], y=Vabs[,j])
    t2 <- magictester(x=Designs_medhigh[,i], y=Vdir[,j])
    t3 <- magictester(x=Designs_medhigh[,i], y=Vabs_cat[,j])
    t4 <- magictester(x=Designs_medhigh[,i], y=Vdir_cat[,j])
    p_num_abs[i,j] <- t1$pval
    p_num_dir[i,j] <- t2$pval
    p_cat_abs[i,j] <- t3$pval
    p_cat_dir[i,j] <- t4$pval
    n_num_abs[i,j] <- t1$method
    n_num_dir[i,j] <- t2$method
    n_cat_abs[i,j] <- t3$method
    n_cat_dir[i,j] <- t4$method
  }
}
pvaltable <- data.frame(n_num_abs[,1], p_num_abs[,1],
                        n_num_dir[,1], p_num_dir[,1],
                        n_cat_abs[,1], p_cat_abs[,1],
                        n_cat_dir[,1], p_cat_dir[,1])
for(j in 2:4) {
  pvaltable <- cbind(pvaltable,
                     data.frame(n_num_abs[,j], p_num_abs[,j],
                                n_num_dir[,j], p_num_dir[,j],
                                n_cat_abs[,j], p_cat_abs[,j],
                                n_cat_dir[,j], p_cat_dir[,j]))
}
colnames(pvaltable) <-
  apply(expand.grid(c("n","p"), c("abs","dir"), c("num","cat"), 1:4),
      1, \(x) paste(x, collapse="_"))

rownames(pvaltable) <- names(Designs_medhigh)
if(write_output) {
  write.csv(pvaltable, file="Output/raw_pvaltable.csv")
}
par(mar=parmar)


## from here:
# look for good stories
# MLR on subset with data (dredge routine!)
# regression tree, maybe use subset and see if it matters
# - see if I can find a better plot method
# logistic regression: stable/unstable, unstable negative, unstable positive (again dredge)
# whatever I find, check out loocv predictive power!!
# ... or, dredge to the best loocv predictive power
# figure out fastest way to dredge



###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 26 Feb 2014
# 
# plot_W145_allResults.R
# 
# This script is to get main Figure 2.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script.
# PlotCIandLoess(1)
# 
# input data in folder data/:
# 1. output/M49/cis_M49_full.rda
# 2. interim/dataset_S5all.csv
# 3. output/M49/Results_SexRatioIGMEandIHME.csv
# 4. inputforpaper/Hill1995_table1(SexRatio).csv
#
# output plots in fig/paper/:
# 1. paper_W1and4and5M49log.pdf - main Figure 2;
#
###############################################################################

####################
## log scale of Q ##
pdf(paste0(paperplot.dir, "paper_W1and4and5", runname, "log.pdf"), 
    height = 9, width = 21)
par(mfrow = c(1, 3), cex.lab = 3, cex.axis = 2.5, mar = c(7, 7, 3, 1), 
    cex.main = 3, mgp = c(4.4, 1.2, 0), tcl = -0.4, las = 1)
## plot W1/4/5 ~ log(Q1/4/5) and CI for W1/4/5 ##
xlim      <- c(300 / Qunit, 5 / Qunit)
ylim      <- c(0.8, 1.5)

########################
## W1/4 and log(Q1/4) ##
for (age in ages.a[-3]) {
  a <- which(ages.a == age)
  
  PlotCIandLoess (
    x = log(res[[paste0("Q", age, ".i")]][agecat.i == age]),
    y = s.i[agecat.i == age], labelx = labelu,
    labelxlocation = labellogu, main = ageGroupName.a[a],
    xlab = "Total mortality rate*1,000 (log-scale)",
    ylab = "Sex ratio", xlim = log(xlim), ylim = ylim, 
    if.redoXaix = TRUE, if.CI = TRUE, cutoff = cutoffS,
    CI = res[[paste0("W", age, ".qk")]], CIx = res[[paste0("logQ", age, ".k")]]
  )  
}#end of age loop

####################
## W5 and log(Q5) ##
xlim       <- c(400 / Qunit, 5 / Qunit)
labelu5    <- union(setdiff(labelu, 300), 400)
labellogu5 <- log(1 / Qunit * labelu5)
a          <- A
age        <- ages.a[a]

plot(x = log(res[[paste0("Q", age, ".i")]][agecat.i == ages.a[a]]),
     y = s.i[agecat.i == ages.a[a]], main = ageGroupName.a[a],
     type = "n", xlim = log(xlim), ylim = ylim,
     xlab = "Total mortality rate*1,000 (log-scale)",
     ylab = "Sex ratio", xaxt = "n")
axis(1, at = labellogu5, label = labelu5, las = 1)
abline(h = cutoffS, lwd = 3)
## create full dataset for observed s5 ##
# normal data cleaning procedures, 
# but keep those excluded becuase of the existence of s1 or s4.
data.alls5  <- read.csv("data/interim/dataset_S5all.csv")
allagecat.i <- data.alls5[, "agecat.i"]
allQ5.i     <- data.alls5[, "Q5.i"]
alls.i      <- data.alls5[, "Sex.Ratio"]

points(x = log(allQ5.i[allagecat.i == ages.a[a]]), 
       y = alls.i[allagecat.i == ages.a[a]], 
       pch = 19, cex = 0.5, col = "lavenderblush3")
curve(predict(loess(c(res[["W5.cqt"]][, 2, ]) ~ log(c(Q5.ct)), 
                    na.action = "na.omit", family = "symmetric"), x), 
      add = TRUE, col = "purple", lwd = 7)

## IHME S5 comparable country-years ##
## read in data for comparable country-years for IGME and IHME S5 ##
tmp <- read.csv(paste0(output.dir, "Results_SexRatioIGMEandGBD.csv"))
xtemp <- log(c(tmp[, "U5MR.IHME"]))
Q <- quantile(xtemp, probs = c(0.05,0.95), na.rm = TRUE)
curve(predict(loess(c(tmp[, "SexRatio.IHME"]) ~ log(c(tmp[, "U5MR.IHME"])), 
                    na.action = "na.omit", family = "symmetric"), x), 
      add = TRUE, col = "royalblue", lwd = 7, from = Q[1], to = Q[2])

## Hill and Upchurch 1995 ##
hill <- read.csv("data/inputforpaper/Hill1995_table1(SexRatio).csv")
## compute male-female sr (original is female-male)
sr5 <- 1 / hill[,"SR.q5"]
## compute total q based on sr
q5m.hill <- hill[, "q5.male"]
q5.hill <- GetQbothfromQmale(qmale = q5m.hill, s = sr5) 
lines(x = log(q5.hill), y = sr5, col = "indianred2", lwd = 7, lty = 1)

## Jamison 2013 Figure 4 ##
S5.Jamision2013 <- 1.18
# GetQbothfromQmale(qmale = 59 / Qunit, s = S5.Jamision2013) #54.6/1000
# abline(v = log(res[["Q5.cqt"]][name.c == "India", 2, years.t == 2005.5]),
#        lwd = 0.1, col = "grey")
# abline(h = S5.Jamision2013, lwd = 0.1, col = "lightgrey")
points(x = log(res[["Q5.cqt"]][name.c == "India", 2, years.t == 2005.5]),
       y = S5.Jamision2013, pch = 19, cex = 3, col = "darkgreen")

legend("bottomright", 
       c("Bayesian model", "GBD (2012)", 
         "Hill and Upchurch (1995)", "Jamison et al, 2013"), 
       col = c("purple", "royalblue", "indianred2", "darkgreen"), 
       lty = c(1, 1, 1, NA), pch = c(NA, NA, NA, 19), lwd = 7, cex = 2.2)
dev.off()

## THE END ##


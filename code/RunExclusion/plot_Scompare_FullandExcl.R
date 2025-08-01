

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_CountryCIcompare_FullandExcl.R
# 
# This script is to compare W (against logQ) between Main.run and Excl.run.
#
# used for which run: Excl.run
#
# this script is called by any other scripts: main49_excl_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIandLoess(2)
# 
# input data in folder data/output:
# 1. /M49_excl/cis_M49_excl.rda
# 2. /M49/cis_M49_full.rda
#
# output plots in folder fig/M49_excl/
# 1. S1and4and5_compare_M49_exclCoutlier.pdf
# 2. S1and4and5_compare_M49_exclCoutlier_log.pdf
# 
###############################################################################


## W comparison plot for full run and excluding outlying countries ##
xlim <- c(0.3,0.003)
ylim <- c(0.9, 1.3)

pdf(paste0(fig.dir, "S1and4and5_compare_", runname, ".pdf"), height = 9, width = 21)
par(mfrow = c(1, 3), cex.lab = 2, cex.axis = 2, mar = c(7, 7, 1, 1), cex.main = 3)
## s1/4 and Q1/4 ##
for (age in ages.a[-A]) {
  PlotCIandLoess (
    x = res.excl[[paste0("Q", age, ".i")]][agecat.i == age], 
    y = s.i[agecat.i == age], labelx = labelu,
    labelxlocation = labellogu,
    xlab = paste0("Q", age, "*1000"), ylab = paste0("S", age), xlim = xlim, ylim = ylim, 
    if.redoXaix = TRUE, if.loess = TRUE, if.CI = TRUE, cutoff = 1,
    CI = res.excl[[paste0("W", age, ".qk")]],
    CIx = exp(res.excl[[paste0("logQ", age, ".k")]])
  )
  lines(res.full[[paste0("W", age, ".qk")]][2, ] ~ 
          exp(res.full[[paste0("logQ", age, ".k")]]), col = "royalblue", lwd = 3)
  polygon(c(res.full[[paste0("W", age, ".qk")]][1, ],
            rev(res.full[[paste0("W", age, ".qk")]][3, ]),
            res.full[[paste0("W", age, ".qk")]][1, 1]) ~ 
            exp(c(res.full[[paste0("logQ", age, ".k")]],
                  rev(res.full[[paste0("logQ", age, ".k")]]),
                  res.full[[paste0("logQ", age, ".k")]][1])),
          col = rgb(65, 105, 225, alpha = 100, maxColorValue = 255), border = NA)
  legend("bottomleft", c("full", "exclude extreme country"),
         lty = 1, col = c("royalblue", "purple"), lwd = 7, cex = 2)
  
}#end of age loop

## s5 and Q5 ##
## remove the loess for W5 (because S5 is driven more by W1 and W4)
age <- ages.a[A]
plot(x = Q5.i[agecat.i == age], y = s.i[agecat.i == age],
     type = "n", xlim = xlim, ylim = ylim,
     xlab = "Q5*1000", ylab = "S5", xaxt = "n")
axis(1, at = labellogu, label = labelu, las = 1)
abline(h = 1)
curve(predict(loess(c(res.excl[[paste0("W", age, ".cqt")]][, 2, ]) ~ c(Q5.ct), 
                    na.action = "na.omit", family = "symmetric"), x), 
      add = TRUE, col = "purple", lwd = 8)
curve(predict(loess(c(res.full[[paste0("W", age, ".cqt")]][, 2, ]) ~ 
                      log(c(res.full[["Q5.cqt"]][, 2, ])), 
                    na.action = "na.omit",family = "symmetric"), x),
      add = TRUE, col = "royalblue", lwd = 4)

legend("bottomleft", c("full", "exclude extreme country"),
       lty = 1, col = c("royalblue", "purple"), lwd = 7, cex = 2)

dev.off()


###############
## log-scale ##
pdf(paste0(fig.dir, "S1and4and5_compare_", runname, "_log.pdf"), height = 9, width = 21)
par(mfrow = c(1, 3), cex.lab = 2, cex.axis = 2, mar = c(7, 7, 1, 1), cex.main = 3)
## s1/4 and log(Q1/4) ##
for (age in ages.a[-A]) {
  PlotCIandLoess (
    x = log(res.excl[[paste0("Q", age, ".i")]][agecat.i == age]),
    y = s.i[agecat.i == age], labelx = labelu, labelxlocation = labellogu,
    xlab = paste0("Q", age, "*1000 (log-scale)"), ylab = paste0("S", age),
    xlim = log(xlim), ylim = ylim, 
    if.redoXaix = TRUE, if.loess = FALSE, if.CI = TRUE, cutoff = 1,
    CI = res.excl[[paste0("W", age, ".qk")]], CIx = res.excl[[paste0("logQ", age, ".k")]]
  )
  
  lines(res.full[[paste0("W", age, ".qk")]][2,] ~ 
          res.full[[paste0("logQ", age, ".k")]], col = "royalblue", lwd = 3)
  polygon(c(res.full[[paste0("W", age, ".qk")]][1, ],
            rev(res.full[[paste0("W", age, ".qk")]][3, ]),
            res.full[[paste0("W", age, ".qk")]][1, 1]) ~ 
            c(res.full[[paste0("logQ", age, ".k")]],
              rev(res.full[[paste0("logQ", age, ".k")]]),
              res.full[[paste0("logQ", age, ".k")]][1]),
          col = rgb(65, 105, 225, alpha = 100, maxColorValue = 255), border = NA)
  legend("bottomleft", c("full", "exclude extreme country"),
         lty = 1, col = c("royalblue", "purple"), lwd = 7, cex = 2)
  
}#end of age loop

## s5 and log(Q5) ##
age <- ages.a[A]
plot(x = log(Q5.i[agecat.i == age]), y = s.i[agecat.i == age],
     type = "n", xlim = log(xlim), ylim = ylim,
     xlab = "Q5*1000 (log-scale)", ylab = "S5", xaxt = "n")
axis(1, at = labellogu, label = labelu, las = 1)
abline(h = 1, lwd = 3)
curve(predict(loess(c(res.excl[[paste0("W", age, ".cqt")]][, 2, ]) ~ log(c(Q5.ct)), 
                    na.action = "na.omit", family = "symmetric"), x), 
      add = TRUE, col = "purple", lwd = 8)
curve(predict(loess(c(res.full[[paste0("W", age, ".cqt")]][, 2, ]) ~ 
                      log(c(res.full[["Q5.cqt"]][, 2, ])),
                    na.action = "na.omit",family = "symmetric"), x), 
      add = TRUE, col = "royalblue", lwd = 4)

legend("bottomleft", c("full", "exclude extreme country"),
       lty = 1, col = c("royalblue", "purple"), lwd = 7, cex = 2)

dev.off()

## the end ##


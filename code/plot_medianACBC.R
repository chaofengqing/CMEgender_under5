

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_medianACBC.R
# 
# This script plots the medians for AR-related parameters.
#
# used for which run: Main.run
#
# this script is called by any other scripts: construct_removeCountry.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: data/output/M49/medianInfo.rda
#
# output plot in folder fig/M49/:
# 1. postmedian_acbc_M49.pdf
# 2. postmedian_ACBCandQ_M49.pdf
# 
###############################################################################

load(file = paste0(output.dir,"medianInfo.rda")) #medianInfo
postmedianAC <- medianInfo[["postmedianAC"]]
postmedianBC <- medianInfo[["postmedianBC"]]
medianQ1c    <- medianInfo[["medianQ1c"]]
medianQ4c    <- medianInfo[["medianQ4c"]]


pdf(paste0(fig.dir, "postmedian_acbc_", runname, ".pdf"))
par(cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
hist(postmedianAC, xlab = "", ylab = "Density", main = "median for a.c",
     breaks = 40, col = "grey", border = "black", freq = FALSE)
abline(v = median(postmedianAC), col = "red")
abline(v = mean(postmedianAC), col = "blue")
legend("topleft", c(paste0("median=", round(median(postmedianAC), 6)),
                   paste0("mean=", round(mean(postmedianAC), 6))),
       col = c("red", "blue"), lty = 1)

hist(postmedianBC, xlab = "", ylab = "Density", main = "median for b.c",
     breaks = 40, col = "grey", border = "black", freq = FALSE)
abline(v = median(postmedianBC), col = "red")
abline(v = mean(postmedianBC), col = "blue")
legend("topleft", c(paste0("median=", round(median(postmedianBC), 6)),
                   paste0("mean=", round(mean(postmedianBC), 6))),
       col = c("red", "blue"), lty = 1)

par(cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(postmedianBC ~ postmedianAC, xlab = "a", ylab = "b", 
     main = paste("correlation is", round(cor(postmedianBC, postmedianAC), 2)))
select <- ifelse(postmedianAC > 0.1, TRUE, FALSE)
points(postmedianBC[select] ~ postmedianAC[select], pch = 19, col = 2)
text(postmedianBC[select] ~ postmedianAC[select], label = name.c[select], pos = 1)
dev.off()


######################################################
## Posterior medians of a.c/b.c vs Q1/4 per country ##
pdf(paste0(fig.dir, "postmedian_ACBCandQ_", runname, ".pdf"))
par(mfrow = c(2, 2))

plot(postmedianAC ~ medianQ1c, pch = 19)
curve(predict(loess(postmedianAC ~ medianQ1c, na.action = "na.omit",
                    family = "symmetric"), x), 
      add = TRUE, col = "darkgreen", lwd = 5)
plot(postmedianBC ~ medianQ4c, pch = 19)
curve(predict(loess(postmedianBC ~ medianQ4c, na.action = "na.omit",
                    family = "symmetric"), x), 
      add = TRUE, col = "darkgreen", lwd = 5)

plot(postmedianAC ~ log(medianQ1c), pch = 19)
curve(predict(loess(postmedianAC ~ log(medianQ1c), na.action = "na.omit",
                    family = "symmetric"), x), 
      add = TRUE, col = "darkgreen", lwd = 5)
plot(postmedianBC ~ log(medianQ4c), pch = 19)
curve(predict(loess(postmedianBC ~ log(medianQ4c), na.action = "na.omit",
                    family = "symmetric"), x), 
      add = TRUE, col = "darkgreen", lwd = 5)

dev.off()

## the end ##


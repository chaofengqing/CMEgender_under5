

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_removeCountry.R
# 
# This script select countries to be excluded in Excl.run
#
# used for which run: Main.run
#
# this script is called by any other scripts: main48_output.R
#
# this script calls other scripts: plot_medianACBC.R
#
# functions called: null
# 
# input data: data/output/M49/mcmc.array_M49.rda
#
# output data in folder data/output/M49/:
# 1. medianInfo.rda
# 2. rmvCountry.rda
# output plot in folder fig/M49/: are constructed in plot_medianACBC.R
# 1. postmedian_acbc_M49.pdf
# 2. postmedian_ACBCandQ_M49.pdf
# 
###############################################################################

######################################
## median for posterior a.c and b.c ##
acpar <- paste0("a.c[", seq(1, C), "]")
bcpar <- paste0("b.c[", seq(1, C), "]")

postmedianAC <- rep(NA, length(acpar))
count <- 0
for (parname in acpar) {
  count <- count + 1
  postmedianAC[count] <- median(c(mcmc.array[, , parname]))
}

postmedianBC <- rep(NA, length(bcpar))
count <- 0
for (parname in bcpar) {
  count <- count + 1
  postmedianBC[count] <- median(c(mcmc.array[, , parname]))
}

######################################################
## Posterior medians of a.c/b.c vs Q1/4 per country ##
medianQ1c <- medianQ4c <- rep(NA, C)

for (c in 1:C) {
  medianQ1c[c] <- median(
    Q1.i[iso.i == paste(iso.c[c]) & !is.na(s.i) & agecat.i == ages.a[1]])
  medianQ4c[c] <- median(
    Q4.i[iso.i == paste(iso.c[c]) & !is.na(s.i) & agecat.i == ages.a[2]])
}

medianInfo <- list(postmedianAC = postmedianAC, postmedianBC = postmedianBC,
                   medianQ1c = medianQ1c, medianQ4c = medianQ4c)
save(medianInfo, file = paste0(output.dir,"medianInfo.rda"))

if (First.run) {
  source("code/plot_medianACBC.R")
}

###############################
## remove outlying countries ##
mean(medianInfo$postmedianAC < medCutoffExclRun)
mean(medianInfo$postmedianBC < medCutoffExclRun)
mean(medianInfo$postmedianBC < medCutoffExclRun | 
       medianInfo$postmedianAC < medCutoffExclRun)

rmvISO.c <- iso.c[medianInfo[["postmedianBC"]] < medCutoffExclRun | 
                    medianInfo[["postmedianAC"]] < medCutoffExclRun]

rmvname.c <- name.c[medianInfo[["postmedianBC"]] < medCutoffExclRun | 
                      medianInfo[["postmedianAC"]] < medCutoffExclRun]
# note: data on China is limited in this run, so it doesn't show up
rmvCountry <- list(rmvISO.c = rmvISO.c, rmvname.c = rmvname.c)
save(rmvCountry, file = paste0(output.dir, "rmvCountry.rda"))

## the end ##


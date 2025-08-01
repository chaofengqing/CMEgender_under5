

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# source_countryCIdataSetup_getW.R
# 
# This script get the posterior samples of W, the golbal sex ratio based on
# levels of IMR, CMR, and U5MR.
#
# used for which run: Main.run; Excl.run; Validation.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(2)
# GetS5fromQboth(1)
# 
# input data: data/output/M49/mcmc.array_runname.rda
#
# The following R objects are saved by construct_countryCIs_SaveResults(cqt).R.
# W4mean.k, W1mean.k, W5median.i, W1.qk, W4.qk
# in data/output/M49/countryCI/cis_M49_2012.rda for Main.run
# in data/output/runname/cis_runname_2012.rda for Excl.run and Validation.run.
# note: the above R objects are not country-specific. They only depend on level
# of mortality.
# 
###############################################################################


## W1 ##
W1.lk <- logW1.lk <- matrix(NA, L, k1)
for (k in 1:k1) {
  logW1.lk[, k] <-     c(mcmc.array[, , paste0("logf1.k[", k, "]")])
  W1.lk   [, k] <- exp(c(mcmc.array[, , paste0("logf1.k[", k, "]")]))
}#end of k loop

W1.qk <- SamplesToUI(W1.lk)
W1mean.k <- apply(W1.lk, 2, mean)

## W4 ##
W4.lk <- logW4.lk <- matrix(NA, L, k4)
for (k in 1:k4) {
  logW4.lk[, k] <-     c(mcmc.array[, , paste0("logf4.k[", k, "]")])
  W4.lk   [, k] <- exp(c(mcmc.array[, , paste0("logf4.k[", k, "]")]))
}#end of k loop

W4.qk <- SamplesToUI(W4.lk)
W4mean.k <- apply(W4.lk, 2, mean)

## W5 ##
W5.li <- matrix(NA, L, I)
for (h in 1:n5) {
  W1hat.i <- exp(c(mcmc.array[, , paste0("logf1.k[", getk1.i[geti.h5[h]], "]")]))
  W4hat.i <- exp(c(mcmc.array[, , paste0("logf4.k[", getk4.i[geti.h5[h]], "]")]))  
  W5.li[, geti.h5[h]] <- GetS5fromQboth(q1both = Q1.i[geti.h5[h]], 
                                        q4both = Q4.i[geti.h5[h]], 
                                        s1 = W1hat.i, s4 = W4hat.i)
}#end of h loop

W5median.i <- apply(W5.li, 2, median)

## the end ##

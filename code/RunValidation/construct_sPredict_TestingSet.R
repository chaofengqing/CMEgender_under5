

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_sPredict_TestingSet.R
# 
# This script is to get posterior samples for predictions of left-out data.
#
# used for which run: Validation.run
#
# this script is called by any other scripts: main49_vali_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(1)
# 
# input data in folder data/output/M49_vali/:
# 1. mcmc.array_M49_vali.rda
#
# output data in folder data/output/M49_vali/:
# 1. sPredict_M49_vali.rda   - posterior samples for predictions of leftout data
# 2. sPredictCI_M49_vali.rda - 90% CI of posterior samples for predictions of
#                              leftout data.
#
###############################################################################


## get indices: FOR LEFT OUT DATA ONLY!!
## get indices for included observations of age group 1,4 and 5 ##
gete.h1 <- seq(1, E)[agecat.e == ages.a[1]] 
gete.h4 <- seq(1, E)[agecat.e == ages.a[2]] 
gete.h5 <- seq(1, E)[agecat.e == ages.a[A]] 
n1e <- length(gete.h1)
n4e <- length(gete.h4)
n5e <- length(gete.h5)
sum(n1e,n4e,n5e) == E

t.e <- c.e <- getk4.e <- getk1.e <- rep(NA, E)

Q4.e[Q4.e < Q4LowerBound] <- Q4LowerBound
for (e in 1:E) {
  # find country-year for each index
  c.e[e] <- which(iso.c == iso.e[e])
  t.e[e] <- which(years.t == floor(year.e[e]) + 0.5)
}#end of e loop
# note:logQ4.k and logQ1.k are based on IGME estimates

for (e in gete.h5) {
  getk4.e[e] <- which((logQ4.k) == log(1 / Qunit * round(Qunit * Q4.e[e])))
  getk1.e[e] <- which((logQ1.k) == log(1 / Qunit * round(Qunit * Q1.e[e])))
}
for (e in gete.h4) {
  getk4.e[e] <- which((logQ4.k) == log(1 / Qunit * round(Qunit * Q4.e[e])))
}
for (e in gete.h1) {
  getk1.e[e] <- which((logQ1.k) == log(1 / Qunit * round(Qunit * Q1.e[e])))
}
##################################################################

## read in logP ##
logP1.ctl <- selectP$logP1.ctl
logP4.ctl <- selectP$logP4.ctl

## read in sigma1/4/5.s ##
sigma1.ls <- sigma4.ls <- sigma5.ls <- matrix(0, nr = L, nc = S)
# sigma.s[S]=0 for group 1,4,5
for (s in 1:(S - 1)) {
  sigma1.ls[, s] <- c(mcmc.array[, , paste0("sigma1.s[", s, "]")])
  sigma4.ls[, s] <- c(mcmc.array[, , paste0("sigma4.s[", s, "]")])
  sigma5.ls[, s] <- c(mcmc.array[, , paste0("sigma5.s[", s, "]")])
}#end of s loop

## read in df for t-distribution for logs5 ##
nu5.l <- c(mcmc.array[, , "nu5"])

## construct trajectory for left out data ##
# logW1.lk is from source_countryCIdataSetup_getW.R
sPredict.le <- matrix(NA, nr = L, nc = E)
for (e in 1:n1e) {
  for (l in 1:L) {
    set.seed(2013 + e * 1000 + l * 100)
    
    logs1hat.e <- logW1.lk[l, getk1.e[gete.h1[e]]] +
      logP1.ctl[c.e[gete.h1[e]], t.e[gete.h1[e]], l]
    
    sigma1.e   <- sqrt(sigma1.ls[l, source.e[gete.h1[e]]]^2 + logSE.e[gete.h1[e]]^2)
    sPredict.le[l, gete.h1[e]] <- exp(rnorm(n = 1, mean = logs1hat.e, sd = sigma1.e))
  }#end of l loop
}#end of e loop

for (e in 1:n4e) {
  for (l in 1:L) {
    set.seed(2013 + e * 1000 + l * 100)
    
    logs4hat.e <- logW4.lk[l, getk4.e[gete.h4[e]]] +
      logP4.ctl[c.e[gete.h4[e]], t.e[gete.h4[e]], l]
    
    sigma4.e <- sqrt(sigma4.ls[l, source.e[gete.h4[e]]]^2 + logSE.e[gete.h4[e]]^2)
    sPredict.le[l, gete.h4[e]] <- exp(rnorm(n = 1, mean = logs4hat.e, sd = sigma4.e))
  }#end of l loop
}#end of e loop

for (e in 1:n5e) {
  for (l in 1:L) {
    set.seed(2013 + e * 1000 + l * 100)
    
    s1hat.e <- exp(logW1.lk[l, getk1.e[gete.h5[e]]] +
                     logP1.ctl[c.e[gete.h5[e]], t.e[gete.h5[e]], l])
    s4hat.e <- exp(logW4.lk[l, getk4.e[gete.h5[e]]] +
                     logP4.ctl[c.e[gete.h5[e]], t.e[gete.h5[e]], l])
    
    sigma5.e <- sqrt(sigma5.ls[l, source.e[gete.h5[e]]]^2 + logSE.e[gete.h5[e]]^2)    
    s5hat.e  <- GetS5fromQboth(q1both = Q1.e[gete.h5[e]], q4both = Q4.e[gete.h5[e]], 
                              s1 = s1hat.e, s4 = s4hat.e)
    
    # logs.i[geti.h5[h]] ~ dt(logs5hat.i[geti.h5[h]], tau5.i[geti.h5[h]], nu5)    
    sPredict.le[l, gete.h5[e]] <- exp(rt(n = 1, df = nu5.l[l]) * sigma5.e + log(s5hat.e))
  }#end of l loop
}#end of e loop

sPredict.el <- t(sPredict.le)
save(sPredict.el, file = paste0(output.dir,"sPredict_",runname,".rda"))

sPredict.qe <- SamplesToUI(sPredict.le)
save(sPredict.qe, file = paste0(output.dir,"sPredictCI_",runname,".rda"))

# dim(sPredict.el) #1853 8640
# dim(sPredict.qe) #3 1853

## the end ##


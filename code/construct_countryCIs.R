

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_countryCIs.R
# 
# This script saves country-specific results: sex ratio (estimated and expected),
# and P multiplier.
#
# used for which run: Main.run; Validation.run; Excl.run
# note: for Main.run, first to get S, W, P first while waiting for output from
# construct_countryCIs_CountryTrajectories.R.
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetS5fromQboth(2)
# SamplesToUI(3)
# 
# input data: data/output/runname/mcmc.array_*.rda
#
# output data in folder data/output/runname/:
# 1. cis_runname.rda - country median and 90% CI 1950 - 2015 for S, W, P;
#
# note the indices in the output R object:
# *.cqt - country related; 
# 
###############################################################################


array.cqt                <- array(NA, c(C, Per, Tend))
dimnames(array.cqt)[[1]] <- name.c
dimnames(array.cqt)[[2]] <- percentiles
dimnames(array.cqt)[[3]] <- floor(years.t)

S1.cqt <- S4.cqt <- S5.cqt <- 
  P1.cqt <- P4.cqt <- P5.cqt <- 
  W1.cqt <- W4.cqt <- W5.cqt <- array.cqt

for (c in 1:C) {
  S1.lt <- S4.lt <- S5.lt <- 
    W1.lt <- W4.lt <- W5.lt <- 
    P1.lt <- P4.lt <- P5.lt <- 
    matrix(NA, L, Tend)
  
  for (t in 1:Tend) {
    P1.lt[, t] <- exp(c(selectP[["logP1.ctl"]][c, t, ]))
    P4.lt[, t] <- exp(c(selectP[["logP4.ctl"]][c, t, ]))
    
    ## W1 ##
    pick.1 <- 
      which(c(logQ1.k) == c(log(1 / Qunit * round(Qunit * Q1.ct[c, t]))))
    
    if (length(pick.1) == 1) {
      logW1.l <- c(mcmc.array[, , paste0("logf1.k[", pick.1, "]")])
    } else {
      logW1.l <- rep(NA, L)
    }
        
    ## W4 ##
    Q4.test <- Q4.ct[c, t]  
    
    if (is.na(Q4.test)) {
      logW4.l <- rep(NA, L)
    } else {
      if (Q4.test >= Q4LowerBound) {
        pick.4 <- which(c(logQ4.k) == c(log(1 / Qunit * round(Qunit * Q4.ct[c, t])))) 
        if (length(pick.4) == 1) {
          logW4.l <- c(mcmc.array[, , paste0("logf4.k[", pick.4, "]")])
        } else {
          logW4.l <- rep(NA, L)
        }    
      } else {
        pick.4 <- 1
        logW4.l <- c(mcmc.array[, , paste0("logf4.k[", pick.4, "]")])
      }
    }# end W4 stuff 
    
    W1.lt[, t] <- exp(logW1.l)
    W4.lt[, t] <- exp(logW4.l)
    W5.lt[, t] <- GetS5fromQboth(q1both = Q1.ct[c, t], q4both = Q4.ct[c, t], 
                                 s1 = W1.lt[, t], s4 = W4.lt[, t])
  }# end of t loop
  
  S1.lt <- P1.lt * W1.lt
  S4.lt <- P4.lt * W4.lt
  
  for (t in 1:Tend) {
    S5.lt[, t] <- GetS5fromQboth(q1both = Q1.ct[c, t], q4both = Q4.ct[c, t], 
                                s1 = S1.lt[, t], s4 = S4.lt[, t])
  }#end of t loop
  
  P5.lt <- S5.lt / W5.lt
  
  for (age in ages.a) {
    eval(parse(text = paste0("S", age, ".cqt[c, , ] <- SamplesToUI(S", age, ".lt)")))
    eval(parse(text = paste0("W", age, ".cqt[c, , ] <- SamplesToUI(W", age, ".lt)")))
    eval(parse(text = paste0("P", age, ".cqt[c, , ] <- SamplesToUI(P", age, ".lt)")))
  }#end of age loop
  
}#end of c loop

res.full <- list(
  logQ1.k = logQ1.k, logQ4.k = logQ4.k,  
  Q1.i = Q1.i, Q4.i = Q4.i, Q5.i = Q5.i,
  Q1.ct = Q1.ct, Q4.ct = Q4.ct, Q5.ct = Q5.ct,
  W4mean.k = W4mean.k, W1mean.k = W1mean.k, W5median.i = W5median.i,
  W1.qk = W1.qk, W4.qk = W4.qk, 
  P1.cqt = P1.cqt, P4.cqt = P4.cqt, P5.cqt = P5.cqt,
  S1.cqt = S1.cqt,S4.cqt = S4.cqt,S5.cqt = S5.cqt,
  W1.cqt = W1.cqt, W4.cqt = W4.cqt, W5.cqt = W5.cqt,
  iso.c = iso.c
)

save(res.full, file = paste(output.dir, "cis_", runname, ".rda", sep = ""))


## the end ##


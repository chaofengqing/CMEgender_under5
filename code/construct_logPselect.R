

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_logPselect.R
# 
# This script is to get trajectories for logP's for each year of the full
# observation period. In JAGS model, we only save those non-missing logP's.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# InternalGetARTrajectories(1)
# 
# input data: data/output/runname/mcmc.array_runname.rda
#
# output data: data/output/runname/selectP_runname.rda
#
###############################################################################


# note: here l refers to posterior sample
rho.l   <- c(mcmc.array[, , "rho"      ])
sigma.l <- c(mcmc.array[, , "sigma.eps"])
logP1.ctl <- logP4.ctl <- array(NA, c(C, Tend, L))

for (c in 1:C) {
  
  set.seed(c)
  
  for (age in ages.a[-A]) {
    
    a        <- which(ages.a[-A] == age)
    nt       <- ifelse(age == ages.a[1], nt1.c[c], nt4.c[c])
    logPname <- ifelse(age == ages.a[1], "logP1.ct", "logP4.ct") 
    gett.z   <- list(gett1.cz[c, ], gett4.cz[c, ])[[a]][1:nt]
    # note: easier to define P to be AR(1) around 0, 
    # and add constant separately!
    nameconstant       <- ifelse(age == ages.a[1], "a.c", "b.c")
    constant.l         <- c(mcmc.array[, , paste0(nameconstant, "[", c, "]")])
    logPminconstant.zl <- matrix(NA, nt, L)
    
    for (z in 1:nt) {
      logPminconstant.zl[z, ] <- (
        -constant.l + 
          c(mcmc.array[, , paste0(logPname, "[", c, ",", gett.z[z], "]")])
        )
    }#end of z loop
    
    logPminconstant.lt <- InternalGetARTrajectories(
      rho.s = rho.l, sigma.s = sigma.l, eps.is = logPminconstant.zl, 
      years.i = gett.z, start.year = 1, end.year = Tend 
    )
    
    if (age == ages.a[1]) {
      logP1.ctl[c, , ] <- (t(logPminconstant.lt) + 
                           matrix(rep(constant.l, each = Tend), Tend, L))
    }
    if (age == ages.a[2]) {
      logP4.ctl[c, , ] <- (t(logPminconstant.lt) + 
                           matrix(rep(constant.l, each = Tend), Tend, L))
    }
    
  }#end of age loop
}#end of c loop

selectP <- list(logP1.ctl = logP1.ctl, logP4.ctl = logP4.ctl)
save(selectP, file = paste0(output.dir, "selectP_", runname, ".rda"))

## the end ##


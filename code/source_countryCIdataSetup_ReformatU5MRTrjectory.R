

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# source_countryCIdataSetup_ReformatU5MRTrjectory.R
# 
# This script reformats IMR and U5MR trajectories from IGME child mortality
# project (not this project) in order to match to the trajectories of sex ratio
# etc. from this project.
#
# used for which run: Main.run; Excl.run; Validation.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetQ4fromQ15(1)
# GetSpreadSample(2)
# 
# input data: data/qtotal_trajectories/U5MRandIMRtrajectories.L_20130813.rda
#
# The following R objects will be used by
# construct_countryCIs_CountryTrajectories.R:
# Q1.ctj, Q4.ctj, Q5.ctj, Lbysex, selectl.j
# note: the index *.ctj are in the standard order after reformatting:
# c - in the order of name.c/iso.c
# t - in the order of years.t
# j - indices of selectl.j with length Lbysex. When getting sex-specific
# results, the trajectories from mcmc.array with length L need to transfer by
# *.l[selectl.j] to *.j in order to have the same length of the country-year
# specific trajectry Q1/4/5.ctj[c, t, ] with length Lbysex.
# 
###############################################################################


## input data: trajectories for IMR and U5MR from IGME 2013 are read in
load("data/qtotal_trajectories/U5MRandIMRtrajectories.L_20130813.rda")
#U5MRandIMRtrajectories.L

if (sum(!is.element(iso.c, U5MRandIMRtrajectories.L[["iso.c"]])) > 0) {
  print("STOP: missing qtotal trajectories for at least one country!")
  print("code does not deal with this!")
}
# possible that there are trajectories for extra countries
# (e.g. in test run where subset of countries were selected)

# find matching countries based on name.c/iso.c
order.c <- rep(NA, C)
for (c in 1:C) {
  order.c[c] <- which(U5MRandIMRtrajectories.L[["iso.c"]] == iso.c[c])
}#end of c loop

# find matching years based on years.t
order.t <- rep(NA, Tend)
for (t in 1:Tend) {
  order.t[t] <- which(U5MRandIMRtrajectories.L[["year.t"]] == years.t[t])
}#end of t loop


# note that dimension may be different from mcmc.array samples,
# so construct qmale and qfemale for the minimum of the two samples
Ltraj  <- dim(U5MRandIMRtrajectories.L[["u5mrfinal.ctj"]])[[3]]
Lbysex <- min(Ltraj, L)  # total numer of samples used by sex-specific results.

if (Lbysex < Ltraj) {

  selectfromtraj.j <- GetSpreadSample(L.in = Ltraj, L.out = Lbysex)
  selectl.j        <- seq(1, L) # no change
  
} else {
  
  selectfromtraj.j <- seq(1, Ltraj) # no change
  selectl.j        <- GetSpreadSample(L.in = L, L.out = Lbysex)
  
}#end of ifelse(Lbysex < Ltraj)

# note: use seq(extra thinning) instead of sample function in order to reduce
# autocorrelation in the samples, and optimize the number of independent
# samples used (a random sample may have more neighbouring samples which are
# more autocorrelated since they are obtained by the MCMC algorithm).

# note: IGME estimates are in values of Q*1000 so need to divide by
# 1000 (i.e. Qunit) first.
Q1.ctj <- 1 / Qunit *
  U5MRandIMRtrajectories.L[["imrfinal.ctj"]][order.c, order.t, selectfromtraj.j]
Q5.ctj <- 1 / Qunit *
  U5MRandIMRtrajectories.L[["u5mrfinal.ctj"]][order.c, order.t, selectfromtraj.j]

Q4.ctj <- array(NA, c(C, Tend, Lbysex))
for (c in 1:C) {
  for (t in 1:Tend) {
    Q4.ctj[c, t, ] <- GetQ4fromQ15(q1 = Q1.ctj[c, t, ], q5 = Q5.ctj[c, t, ])
  }#end of t loop  
}#end of c loop


## the end ##


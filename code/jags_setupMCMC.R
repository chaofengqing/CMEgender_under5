

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_setupMCMC.R
# 
# This script setup parameters, input data, initial values and JAGS model file
# for JAGS model.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_*.R
#
# this script calls other scripts: jags_writeJAGSmodel.R
#
# functions called: null
# 
# input data: most constants and vectors are defined in source_BasicSetup
#
# output data:
# 1. ChainIDs
# 2. mort.data
# 3. mort.inits
# 4. mort.parameters
# note: JAGS model file is created in jags_writeJAGSmodel.R
#
# JAGS model setup summary in five parts:
# part 1: assign sequence of ChainIDs based on runID
#
# part 2: specify parameters to be saved for JAGS output, i.e. MCMC array
#
# part 3: specify input data for JAGS model
#
# part 4: assign initial values of parameters for JAGS model
#
# part 5: write out JAGS model in txt format
# 
###############################################################################

##########
## part 1: assign sequence of ChainIDs based on runID
ChainIDs <- (runID - 1) * mcmc.chains + seq(1, mcmc.chains) # number of chains for aech script

##########
## part 2: specify parameters to be saved for JAGS output, i.e. MCMC array
para1 <- c("nu5",
           "sigma.a", "sigma.b",
           "sigma.14", "rho", "sigma.eps")
paraP1 <- paraP4 <- NULL

for (c in 1:C) {
  paraP1 <- c(paraP1, paste0("logP1.ct[", c, ",", gett1.cz[c, 1:nt1.c[c]], "]"))
  paraP4 <- c(paraP4, paste0("logP4.ct[", c, ",", gett4.cz[c, 1:nt4.c[c]], "]"))
}

sigmapars <- paste0("sigma", rep(ages.a, each = S - 1), ".s[", seq(1, S - 1), "]")
para2 <- c(sigmapars,
           paste0("b", rep(ages.a[-A], each = 2), ".k[", seq(1, 2), "]"),
           paste0("u1.q", "[", seq(1, Q.1), "]"),
           paste0("u4.q", "[", seq(1, Q.4), "]"),
           paste0("logf1.k[",  seq(1, k1),  "]"),
           paste0("logf4.k[",  seq(1, k4),  "]"),
           paraP1, paraP4)

paralist <- list(para1 = para1, para2 = para2)

pardata.hyper   <- NULL 
parnames.hyper  <- paralist$para1
global.mortpara <- paralist$para2
global.data     <- NULL

mort.parameters <- c(parnames.hyper, "a.c", "b.c", global.mortpara)

##########
## part 3: specify input data for JAGS model
mort.data <- c(
  list(
    # data
    gett1.cz = gett1.cz,
    gett4.cz = gett4.cz,
    nt4.c = nt4.c,
    nt1.c = nt1.c,
    
    # prior info
    Sigma0 = diag(pri.sigma.ab.gamma.rate),
    pri.u1.mean = pri.u1.mean,
    pri.u4.mean = pri.u4.mean,
    
    pri.b11.lower = pri.b11.lower, 
    pri.b41.lower = pri.b41.lower,
    pri.b11.upper = pri.b11.upper,
    pri.b41.upper = pri.b41.upper,
    pri.b12.lower = pri.b12.lower,
    pri.b42.lower = pri.b42.lower,
    pri.b12.upper = pri.b12.upper,
    pri.b42.upper = pri.b42.upper,
    
    pri.ac.mu = pri.ac.mu,
    pri.bc.mu = pri.bc.mu,
    pri.ac.v = pri.ac.v,
    pri.bc.v = pri.bc.v,
    pri.ac.tranc.upper = pri.ac.tranc.upper,
    pri.bc.tranc.upper = pri.bc.tranc.upper,
    
    pri.sigma.14.lower = pri.sigma.14.lower,
    pri.sigma.14.upper = pri.sigma.14.upper,
    
    pri.sigma.a.shape = pri.sigma.a.shape,
    pri.sigma.b.shape = pri.sigma.b.shape,
    
    pri.sigma.eps.lower = pri.sigma.eps.lower,
    pri.sigma.eps.upper = pri.sigma.eps.upper,
    pri.rho.lower = pri.rho.lower,
    pri.rho.upper = pri.rho.upper,
    
    pri.sigma1s.lower = pri.sigma1s.lower,
    pri.sigma4s.lower = pri.sigma4s.lower,
    pri.sigma5s.lower = pri.sigma5s.lower,
    pri.sigma1s.upper = pri.sigma1s.upper,
    pri.sigma4s.upper = pri.sigma4s.upper,
    pri.sigma5s.upper = pri.sigma5s.upper,
    
    pri.nu5.lower = pri.nu5.lower,
    pri.nu5.upper = pri.nu5.upper    
  ),
  
  global.data, #combine two lists
  list(
    t.i = t.i, ##AR & splines
    c.i = c.i,  
    BG.tm.1 = BG.tm.1, Z.tk.1 = Z.tk.1, Q.1 = Q.1,  ##splines
    BG.tm.4 = BG.tm.4, Z.tk.4 = Z.tk.4, Q.4 = Q.4,  ##splines
    C = C,
    getk4.i = getk4.i, getk1.i = getk1.i,
    k1 = k1, k4 = k4,   
    S = S, source.i = source.i,
    logs.i = logs.i,
    logSE.i = logSE.i,
    geti.h1 = geti.h1, n1 = n1,
    geti.h5 = geti.h5, n5 = n5,
    geti.h4 = geti.h4, n4 = n4,
    Q1.i = Q1.i, w1 = w1,
    Q4.i = Q4.i
  )
)

##########
## part 4: assign initial values of parameters for JAGS model
global.inits <- list(
  b1.k = c(runif(1, int.b11.lower, int.b11.upper), runif(1, int.b12.lower, int.b12.upper)),
  b4.k = c(runif(1, int.b41.lower, int.b41.upper), runif(1, int.b42.lower, int.b42.upper)),
  u1.q = runif(Q.1, int.u1.lower, int.u1.upper),
  u4.q = runif(Q.4, int.u4.lower, int.u4.upper),
  sigma.eps = runif(1, int.sigma.eps.lower, int.sigma.eps.upper)
)

mort.inits<- function () {
  a.c <- rnorm(C, int.ac.mean, int.ac.sd) 
  b.c <- rnorm(C, int.bc.mean, int.bc.sd) 
  c(list(a.c = ifelse(a.c < int.ac.lower.cutoff, int.ac.lower.cutoff, a.c),
         b.c = ifelse(b.c < int.bc.lower.cutoff, int.bc.lower.cutoff, b.c)
  ),
    global.inits
  )#combine two lists  
}

##########
## part 5: write out JAGS model in txt format
JAGSmodel.name <- paste0(runname, ".txt")
if (First.run) source("code/jags_writeJAGSmodel.R")

# if (First.run) {
#   # mort.data
#   save(mort.data, file = file.path(output.dir, "mortdata.rda"))
#   # write JAGS model
#   source("code/jags_writeJAGSmodel.R")  
# } else {
#   load(file = file.path(output.dir, "mortdata.rda"))
# }

## The End! ##


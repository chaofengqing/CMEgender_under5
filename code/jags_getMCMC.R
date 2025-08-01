

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_getMCMC.R
# 
# This script calls JAGS and run the JAGS model on several serves parallely.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_1/2/3/4.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: most information are from jags_setupMCMC.R.
#
# output data: data/output/runname/temp.JAGSobjects/* - stepwise JAGS output
# note: these output files will be combined in main*_output.R to get mcmc.array
#
###############################################################################

## RUN FOR JAGS ##

## for linux server only:
library(doMC)
registerDoMC()

## start MCMC

foreach(chain = ChainIDs) %dopar% {
  set.seed(2013 + chain * 1000)
  rnorm(chain)
  
  mod <- jags(data = mort.data, inits = mort.inits,
            parameters.to.save = mort.parameters,           
            model.file = paste0(output.dir, JAGSmodel.name),
            jags.seed = chain,
            n.chains = 1, n.iter = n.iter.perstep + mcmc.burnin,
            n.burnin = mcmc.burnin, n.thin = mcmc.thin, DIC = TRUE)
  
  i = 1 # index for which update
  mod.upd <- mod
  save(mod.upd, file = paste0(jagsStep.dir, "jags_mod", 
                             runname, chain, "update_", i, ".Rdata"))
  cat(paste0("MCMC results step ", 1, " for chain ", chain, 
             " written to folder ", jagsStep.dir), "\n")
  
  #--- update MCMC ----------
  if (N.STEPS > 1){
    for (i in 2:(N.STEPS)) {      
      
      mod.upd <- update(mod.upd, parameters.to.save = mort.parameters, 
                       n.iter = n.iter.perstep,
                       n.thin = mcmc.thin)
      save(mod.upd,file = paste0(jagsStep.dir, "jags_mod", 
                                 runname, chain, "update_", i, ".Rdata"))
      cat(paste("MCMC results step", i, "for chain", chain, 
                "written to folder temp.JAGSobjects/"), "\n")
      
    }#end of i loop (steps)
  }#end of if(step > 1) 
}#end of chain loop


## the end ##


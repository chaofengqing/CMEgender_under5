

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# main49_excl_output.R
# 
# This script is the master file to get all results for Excl.run. Run this
# script ONLY AFTER main49_excl*.R have all been run through.
#
# used for which run: Excl.run
#
# this script is called by any other scripts: null
#
# this script calls other scripts:
# 01. source_BasicSetup.R
# 02. source_DirectorySetup.R
# 03. source_dataSetup.R
# 04. jags_setupMCMC.R
# 05. jags_ConvergenceCheck.R
# 06. plot_postprior.R
# 07. construct_logPselect.R
# 08. source_countryCIdataSetup_getW.R
# 09. construct_countryCIs.R
# 10. RunExclusion/plot_Scompare_FullandExcl.R
# 11. RunExclusion/plot_CountryCIcompare_FullandExcl.R
# 12. check_reproducibility.R
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# ReadJagsOutput(1)
# GetLimitCountryData(1)
# 
# input data in folder data/:
# 1. interim/dataset_formodeling.csv - SR data base for Main.run; created by
#                                      source_dataCleaning.R (main48_1.R)
# 2. input/Results.Table_Final_20130812.csv - IGME 2013 estimates file
# 3. output/M49/rmvCountry.rda              - info of countries to be excluded
#                                             in Excl.run
# 4. output/M49_excl/temp.JAGSobjects/* - read in stepwise JAGS output
#
# output data in folder data/output/M49_excl/
# 1. mcmc.array_M49_excl.rda - MCMC array, i.e. resulting trajectory for SR model.
# 2. countryCI/cis_M49_excl.rda - country-specific S, W, P median with 90% CI.
# Note: only the main output data are listed here since it is a master script.
# The above output data may be created in other scripts which are called in
# this script.
# 
###############################################################################

rm(list=objects())

runnamewd <- "CMEgender"
runname   <- "M49_exclCoutlier" # exclude extreme countries from M49
runID     <- 2 # default is 1 to construct output; 2/3/4 to set First.run to be FALSE

Main.run       <- ifelse(runname == "M49",              TRUE, FALSE)
Validation.run <- ifelse(runname == "M49_vali",         TRUE, FALSE)
Excl.run       <- ifelse(runname == "M49_exclCoutlier", TRUE, FALSE)
getQbysex      <- Main.run  # get sex-specific mortality?
CleanData      <- FALSE  # only do once to avoid problems with sorting differences
First.run      <- ifelse(runID == 1, TRUE, FALSE) # construct mcmcarray and output?
DoMCMC         <- FALSE

#workdir <- "E:/Dropbox/ChildMortality/" #laptop
#workdir <- "D:/Dropbox/ChildMortality/" #working PC
workdir <- "~/Dropbox/ChildMortality/"     #iMac
#workdir <- "/Users/pinwheel/Dropbox/ChildMortality/" #MacBook
# workdir <- "/hpctmp/stavcf/" #hpctmp

setwd(file.path(workdir, runnamewd))

#---------------------------------------------------------------------
# setup constants, functions, libraries
source("code/source_BasicSetup.R")

# setup directories
source("code/source_DirectorySetup.R")

# setup database
dataset <- read.csv("data/interim/dataset_formodeling.csv", header = TRUE,
                    stringsAsFactors = FALSE, strip.white = TRUE)
est <- read.csv("data/input/Results.Table_Final_20130812.csv", header = TRUE,
                stringsAsFactors = FALSE, strip.white = TRUE)

source("code/source_dataSetup.R")

# setup MCMC settings
source("code/jags_setupMCMC.R")

#---------------------------------------------------------------------
if (First.run) {
  #####################################################
  ## PART 1 - construct MCMC array (primary results) ##
  
  ## step 1: read in MCMC step-chains and get MCMC array as a whole
  mcmc.array <- ReadJagsOutput(
    n.steps = N.STEPS, ChainNums = fullChainIDs,# read in totall number of chains!
    runname = runname, output.dir = output.dir
  )
  save(mcmc.array, file = paste0(output.dir, "mcmc.array_", runname, ".rda"))
  
  ## step 2: check model convergency
  load(paste0(output.dir, "mcmc.array_", runname, ".rda")) #mcmc.array
  L <- dim(mcmc.array)[1] * dim(mcmc.array)[2]
  
  ## convergence check ##
  source("code/jags_ConvergenceCheck.R") 
  source("code/plot_postprior.R")     
  
  ##########################################
  ## PART 2 - construct secondary results ##
  
  ## reconstruct all logP.ct's ##
  source("code/construct_logPselect.R")
  load(file = paste0(output.dir,"selectP_",runname,".rda"))#selectP
 
  ## construct country/region/world CIs ##
  source("code/source_countryCIdataSetup_getW.R")  
  source("code/construct_countryCIs.R")  
}#end of if (First.run)

## comparison plots for full and exclude runs ##
# exclusion run
load(file = paste0(output.dir,"cis_",runname,".rda")) #res.full
res.excl <- GetLimitCountryData(res.full)

# full run
load(file = "data/output/M49/cis_M49_full.rda") #res.full
res.full <- GetLimitCountryData(res.full) #results for [1950, floor(yearResult)]
# compare W
source("code/RunExclusion/plot_Scompare_FullandExcl.R")
#country-specific S and P compare
source("code/RunExclusion/plot_CountryCIcompare_FullandExcl.R")

# check reproducibility!
source("code/check_reproducibility.R")

## The End! ##


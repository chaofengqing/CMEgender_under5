

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# main48_4.R
# 
# This script is the master file to get MCMC array for Main.run. Run this
# script AFTER main48_1.R.
#
# used for which run: Main.run
#
# this script is called by any other scripts: null
#
# this script calls other scripts:
# 1. source_BasicSetup.R
# 2. source_DirectorySetup.R
# 3. source_dataSetup.R
# 4. jags_setupMCMC.R
# 5. jags_getMCMC.R
#
# functions called: null
# 
# input data in folder data/:
# 1. interim/dataset_formodeling.csv        - the SR database for modelling
# 2. input/Results.Table_Final_20130812.csv - IGME 2013 estimates file
#
# output data in folder data/:
# 1. output/M49/temp.JAGSobjects/ - stepwise JAGS trajectory output
#
###############################################################################

rm(list = objects())

runnamewd <- "CMEgender"
runname   <- "M49"
runID     <- 4 # defaults to 1 (set to 2,3, etc if you start several jobs) 

Main.run       <- ifelse(runname == "M49",              TRUE, FALSE)
Validation.run <- ifelse(runname == "M49_vali",         TRUE, FALSE)
Excl.run       <- ifelse(runname == "M49_exclCoutlier", TRUE, FALSE)
getQbysex      <- Main.run  # get sex-specific mortality?
First.run      <- ifelse(runID == 1, TRUE, FALSE)  # construct mcmcarray and output?
CleanData      <- ifelse(First.run & Main.run, TRUE, FALSE)  # only do once to avoid problems with sorting differences
DoMCMC         <- TRUE # get step-wise JAGS output?

workdir <- "/hpctmp/stavcf/"
setwd(file.path(workdir, runnamewd))

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


## STOP HERE FOR JAGS ##
if (DoMCMC) {
  source("code/jags_getMCMC.R")
}  

# The End!

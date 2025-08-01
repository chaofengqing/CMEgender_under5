

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# main48_1.R
# 
# This script is the master file to get MCMC array for Main.run. Run this
# script BEFORE main48_2/3/4.R. AFTER finish running main48_1/2/3/4.R, go
# to main48_output.R to get reulsts.
#
# used for which run: Main.run
#
# this script is called by any other scripts: null
#
# this script calls other scripts:
# 1. source_BasicSetup.R
# 2. source_DirectorySetup.R
# 3. source_dataCleaning.R - is only called once and is in this script.
# 4. source_dataSetup.R
# 5. jags_setupMCMC.R
# 6. jags_getMCMC.R
#
# functions called: function(2) means the function is called twice in this
# script. Functions called in the scripts listed above are not listed.
# GetInputData(1)
# 
# input data in folder data/input/:
# 1. datasetfinal20120624.csv         - inupt data from all countries
# 2. Data Entry-Post-CC_20130806.xlsx - updated data from several countries
# 3. Haiti Gabon All_20130807.xlsx    - updated data from Haiti and Gabon
# 4. Sudan_20130807.xlsx              - updated data from Sudan
# 5. Results.Table_Final_20130812.csv - IGME 2013 estimates file
#
# output data in folder data/:
# 1. interim/dataset_formodeling.csv - the SR database for modelling
# 2. output/M49/temp.JAGSobjects/    - stepwise JAGS trajectory output
# Note: only the main output data are listed here since it is a master script.
#
###############################################################################


rm(list = objects())

runnamewd <- "CMEgender"
runname   <- "M49"
runID     <- 1 # defaults to 1 (set to 2,3, etc if you start several jobs) 

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

if (CleanData) {

  # combine and reformat input data files in folder: data/input/
  dataraw <- GetInputData(
    inputBaseData.name = "datasetfinal20120624.csv",
    inputData.name1 = "Data Entry-Post-CC_20130806.xlsx",
    inputData.name2 = "Haiti Gabon All_20130807.xlsx",
    inputData.name3 = "Sudan_20130807.xlsx"
  )
  write.csv(dataraw, "data/interim/dataset_raw_2013.csv",
            row.names = FALSE, na = "")
  # read in IGME estimates for Qtotals: determines iso.c etc.
  est <- read.csv("data/input/Results.Table_Final_20130812.csv", header = TRUE,
                  stringsAsFactors = FALSE, strip.white = TRUE)
  
  # data cleaning
  source("code/source_dataCleaning.R")
  
  # final SR database
  write.csv(dataset, "data/interim/dataset_formodeling.csv", row.names = FALSE)
  # some observed SR entries will be removed in source_dataSetup.R
  # if IGME Q1/4/5 is NA for a certain country-year
  
}#end of if (CleanData)

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

## The End! ##


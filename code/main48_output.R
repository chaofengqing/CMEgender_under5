

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# main48_output.R
# 
# This script is the master file to get all results for Main.run. Run this
# script ONLY AFTER main48_*.R have all been run through.
#
# used for which run: Main.run
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
# 07. construct_removeCountry.R
# 08. construct_logPselect.R
# 09. source_countryCIdataSetup_getW.R
# 10. source_countryCIdataSetup_ReformatU5MRTrjectory.R
# 11. construct_countryCIs.R
# 12. construct_countryCIs_CountryTrajectories.R
# 13. construct_countryCIs_SaveResults(cqt).R
# 14. paper/construct_signExcessCountryIndices.R
# 15. paper/table_mainpaper.R
# 16. paper/table_appendix.R
# 17. paper/table_appendix_supplementary.R
# 18. plot_DataseriesandCountryCI_country.R
# 19. plot_DataseriesandCountryCI.R
# 20. plot_sign_P.R
# 21. plot_sign_Px.R
# 22. plot_sign_ExcessFemaleMortality.R
# 23. plot_sign_ExcessFemaleDeath.R
# 24. paper/plot_W145_allResults.R
# 25. paper/plot_paperLineSement.R
# 26. paper/construct_SexRatioGBD.R
# 27. paper/construct_fullS5observation.R
# 28. paper/plot_DataseriesandCountryCI_GBDcompare.R
# 29. check_reproducibility.R
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# ReadJagsOutput(1)
# GetLimitCountryData(1)
# GetResultExcel(4)
# 
# input data in folder data/:
# 1. interim/dataset_formodeling.csv - SR data base;
#                                      created by source_dataCleaning (main48_1.R)
# 2. input/Results.Table_Final_20130812.csv - IGME 2013 estimates file
# 3. output/M49/temp.JAGSobjects/* - read in stepwise JAGS output
#
# output data in folder data/output/M49/
# 1. mcmc.array_M49.rda - MCMC array, i.e. resulting trajectory for SR model.
# 2. cis_M49_full.rda   - the world/region/country median with 90% CI in order
#                         to get other tables and plots.
# 3. Results_*.csv      - excel tables summarizing country specific results.
# Note: only the main output data are listed here since it is a master script.
# The above output data may be created in other scripts which are called in
# this script.
# 
###############################################################################


rm(list = objects())

runnamewd <- "CMEgender"
runname   <- "M49"
runID     <- 2 # default is 1 to construct output; 2/3/4 to set First.run to be FALSE

Main.run       <- ifelse(runname == "M49",              TRUE, FALSE)
Validation.run <- ifelse(runname == "M49_vali",         TRUE, FALSE)
Excl.run       <- ifelse(runname == "M49_exclCoutlier", TRUE, FALSE)
getQbysex      <- Main.run  # get sex-specific mortality?
CleanData      <- FALSE  # only do once to avoid problems with sorting differences
First.run      <- ifelse(runID == 1, TRUE, FALSE) # construct mcmcarray and output?
DoMCMC         <- FALSE
#one-country run refer to: Dropbox\ChildMortality\code\main7_M7.R and 
# model2_splines_M7.txt

# workdir <- "E:/Dropbox/ChildMortality/" #laptop
# workdir <- "D:/Dropbox/ChildMortality/" #working PC
workdir <- "~/Dropbox/ChildMortality/"     #iMac
# workdir <- "/Users/pinwheel/Dropbox/ChildMortality/" #MacBook
# workdir <- "/hpctmp/stavcf/" #hpctmp

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

##-----------------------------------------------------------------------------
if (First.run) {
  #####################################################
  ## PART 1 - construct MCMC array (primary results) ##
  
  ## step 1: read in MCMC step-chains and get MCMC array as a whole
  mcmc.array <- ReadJagsOutput(
    n.steps = N.STEPS,
    ChainNums = fullChainIDs, # read in totall number of chains!
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
  
  ## remove countries ##
  # based on median for a.c, b.c and Q1 Q4 for each country
  source("code/construct_removeCountry.R")#rmvCountry
  
  ## reconstruct all logP.ct's ##
  source("code/construct_logPselect.R")
  load(file = paste0(output.dir, "selectP_", runname, ".rda"))#selectP
  
  ## construct country/region/world CIs ##
  # step 1: get W1/4/5
  source("code/source_countryCIdataSetup_getW.R")
  
  # step 2: read in and reformat IMR and U5MR trajectories
  source("code/source_countryCIdataSetup_ReformatU5MRTrjectory.R")      
  
  # step 3: construct trajectories for each country seperately
  source("code/construct_countryCIs.R") # get country-specific S, W, P first!
  source("code/construct_countryCIs_CountryTrajectories.R")
  
  # step 4: get country, regional, and global results
  #save in the format of *1/4/5.cqt
  source("code/construct_countryCIs_SaveResults(cqt).R")
  ## end of getting country/region/world CIs ##      
}

# only need results upto the year yearResult, so read in result for full
# observation period first. Then set output after yearResult to NA
# for year-related results.
# res[["*.cqt"]] - country related; 
# res[["*.rqt"]] - region related;
# res[["*.qt"]]  - world related.
load(file = paste0(output.dir, "cis_", runname, "_full.rda")) #res.full
res <- GetLimitCountryData(data.in = res.full)


##-----------------------------------------------------------------------------
############
## TABLES ##

## get csv's for sex-specific results: needs to load res in order to get excels
GetResultExcel(output.variable = "Qfemale")
GetResultExcel(output.variable = "Qmale")
GetResultExcel(output.variable = "Sex ratio")
GetResultExcel(output.variable = "P")

## get indices for countries with outlying sex ratios
# for tables in main paper and appendix.
source("code/paper/construct_signExcessCountryIndices.R")
## tables for main paper ##
source("code/paper/table_mainpaper.R") # table 1-3
## tables for Appendix (in Latex script) ##
source("code/paper/table_appendix.R")               # appendix table 1, 3, 4
source("code/paper/table_appendix_supplementary.R") # appendix table 5-10


##-----------------------------------------------------------------------------
###########
## PLOTS ##

## plot country/region/world CIs ##
# country-specific only (each country in a seperate plot)
source("code/plot_DataseriesandCountryCI_country.R")
# world, region, country combined plots
source("code/plot_DataseriesandCountryCI.R")

## sign plots ##
source("code/plot_sign_P.R")
source("code/plot_sign_Px.R")
source("code/plot_sign_ExcessFemaleMortality.R")
source("code/plot_sign_ExcessFemaleDeath.R")

## plot fitted W1 and W4 with loess, and compare W5 with other studies
source("code/paper/plot_W145_allResults.R")

# Line segment version of plots for paper
source("code/paper/plot_paperLineSement.R")

## country comparison plot of IGME and GBD ##
if (First.run) {
  source("code/paper/construct_SexRatioGBD.R")
  source("code/paper/construct_fullS5observation.R")
}
source("code/paper/plot_DataseriesandCountryCI_GBDcompare.R")

##-----------------------------------------------------------------------------
# check reproducibility!
source("code/check_reproducibility.R")

sink("code/paper/datasource.txt")
source("code/paper/table_appendix_supplementary_datasource.R", echo = TRUE)
sink()
## The End! ##


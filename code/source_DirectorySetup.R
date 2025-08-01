

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-5 mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# source_DirectorySetup.R
# 
# This script creates and assign all directories used in this project.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_*.R
#
# this script calls other scripts: null
# functions called:                null
# input data:                      null
# output data:                     null
#
# Folders that you need to create by yourself before you start this project:
# project folder: CMEgender/
# input data folder: CMEgender/data/input/
# input population data folder: CMEgender/data/population/
# input IGME mortality trajectory folder: CMEgender/data/qtotal_trajectories/
# input data only relate to paper output folder: CMEgender/data/inputforpaper/
#
# Folders that will be created after running this script for a certain run:
# 1. CMEgender/data/interim/
# 2. CMEgender/data/output/; and its subfolders
# 3. CMEgender/fig/; and its subfolders
#
###############################################################################

###############################
## assign project subfolders ##

## data-related ##
output.dir      <- paste0("data/output/", runname, "/")
jagsStep.dir    <- paste0(output.dir, "temp.JAGSobjects/") # to save step-wise JAGS output
popdata.dir     <- paste0("data/population/") # directory for population input data
countryTraj.dir <- paste0(output.dir, "countryTrajectory/") # directory for country-specific rda results

## figure-related ##
fig.dir          <- paste0("fig/", runname, "/") # root directory for all plots
paperplot.dir    <- paste0("fig/", "paper/") # plots for paper
convergeplot.dir <- paste0(fig.dir, "convergence/") # convergence plots
countryplot.dir  <- paste0(fig.dir, "countryplot/") # estiamtes are saved seperately for each country
Pplot.dir        <- paste0(fig.dir, "signplot_P/") # directory for P sign plots
Pxplot.dir       <- paste0(fig.dir, "signplot_Px/")         # directory for P sign plots
excQplot.dir     <- paste0(fig.dir, "signplot_excQfemale/") # directory for excess Qfemale sign plots
excDplot.dir     <- paste0(fig.dir, "signplot_excDfemale/") # directory for excess Dfemale sign plots
# for Country Consultation: estiamtes are saved seperately for each country
ccplot.dir       <- paste0(fig.dir, "countryplotscc/")

PplotExplore.dir <- paste0(Pplot.dir, "explore/") #FQ: remove the following?

####################
## create folders ##
if (First.run | CleanData) {
  
  ## subfolder for intermediate data ##
  dir.create("data/interim/", showWarnings = FALSE)
  
  ## output data folderss ##
  dir.create("data/output", showWarnings = FALSE)
  dir.create(output.dir, showWarnings = FALSE)
  dir.create(jagsStep.dir, showWarnings = FALSE)
  if (Main.run) {
    dir.create(countryTraj.dir, showWarnings = FALSE)
  }
  
  ## figure folders ##
  dir.create("fig", showWarnings = FALSE)
  dir.create(fig.dir, showWarnings = FALSE)
  dir.create(convergeplot.dir, showWarnings = FALSE)
  dir.create(paperplot.dir, showWarnings = FALSE)

  if (Main.run) {
    dir.create(countryplot.dir, showWarnings = FALSE)
    dir.create(ccplot.dir, showWarnings = FALSE)
    dir.create(Pplot.dir, showWarnings = FALSE)
    dir.create(Pxplot.dir, showWarnings = FALSE)  
    dir.create(PplotExplore.dir, showWarnings = FALSE)  
    dir.create(excQplot.dir, showWarnings = FALSE)
    dir.create(excDplot.dir, showWarnings = FALSE)    
  }
  
}#end of if (First.run | CleanData)

## the end ##



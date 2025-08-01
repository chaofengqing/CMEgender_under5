

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 26 Feb 2014
# 
# source_BasicSetup.R
# 
# This script sets up the basic stuff for all the rest script in code/ folder.
# 1. assign constants;
# 2. install packages and call libraries;
# 3. call function scripts.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_*.R
#
# this script calls other scripts:
# 1. F_GenderFunctions.R       - mortality-related computations;
# 2. F_DataReformatFunctions.R - reformat, combine data
# 3. F_ComputeFunctions.R      - splines, JAGS, convergence, etc. computations;
# 4. F_PlotFunctions.R         - plot
#
# functions called: null
# 
# input data: null; manually assign values in this script based on the nature
# of this project. Some values or entries may change if there are updates for
# input data.
#
# output data: null
#
###############################################################################

## indices ##
# a - age groups c(1,4,5)
# c - countries
# r - regions
# t - years
# l - MCMC array for sex ratios, L
# j - index to select IGME trajectories for IMR, CMR, U5MR to match SR mcmc.array
# k - grids for age group 1 and 4
# b - grids Q1male
# i - data input
# h1, h4, h5 - subset of data input


#######################
## part 1: constants ##
# age groups
ageGroupName.a <- c("IMR", "CMR", "U5MR")
ages.a         <- c(    1,     4,      5)
A              <- length(ages.a)

# regions
regions.r <- c("Developed regions",
               "Northern Africa", "Sub-Saharan Africa",
               "Eastern Asia", "Southern Asia", "South-eastern Asia",
               "Western Asia", "Caucasus and Central Asia",   
               "Latin America and the Caribbean", "Oceania")
R         <- length(regions.r)

# years
years.t      <- c(1950.5 : 2015.5) # set years max based on Q5 estimates
yearSex      <- c(1990.5 : 2012.5) # years plotted for sex-specific results
yearBoth     <- c(1950.5 : 2012.5) # years plotted for other results
yearResult   <- 2012.5             # get final result up to this year
startYearSex <- 1990.5             # starting year to save sex-specific results
yearLeftout  <- 2006               # Validation.run only: left out data in and beyond this year
Tend         <- length(years.t)
# note: the starting year to get sex-specific estimate is later than the
# beginning of the observation period 1950 since we do not have data for
# sufficient number of countries from 1950 to startYearSex, i.e. 1990.

# population/lifetable (used in Main.run only)
lifetablea4 <- 0.4

# data cleaning
extremeSR <- 0.2 # Remove SR outside [extremeSR, 1/extremeSR]

# mortality-related
Qunit <- 1000 #since IMR/CMR/U5MR are per 1000 live births

# splines
d              <- 2    # difference penalty setting
IntervalLength <- 0.3  # scale of logQ
Q4LowerBound   <- 1 / Qunit # constant splines below this value for CMR
Qstartsplines  <- 5 / Qunit # Q before which splines are held constant
Qpercentilesplinesend <- 0.95  # Q percentile (between 0 and 1) after which splines are held constant

SRB <- 1.05
w1  <- SRB / (1 + SRB)

logSEImpute <- 0.15  # imputed SE when SE is missing (for non-VR data)
# uncomment the next line to see the details of getting the value 0.15 for logSEImpute
# file.show("code/info/getSE.txt") # opens in a new window in R

# get expected Qfemale by GetExpQf()
maxW <- 1.3 # max(W1/4/5) from Main.run
minW <- 0.9 # min(W1/4/5) from Main.run

# median cutoff value to select countries to be excluded in Excl.run
medCutoffExclRun <- -0.1

# cutoff values
cutoffS <- 1 # sex ratio
cutoffP <- 1 # country-specifc multiplier; same cutoff applies to Px
cutoffQ <- 0 # IMR, CMR, U5MR (also to expected/excess/sex-specific)
cutoffD <- 0 # death (also to expected/excess/sex-specific)
excessQcutoffmedian <- 1 # cutoff value for outlying countries (* Qunit)

# percentiles to get UI: get 90% CI throughout this project since
# child mortality estimates and related outcomes are quite uncertain.
percentiles <- c(0.05, 0.5, 0.95) 
Per         <- length(percentiles)

# number of simulations
N <- 1000

# plot size in the unit of cm
cm <- 1 / 2.54

plotWhichP <- 30 # trace plot every 30 of logP: there are too many if plot all
plotWhichW <- 10 # trace plot for every 10 of logW; same reason as above

labelu <- c(1, 5, 10, 20, 50, 150, 300) # display x-axis on normal scale
labellogu <- log(1 / Qunit * labelu) # display x-axis on (log-scale * 1000)

# tables - number of significant digits to save (table_S5Q5maleexcDeath5.R)
preciSR <- 2
# preciSRchange <- preciSR - 2
preciQ  <- 1
preciD  <- 3
preciDchange <- preciD - 2

# MCMC array structure: ChainIDs is specified in jags_setupMCMC.R (need runID).
mcmc.chains    <- 6 
fullChainIDs   <- seq(1, 24) # totall number of chains for all scripts together
mcmc.burnin    <- 5000
N.STEPS        <- 30
n.iter.perstep <- 5000
mcmc.thin      <- 10
# # for a test, overwrite MCMC settings
# mcmc.chains    <- 2
# ChainIDs       <- seq(1, mcmc.chains) 
# mcmc.burnin    <- 2
# N.STEPS        <- 1
# n.iter.perstep <- 3
# mcmc.thin      <- 1 

# write JAGS model (jags_writeJAGSmodel.R)
NoTermPerLine <- 2 #shorten the length of long summition to seperate lines

# JAGS parameter initial values
int.b11.lower <- int.b41.lower <- 0
int.b11.upper <- int.b41.upper <- 0.3
int.b12.lower <- int.b42.lower <- -0.04
int.b12.upper <- int.b42.upper <- 0

int.u1.lower <- int.u4.lower <- -0.2
int.u1.upper <- int.u4.upper <- 0.2

int.sigma.eps.lower <- 0
int.sigma.eps.upper <- 0.05

int.ac.mean         <- int.bc.mean         <- 0
int.ac.sd           <- int.bc.sd           <- 0.1
int.ac.lower.cutoff <- int.bc.lower.cutoff <- -0.5

# JAGS parameter priors
pri.sigma.ab.gamma.rate <- c(sigma.a = 0.03^2, sigma.b = 0.04^2)

pri.u1.mean <- pri.u4.mean <- 0

pri.b11.lower <- pri.b41.lower <- 0
pri.b11.upper <- pri.b41.upper <- 0.3
pri.b12.lower <- pri.b42.lower <- -0.1
pri.b12.upper <- pri.b42.upper <- 0.1

pri.ac.mu          <- pri.bc.mu          <- 0
pri.ac.v           <- pri.bc.v           <- 3
pri.ac.tranc.upper <- pri.bc.tranc.upper <- log(1.6)

pri.sigma.14.lower <- 0
pri.sigma.14.upper <- 0.2

pri.sigma.a.shape <- pri.sigma.b.shape <- 0.5

pri.sigma.eps.lower <- 0
pri.sigma.eps.upper <- 0.05

pri.rho.lower <- 0
pri.rho.upper <- 1

pri.sigma1s.lower <- pri.sigma4s.lower <- pri.sigma5s.lower <- 0
pri.sigma1s.upper <- pri.sigma4s.upper <- pri.sigma5s.upper <- 2

pri.nu5.lower <- 3
pri.nu5.upper <- 50

## table for appendix (latex) ##
# BEFORE paper proof
# sig.ratio.syb <- "*"      # est/exp female mortality is sig diff from one
# sig.change.syb <- "\\dag" # change of S, Rx 1990-2012 is sig diff from zero
# sig.1990.syb <- "\\P"     # countries with outlying sex ratio in 1990 
# sig.2012.syb <- "\\S"     # countries with outlying sex ratio in 2012
# AFTER paper proof (2014-08-13)
sig.ratio.syb <- "\\ddag"      # est/exp female mortality is sig diff from one
sig.change.syb <- "\\S" # change of S, Rx 1990-2012 is sig diff from zero
sig.1990.syb <- "$\\star$"     # countries with outlying sex ratio in 1990 
sig.2012.syb <- "\\dag"     # countries with outlying sex ratio in 2012


####################################
## part 2: install/call libraries ##

# install the following R packages in order to run through this project code
# note: the commands of installing packages are commentted out because issues
# may occur when running on remove servers.
# install.packages("R2jags")
# install.packages("splines")
# install.packages("foreach")
# install.packages("mvtnorm")
# install.packages("MCMCpack")
# install.packages("doMC")
# install.packages("xlsx")
# install.packages("xtable")

library(R2jags) # for JAGS
library(splines)
library(foreach)
library(mvtnorm)
library(MCMCpack)
library(xlsx) # read in xlsx data file
library(xtable) # print out latex script in R
# library(doMC) #is loaded when getMCMC is called


###################################
## part 3: call function scripts ##
source("code/R/F_GenderFunctions.R")       # all functions for mortality relations
source("code/R/F_DataReformatFunctions.R") # all functions for data reformatting
source("code/R/F_ComputeFunctions.R")      # all functions for computing
source("code/R/F_PlotFunctions.R")         # all functions for plots

## the end ##
                                                                               

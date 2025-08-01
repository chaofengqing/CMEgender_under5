
###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_ErrorRelativeErrorCoverage_TestingSet.R
# 
# This script is to get error and relative error for testing set.
#
# used for which run: Validation.run
#
# this script is called by any other scripts: main49_vali_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetLimitCountryData(1)
# 
# input data in folder data/output/M49_vali/:
# 1. sPredictCI_M49_vali.rda;
# 2. cis_M49_vali.rda.
#
# output data in folder data/output/M49_vali/:
# 1. Results_CoverageAllLeftoutSummary_TestingSet.csv;
# 2. Results_CoverageOneperCountry_TestingSet.rda;
# 3. Results_CoverageOneperCountrySummary_TestingSet.xlsx
#
###############################################################################


## read in data ##
load(file = paste0(output.dir, "sPredictCI_", runname, ".rda")) #sPredict.qe
load(file = paste0(output.dir, "cis_", runname, ".rda")) #res.full
res <- GetLimitCountryData(res.full)

## create vectors ##
# s.e <- data.test$s.i #observed values for left-out data
s.l <- sPredict.qe[1, ] # lower bound for predicted left-out data
s.m <- sPredict.qe[2, ] # median for predicted left-out data
s.u <- sPredict.qe[3, ] # upper bound for predicted left-out data

## identify extreme medians and use mcmc posterior median to replace ##
e.select <- which(s.m >= 100)
for(e in e.select) {
  age <- agecat.e[e]
  select.c <- iso.c == iso.e[e]
  select.t <- years.t == year.e[e]
  s.m[e] <- res[[paste0("S", age, ".cqt")]][select.c, 2, select.t]
}#end of e loop
summary(s.m)

#################################################################
## mean/median for error, relative error, and abs of these two ##
Error <- s.e - s.m # error
RelativeError <- Error / s.e * 100 # relative error (%)

Error.Mean <- mean(Error)
Error.Median <- median(Error)
RelativeError.Mean <- mean(RelativeError)
RelativeError.Median <- median(RelativeError)

absError.Mean <- mean(abs(Error))
absError.Median <- median(abs(Error))
absRelativeError.Mean <- mean(abs(RelativeError))
absRelativeError.Median <- median(abs(RelativeError))

## coverage ##
Coverage.Below <- mean(s.e < s.l)
Coverage.Above <- mean(s.e > s.u)

leftout.summary <- c(Error.Mean, Error.Median,
                     RelativeError.Mean, RelativeError.Median,
                     absError.Mean, absError.Median,
                     absRelativeError.Mean, absRelativeError.Median,
                     Coverage.Below * 100, Coverage.Above * 100)

ErrorCoverage.summary <- data.frame(round(leftout.summary, 2))
rownames(ErrorCoverage.summary) <- c("mean.Error", "median.Error",
                                     "mean.RelativeError (%)", "median.RelativeError (%)",
                                     "mean.absError", "median.absError",
                                     "mean.absRelativeError (%)", "median.absRelativeError (%)",
                                     "coverage.below 90% PI (%)", "coverage.above 90% PI (%)")
colnames(ErrorCoverage.summary) <- "All left-out"
write.csv(ErrorCoverage.summary, 
          paste0(output.dir, "Results_CoverageAllLeftoutSummary_TestingSet.csv"))

ErrorCoverage.output <- cbind(data.frame(data.vali[Indicator.i == "TestingSet", ],
                                         s.Median = s.m, s.90PI.Lower = s.l, s.90PI.Upper = s.u,
                                         Error = Error, 
                                         RelativeError = RelativeError))
write.csv(ErrorCoverage.output, 
          paste0(output.dir, "Results_ErrorRelativeError_TestingSet.csv"), row.names = FALSE)

##note: the prob for E leftout data to fall outside 90% PI for 95% of the time
quantile(rbinom(N, size=E, prob = 0.05) / E, percentiles)

###############################################################################
## sample one leftout data per country with N repeated draws to get coverage ##
pick.nc <- matrix(NA, nr = N, nc = length(unique(iso.e))) #record the chosen leftout for each time
## all regions combined ##
Coverage.nq <- matrix(NA, nr = N, nc = 2) #FQ: Per - 1 ?
meanError.n <- medianError.n <- medianAbsError.n <- medianAbsRelativeError.n <- rep(NA, N)
## region-specific ##
pick.anc <- array(NA, c(A, N, C))
reg.e <- as.character(reg.e)
R <- length(unique(reg.e)) #FQ: caution, this overwrite R = 10 for Main.run in source_BasicSetup.R
Coverage.nqr <- array(NA, dim = c(N, 2, R))
meanError.nr <- medianError.nr <- medianAbsError.nr <- 
  medianAbsRelativeError.nr <- matrix(NA, nr = N, nc = R)

## age group specific ##
ageGrpSize.an <- matrix(NA, nr = A, nc = N)
ageGrpSize.anr <- array(NA, c(A, N, R))
Coverage.naq <- array(NA, c(N, A, 2))
meanError.na <- medianError.na <- medianAbsError.na <- 
  medianAbsRelativeError.na <- matrix(NA, nr = N, nc = A)
Coverage.naqr <- array(NA, dim = c(N, A, 2, R))
meanError.nar <- medianError.nar <- medianAbsError.nar <- 
  medianAbsRelativeError.nar <- array(NA, c(N, A, R))

for (n in 1:N) { #N repeated draws
  pick <- rep(NA, length(unique(iso.e)))
  set.seed(2013 + n * 100)
  
  for (j in 1:length(unique(iso.e))) {
    pick[j] <- sample(which(iso.e == unique(iso.e)[j]), size = 1) #sample one per country
  }#end of j loop
  pick.nc[n, ] <- pick
  
  ## all regions combined (all age groups) ##
  Coverage.nq[n, 1] <- mean(s.e[pick] < s.l[pick]) #Coverage.Below
  Coverage.nq[n, 2] <- mean(s.e[pick] > s.u[pick]) #Coverage.Above
  meanError.n[n]   <- mean(Error[pick])                           #mean.Error
  medianError.n[n] <- median(Error[pick])                         #median.Error
  medianAbsError.n[n] <- median(abs(Error)[pick])                 #median.AbsError
  medianAbsRelativeError.n[n] <- median(abs(RelativeError)[pick]) #median.AbsRelativeError
  
  ## region-specific (all age groups) ##
  for (r in 1:R) {
    pick.r <- pick[which(reg.e[pick] == unique(reg.e)[r])]
    
    Coverage.nqr[n, 1, r] <- mean(s.e[pick.r] < s.l[pick.r]) #Coverage.Below
    Coverage.nqr[n, 2, r] <- mean(s.e[pick.r] > s.u[pick.r]) #Coverage.Above
    meanError.nr[n, r]   <- mean(Error[pick.r])                           #mean.Error
    medianError.nr[n, r] <- median(Error[pick.r])                         #median.Error
    medianAbsError.nr[n, r] <- median(abs(Error)[pick.r])                 #median.AbsError
    medianAbsRelativeError.nr[n, r] <- median(abs(RelativeError)[pick.r]) #median.AbsRelativeError    
  }#end of r loop
  
  
  ########################
  ## age group specific ##
  for (age in ages.a) {
    a <- which(ages.a == age)
    pick.anc[a, n, 1:length(pick.age)] <- pick.age <- pick[agecat.e[pick] == age]
    ageGrpSize.an[a, n] <- length(pick.age)
    
    ## all regions combined ##
    Coverage.naq[n, a, 1] <- mean(s.e[pick.age] < s.l[pick.age]) #Coverage.Below
    Coverage.naq[n, a, 2] <- mean(s.e[pick.age] > s.u[pick.age]) #Coverage.Above
    meanError.na[n, a]   <- mean(Error[pick.age])                           #mean.Error
    medianError.na[n, a] <- median(Error[pick.age])                         #median.Error
    medianAbsError.na[n, a] <- median(abs(Error)[pick.age])                 #median.AbsError
    medianAbsRelativeError.na[n, a] <- median(abs(RelativeError)[pick.age]) #median.AbsRelativeError
    
    ## region-specific ##
    for (r in 1:R) {
      pick.ar <- pick[which(reg.e[pick.age] == unique(reg.e)[r])]
      ageGrpSize.anr[a, n, r] <- length(pick.ar)
      
      if (length(pick.ar) != 0) {
        Coverage.naqr[n, a, 1, r] <- mean(s.e[pick.ar] < s.l[pick.ar]) #Coverage.Below
        Coverage.naqr[n, a, 2, r] <- mean(s.e[pick.ar] > s.u[pick.ar]) #Coverage.Above
        meanError.nar[n, a, r]   <- mean(Error[pick.ar])                           #mean.Error
        medianError.nar[n, a, r] <- median(Error[pick.ar])                         #median.Error
        medianAbsError.nar[n, a, r] <- median(abs(Error)[pick.ar])                 #median.AbsError
        medianAbsRelativeError.nar[n, a, r] <- median(abs(RelativeError)[pick.ar]) #median.AbsRelativeError        
      }#end of if
    }#end of r loop    
  }#end of age loop  
}#end of n loop

Coverage.OneperCountry.df <- list(
  Coverage.nq = Coverage.nq * 100, Coverage.nqr = Coverage.nqr * 100, 
  pick.nc = pick.nc,
  meanError.n = meanError.n, meanError.nr = meanError.nr,
  medianError.n = medianError.n, medianError.nr = medianError.nr,
  medianAbsError.n = medianAbsError.n, medianAbsError.nr = medianAbsError.nr,
  medianAbsRelativeError.n = medianAbsRelativeError.n,
  medianAbsRelativeError.nr = medianAbsRelativeError.nr,
  ## age groups specific ##
  pick.anc = pick.anc, 
  ageGrpSize.an =ageGrpSize.an, ageGrpSize.anr = ageGrpSize.anr,
  Coverage.naq = Coverage.naq * 100, Coverage.naqr = Coverage.naqr * 100,
  meanError.na = meanError.na, meanError.nar = meanError.nar,
  medianError.na = medianError.na, medianError.nar = medianError.nar,
  medianAbsError.na = medianAbsError.na, medianAbsError.nar = medianAbsError.nar,
  medianAbsRelativeError.na = medianAbsRelativeError.na,
  medianAbsRelativeError.nar = medianAbsRelativeError.nar
)

save(Coverage.OneperCountry.df,
     file = paste0(output.dir, "Results_CoverageOneperCountry_TestingSet.rda"))


##################
## save to file ##
## all age groups combined ##
result.all <- apply(cbind(Coverage.nq * 100, #3.83 4.77
                          meanError.n, #0.0045
                          medianError.n, #0.0045
                          medianAbsError.n,#0.12
                          medianAbsRelativeError.n),#10.75
                    2, mean)

result.region <- matrix(NA, nr = 6, nc = R)
for (r in 1:R) {
  result.region[1:2, r] <- apply(Coverage.nqr[, , r] * 100, 2, mean)
  result.region[3  , r] <- mean(meanError.nr[, r])
  result.region[4  , r] <- mean(medianError.nr[, r])
  result.region[5  , r] <- mean(medianAbsError.nr[, r])
  result.region[6  , r] <- mean(medianAbsRelativeError.nr[, r])
}#end of r loop

n.reg <- N.reg <- rep(NA, R)
for (r in 1:R) {
  n.reg[r] <- sum(reg.e[pick] == unique(reg.e)[r])#number of countries within each region
  N.reg[r] <- sum(reg.e == unique(reg.e)[r])      #number of left-out data within each region
}#end of r loop
# sum(n.reg)==length(unique(iso.e))
n.result <- c(length(unique(iso.e)), n.reg)
N.result <- c(E, N.reg)
result.tmp <- cbind(result.all, result.region)
result.CoverageOneperCountry <- rbind(N.result, n.result, result.tmp)

rownames(result.CoverageOneperCountry) <- c(
  "number of left-out observations within each region",
  "number of countries within each regoin",
  "Left-out observations fall below 90% PI (%)",
  "Left-out observations fall above 90% PI (%)",
  "Mean of Error",
  "Median of Error",
  "Median of absolute Error",
  "Median of absolute Relative Error (%)"
  )
colnames(result.CoverageOneperCountry) <- c("all regions", unique(reg.e))
result.CoverageOneperCountry.allAge <- result.CoverageOneperCountry

#########################
## age groups specific ##
result.CoverageOneperCountry.age <- list(age1 = NULL, age4 = NULL, age5 = NULL)

for (age in ages.a) {
  a <- which(ages.a == age)
  result.all <- apply(cbind(Coverage.naq[, a, ] * 100,
                            meanError.na[, a],
                            medianError.na[, a],
                            medianAbsError.na[, a],
                            medianAbsRelativeError.na[, a]),
                      2, mean, na.rm = TRUE)
  
  result.region <- matrix(NA, nr = 6, nc = R)
  for (r in 1:R) {
    result.region[1:2, r] <- apply(Coverage.naqr[, a, , r] * 100, 2, mean, na.rm = TRUE)
    result.region[3  , r] <- mean(meanError.nar[, a, r], na.rm = TRUE)
    result.region[4  , r] <- mean(medianError.nar[, a, r], na.rm = TRUE)
    result.region[5  , r] <- mean(medianAbsError.nar[, a, r], na.rm = TRUE)
    result.region[6  , r] <- mean(medianAbsRelativeError.nar[, a, r], na.rm = TRUE)
  }#end of r loop
  
  n.reg <- N.reg <- rep(NA, R)
  for (r in 1:R) {
    n.reg[r] <- mean(ageGrpSize.anr[a, , r])                     #number of countries within each region
    N.reg[r] <- sum(reg.e == unique(reg.e)[r] & agecat.e == age) #number of left-out within each region (age group a)
  }#end of r loop
  n.result <- c(length(unique(iso.e[agecat.e == age])), n.reg)
  N.result <- c(sum(agecat.e == age), N.reg)
  result.tmp <- cbind(result.all, result.region)
  result.CoverageOneperCountry <- rbind(N.result, n.result, result.tmp)
  
  rownames(result.CoverageOneperCountry) <- c(
    "number of left-out observations within each region",
    "number of countries within each regoin",
    "Left-out observations fall below 90% PI (%)",
    "Left-out observations fall above 90% PI (%)",
    "Mean of Error",
    "Median of Error",
    "Median of absolute Error",
    "Median of absolute Relative Error (%)"
  )
  colnames(result.CoverageOneperCountry) <- c("all regions", unique(reg.e))
  result.CoverageOneperCountry.age[[a]] <- result.CoverageOneperCountry
}#end of age loop

## save the coverage summary for all age groups combined and for each age group ##
workbook <- createWorkbook() #create blank workbook
sheet1 <- createSheet(workbook, sheetName = "all age groups")
sheet2 <- createSheet(workbook, sheetName = "age group 1")
sheet3 <- createSheet(workbook, sheetName = "age group 4")
sheet4 <- createSheet(workbook, sheetName = "age group 5")
addDataFrame(data.frame(result.CoverageOneperCountry.allAge), sheet1)
addDataFrame(data.frame(result.CoverageOneperCountry.age[["age1"]]), sheet2)
addDataFrame(data.frame(result.CoverageOneperCountry.age[["age4"]]), sheet3)
addDataFrame(data.frame(result.CoverageOneperCountry.age[["age5"]]), sheet4)
saveWorkbook(workbook, paste0(output.dir, "Results_CoverageOneperCountrySummary_TestingSet.xlsx"))


## analysis for region: Caucasus and Central Asia
iso.region6 <- as.character(iso.e[pick])[reg.e[pick] == unique(reg.e)[6]] ## coutnries in region 6
n.isoregion6 <- rep(NA, n.result[6 + 1])
for (c in 1:n.result[6 + 1]) {
  n.isoregion6[c] <- sum(iso.e == iso.region6[c])
}#end of c loop

## the end ##




###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_ErrorRelativeErrorCoverage_TrainingSet.R
# 
# This script is to get error and relative error for training set.
#
# used for which run: Validation.run
#
# this script is called by any other scripts: main49_vali_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetLimitCountryData(2)
# 
# input data in folder data/output/M49_vali/:
# 1. cis_M49_full.rda;
# 2. cis_M49_vali.rda.
#
# output data in folder data/output/M49_vali/:
# 1. Results_CoveragePickYearSummary_TrainingSet.csv;
#
###############################################################################

## changes in estimates (training set) ##
load(file = paste0(output.dir, "cis_", runname, ".rda")) #res.full
res.vali <- GetLimitCountryData(res.full)
load(file = "data/output/M49/cis_M49_full.rda")#res.full
res.full <- GetLimitCountryData(res.full)

table(dim(res.vali[["S1.cqt"]]) == dim(res.full[["S1.cqt"]]))
table(res.vali[["iso.c"]] == res.full[["iso.c"]])

## only get coverage results for the year 2000 and 2005 ##
target.years <- c(2000, 2005)
t.select <- which(floor(years.t) == c(2000, 2005))

## matrix to store the coverage summary for all data ##
rowNAME.ErrorCoverage <- c("Mean of Error",
                           "Median of Error",
                           "Median of absolute Error",
                           "Full run prediction below 90% CI of validation run (%)",
                           "Full run prediction above 90% CI of validation run (%)")
colNAME.ErrorCoverage <- paste0(rep(paste0("age group ", ages.a), each = length(target.years)),
                                rep(paste0(" (", target.years, ")"), times = A))
ErrorCoverage.summary <- matrix(NA, nr = length(rowNAME.ErrorCoverage), 
                                nc = length(target.years) * A)
rownames(ErrorCoverage.summary) <- rowNAME.ErrorCoverage
colnames(ErrorCoverage.summary) <- colNAME.ErrorCoverage

## create seperate file for each target year ##
for (t in t.select) {  
  for (age in ages.a) {
    ## full run ##    
    s.m.f <- c(res.full[[paste0("S", age, ".cqt")]][, 2, t]) #median
    ## validation run ##    
    s.m.v <- c(res.vali[[paste0("S", age, ".cqt")]][, 2, t]) #median    
    s.l.v <- c(res.vali[[paste0("S", age, ".cqt")]][, 1, t]) #90% lower bound    
    s.u.v <- c(res.vali[[paste0("S", age, ".cqt")]][, 3, t]) #90% upper bound
    
    noNA <- which(!is.na(s.m.f) & !is.na(s.m.v))
    s.m.f <- s.m.f[noNA]
    s.m.v <- s.m.v[noNA]
    s.l.v <- s.l.v[noNA]
    s.u.v <- s.u.v[noNA]
        
    ## mean/median for error, relative error, and abs of these two ##
    Error <- s.m.f - s.m.v # error
    Error.Mean <- mean(Error)
    Error.Median <- median(Error)
    
    absError.Mean <- mean(abs(Error))
    absError.Median <- median(abs(Error))
    
    ## coverage ##
    Coverage.Below <- mean(s.m.f < s.l.v) * 100
    Coverage.Above <- mean(s.m.f > s.u.v) * 100
    
    col.posi <- which(paste0("age group ", age, " (", floor(years.t[t]), ")") == colNAME.ErrorCoverage)
    print(col.posi)
    ErrorCoverage.summary[, col.posi] <- c(Error.Mean, Error.Median, absError.Median,
                                          Coverage.Below, Coverage.Above)
  }#end of age loop
}#end of target.yr loop

## save coverage results for all data ##
write.csv(ErrorCoverage.summary, 
          paste0(output.dir, "Results_CoveragePickYearSummary_TrainingSet.csv"))



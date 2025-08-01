

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_signExcessCountryIndices.R
# 
# This script gets indices for world, regions, and countries with outlying
# sex ratio in 1990 and 2012 seperately with criteria:
# 1. excess female mortality is significantly different from zero, i.e. ratio
#    of estimated to expected female mortality is significantly different
#    from one;
# 2. absolute point estimate for excess female mortality is more than 1 per
#    1000 (this criterion is to exclude mostly developed countries where
#    differences are so small that they are meaningless).
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: null
# 
# input data: data/output/M49/cis_M49_full.rda.
#
# output data in output/M49/:
# 1. indicesforexcess.1.all.rda - outlying sex ratio for IMR;
# 2. indicesforexcess.4.all.rda - outlying sex ratio for CMR;
# 3. indicesforexcess.5.all.rda - outlying sex ratio for U5MR;
# Note: there are four components in each rda file:
# 1990m.all: sign in 1990, male excess
# 1990f.all: sign in 1990, female excess
# 2012m.all: sign in 2012, male excess
# 2012f.all: sign in 2012, female excess
# where .all means the vector is c(world, regions.r, name.c), instead of .r for
# regions only or .c for countries only.
#
###############################################################################


years.select <- c(1990, 2012) + 0.5

## world, region, and country outlying indices ##
for (age in ages.a) {
  a <- which(ages.a == age)
  
  excQf.names <- c("World", regions.r, name.c)
  excQf.ALLqt <- array(NA, c(length(excQf.names), Per, Tend))
  dimnames(excQf.ALLqt)[[1]] <- excQf.names
  excQf.ALLqt["World"  , , ] <- res[[paste0("excQ", age, "f.qt")]]  * Qunit
  excQf.ALLqt[regions.r, , ] <- res[[paste0("excQ", age, "f.rqt")]] * Qunit
  excQf.ALLqt[name.c   , , ] <- res[[paste0("excQ", age, "f.cqt")]] * Qunit
    
  for (year in years.select) {
    select.t <- (years.t == year)
    
    # include countries with UIs that do not contain zero
    signExcQf.all <- (excQf.ALLqt[, 1, select.t] > cutoffQ) |
      (excQf.ALLqt[, 3, select.t] < cutoffQ)
    
    # get countries with excess female mortality higher than +excessQcutoffmedian
    female.all <- excQf.ALLqt[, 2, select.t] > excessQcutoffmedian
    # get countries with excess male mortality higher than +excessQcutoffmedian
    male.all   <- excQf.ALLqt[, 2, select.t] < (-excessQcutoffmedian)
    
    eval(parse(text = paste0("f", floor(year), ".all <- female.all  & signExcQf.all")))
    eval(parse(text = paste0("m", floor(year), ".all <- male.all  & signExcQf.all")))
  }#end of year loop
  
  #   eval(parse(text = paste0()))
  list.names <- paste0(rep(c("f", "m"), each = length(years.select))
                       ,floor(years.select),
                       rep(".all", each = length(years.select) * 2))
  eval(parse(text = paste0("indicesforexcess.", age, ".all <- list(",
                           paste(list.names, collapse = ","), ")")))
  eval(parse(text = paste0("names(indicesforexcess.", age, ".all) <- list.names")))
}#end of age loop

save(indicesforexcess.1.all, file = paste0(output.dir, "indicesforexcess.1.all.rda"))
save(indicesforexcess.4.all, file = paste0(output.dir, "indicesforexcess.4.all.rda"))
save(indicesforexcess.5.all, file = paste0(output.dir, "indicesforexcess.5.all.rda"))

## the end ##


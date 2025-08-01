

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# table_mainpaper.R
# 
# This script is to get Table 1-3 in main paper.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script.
# ExternalMakeCountryNamesFull(3)
# StandardizeDecimal(37)
# StandardizeSignifDigit(1)
# 
# input data: data/output/M49/:
# 1. trajectory_M49_Rx.rda;
# 2. trajectory_M49_S.rda;
# 3. indicesforexcess.1.all.rda
# 4. indicesforexcess.4.all.rda
# 5. indicesforexcess.5.all.rda
#
# output data in /M49/: latex script of tables in Appendix Table 5-10
# 1. paper_table1_IMR.csv  - main paper Table 1;
# 2. paper_table1_CMR.csv  - main paper Table 1;
# 3. paper_table1_U5MR.csv - main paper Table 1; combine the three files
# 4. paper_table2_IMR.csv  - main paper Table 2;
# 5. paper_table2_CMR.csv  - main paper Table 2; combine the two files
# 6. paper_table3_U5MR.csv - main paper Table 3
#
###############################################################################

## indicies for outlying countries ##
load(file = paste0(output.dir, "indicesforexcess.1.all.rda")) #indicesforexcess.1.all
load(file = paste0(output.dir, "indicesforexcess.4.all.rda")) #indicesforexcess.4.all
load(file = paste0(output.dir, "indicesforexcess.5.all.rda")) #indicesforexcess.5.all

## S and Rx country/region/world trajectories ##
load(file = paste0(output.dir, "trajectory_", runname, "_Rx.rda")) #res.Rxtrajectory
load(file = paste0(output.dir, "trajectory_", runname, "_S.rda")) #res.Strajectory

t.select <- which(is.element(years.t, c(1990.5, 2012.5)))


###################
## paper table 1 ##
for (age in ages.a) {
  a <- which(ages.a == age)  
  
  table.colnames <- c(
    paste("Sex ratio", ageGroupName.a[a], "(1990)"), 
    paste("Sex ratio", ageGroupName.a[a], "(2012)"), 
    paste("Sex ratio", ageGroupName.a[a], "change (1990-2012)"),
    paste("Estimated/Expected female", ageGroupName.a[a], "(1990)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "(2012)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "change (1990-2012)")
  )
  table.rownames <- c("World", regions.r)
  table.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
  rownames(table.df) <- table.rownames
  colnames(table.df) <- table.colnames
  
  for (t in t.select) {
    S.median <- c(res[[paste0("S", age, ".qt" )]][          2, t],
                  res[[paste0("S", age, ".rqt")]][        , 2, t])
    S.lower <- c(res[[paste0("S", age, ".qt" )]][          1, t],
                 res[[paste0("S", age, ".rqt")]][        , 1, t])
    S.upper <- c(res[[paste0("S", age, ".qt" )]][          3, t],
                 res[[paste0("S", age, ".rqt")]][        , 3, t])
    
    table.df[, paste0("Sex ratio ", ageGroupName.a[a], " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(S.median, preciSR), " [", 
             StandardizeDecimal(S.lower, preciSR), "; ",
             StandardizeDecimal(S.upper, preciSR), "]")
    
  }#end of t loop
  
  S.jt  <- res.Strajectory[[paste0("S", age, ".jt")]]
  S.rjt <- res.Strajectory[[paste0("S", age, ".rjt")]]
  
  changeS.df <- cbind(
    quantile(S.jt[, t.select[2]] - S.jt[,    t.select[1]], percentiles, na.rm = TRUE),
    apply(S.rjt[, , t.select[2]] - S.rjt[, , t.select[1]],
          1, quantile, percentiles, na.rm = TRUE)
  )
  
  table.df[, paste("Sex ratio", ageGroupName.a[a], 
                   "change (1990-2012)")] <- 
    paste0(StandardizeDecimal(changeS.df[2, ], preciSR), " [", 
           StandardizeDecimal(changeS.df[1, ], preciSR), "; ",
           StandardizeDecimal(changeS.df[3, ], preciSR), "]")
  
  
  for (t in t.select) {
    Rx.median <- c(res[[paste0("Rx", age, ".qt" )]][  2, t],
                   res[[paste0("Rx", age, ".rqt")]][, 2, t])
    Rx.lower <- c(res[[paste0("Rx", age, ".qt" )]][  1, t],
                  res[[paste0("Rx", age, ".rqt")]][, 1, t])
    Rx.upper <- c(res[[paste0("Rx", age, ".qt" )]][  3, t],
                  res[[paste0("Rx", age, ".rqt")]][, 3, t])
    
    table.df[, paste0("Estimated/Expected female ", ageGroupName.a[a],
                      " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(Rx.median, preciSR), " [",
             StandardizeDecimal(Rx.lower, preciSR), "; ",
             StandardizeDecimal(Rx.upper, preciSR), "]")    
  }#end of t loop
  Rx.jt  <- res.Rxtrajectory[[paste0("Rx", age, ".jt")]]
  Rx.rjt <- res.Rxtrajectory[[paste0("Rx", age, ".rjt")]]
  
  changeRx.df <- cbind(
    quantile(Rx.jt[, t.select[2]] - Rx.jt[,    t.select[1]], percentiles, na.rm = TRUE),
    apply(Rx.rjt[, , t.select[2]] - Rx.rjt[, , t.select[1]],
          1, quantile, percentiles, na.rm = TRUE)
  )
  
  table.df[, paste("Estimated/Expected female", ageGroupName.a[a], 
                   "change (1990-2012)")] <- 
    paste0(StandardizeDecimal(changeRx.df[2, ], preciSR), " [", 
           StandardizeDecimal(changeRx.df[1, ], preciSR), "; ", 
           StandardizeDecimal(changeRx.df[3, ], preciSR), "]")
  
  write.csv(table.df, paste0(output.dir, "paper_table1", "_", 
                             ageGroupName.a[a], ".csv"), row.names = TRUE)
}#end of age loop


###################
## paper table 2 ##
# for IMR and CMR only
for (age in ages.a[-A]) {
  a <- which(ages.a == age)
  ## select countries with outlying sex ratio ##
  eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", age, ".all")))
  female.name <- name.c[
    (indicesforexcess[[paste0("f", floor(years.t[t.select[1]]), ".all")]] |
       indicesforexcess[[paste0("f", floor(years.t[t.select[2]]), ".all")]])[name.c]]
  all.name <- female.name
  
  c.select <- rep(NA, length(all.name)) 
  for (i in 1:length(all.name)) {
    c.select[i] <- which(name.c == all.name[i])
  }#end of i loop
  
  
  table.colnames <- c(
    paste("Sex ratio", ageGroupName.a[a], "(1990)"), 
    paste("Sex ratio", ageGroupName.a[a], "(2012)"), 
    paste("Sex ratio", ageGroupName.a[a], "change (1990-2012)"),
    paste("Excess female", ageGroupName.a[a], "per 1,000 (1990)"),
    paste("Excess female", ageGroupName.a[a], "per 1,000 (2012)"),
    paste("Estimated/Expected female", ageGroupName.a[a], "(1990)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "(2012)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "change (1990-2012)")
  )
  table.rownames <- ExternalMakeCountryNamesFull(name.c)[c.select]
  table.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
  rownames(table.df) <- table.rownames
  colnames(table.df) <- table.colnames
  
  for (t in t.select) {
    S.median <- res[[paste0("S", age, ".cqt")]][c.select, 2, t]
    S.lower <- res[[paste0("S", age, ".cqt")]][c.select, 1, t]
    S.upper <- res[[paste0("S", age, ".cqt")]][c.select, 3, t]
    
    table.df[, paste0("Sex ratio ", ageGroupName.a[a], " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(S.median, preciSR), " [", 
             StandardizeDecimal(S.lower, preciSR), "; ",
             StandardizeDecimal(S.upper, preciSR), "]")
    
  }#end of t loop
  
  S.clt <- res.Strajectory[[paste0("S", age, ".clt")]]
  
  changeS.df <- apply(S.clt[c.select, , t.select[2]] - S.clt[c.select, , t.select[1]],
                      1, quantile, percentiles, na.rm = TRUE)
  
  table.df[, paste("Sex ratio", ageGroupName.a[a], 
                   "change (1990-2012)")] <- 
    paste0(StandardizeDecimal(changeS.df[2, ], preciSR), " [", 
           StandardizeDecimal(changeS.df[1, ], preciSR), "; ",
           StandardizeDecimal(changeS.df[3, ], preciSR), "]")
  
  for (t in t.select) {
    excQf.median <- res[[paste0("excQ", age, "f.cqt")]][c.select, 2, t]
    excQf.lower  <- res[[paste0("excQ", age, "f.cqt")]][c.select, 1, t]
    excQf.upper  <- res[[paste0("excQ", age, "f.cqt")]][c.select, 3, t]
    
    table.df[, paste0("Excess female ", ageGroupName.a[a], 
                      " per 1,000 (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(excQf.median * Qunit, preciQ), " [", 
             StandardizeDecimal(excQf.lower * Qunit, preciQ), "; ", 
             StandardizeDecimal(excQf.upper * Qunit, preciQ), "]")
  }#end of t loop
  
  
  for (t in t.select) {
    Rx.median <- res[[paste0("Rx", age, ".cqt")]][c.select, 2, t]
    Rx.lower <- res[[paste0("Rx", age, ".cqt")]][c.select, 1, t]
    Rx.upper <- res[[paste0("Rx", age, ".cqt")]][c.select, 3, t]
    
    table.df[, paste0("Estimated/Expected female ", ageGroupName.a[a],
                      " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(Rx.median, preciSR), " [",
             StandardizeDecimal(Rx.lower, preciSR), "; ",
             StandardizeDecimal(Rx.upper, preciSR), "]")    
  }#end of t loop
  
  Rx.cjt <- res.Rxtrajectory[[paste0("Rx", age, ".cjt")]]
  
  changeRx.df <- apply(Rx.cjt[c.select, , t.select[2]] - Rx.cjt[c.select, , t.select[1]],
                       1, quantile, percentiles, na.rm = TRUE)
  
  table.df[, paste("Estimated/Expected female", ageGroupName.a[a], 
                   "change (1990-2012)")] <- 
    paste0(StandardizeDecimal(changeRx.df[2, ], preciSR), " [", 
           StandardizeDecimal(changeRx.df[1, ], preciSR), "; ", 
           StandardizeDecimal(changeRx.df[3, ], preciSR), "]")
  
  write.csv(table.df, paste0(output.dir, "paper_table2", "_", 
                             ageGroupName.a[a], ".csv"), row.names = TRUE)
}#end of age loop


###################
###################
## paper table 3 ##

# only for U5MR
age <- ages.a[A]
a <- which(ages.a == age)
## select countries with outlying sex ratio in 2012 only
t.select <- which(is.element(floor(years.t), 2012))
## select countries with outlying sex ratio ##
eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", age, ".all")))
include.c <- (indicesforexcess[[paste0("f", floor(years.t[t.select]), ".all")]] |
  indicesforexcess[[paste0("m", floor(years.t[t.select]), ".all")]])[name.c]

table.colnames <- c(
  paste("Sex Ratio", ageGroupName.a[a]),
  paste("Estimated/Expected female", ageGroupName.a[a]),
  paste("Excess female", ageGroupName.a[a], "(per 1000)"),
  paste("Excess female death", ageGroupName.a[a]),
  paste("% of death", ageGroupName.a[a])
)
table.rownames <- ExternalMakeCountryNamesFull(name.c)[include.c]

table.excDf.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
rownames(table.excDf.df) <- table.rownames
colnames(table.excDf.df) <- table.colnames


S.median <- res[[paste0("S", age, ".cqt")]][include.c, 2, t.select]
S.lower <- res[[paste0("S", age, ".cqt")]][include.c, 1, t.select]
S.upper <- res[[paste0("S", age, ".cqt")]][include.c, 3, t.select]
table.excDf.df[, paste("Sex Ratio", ageGroupName.a[a])] <- 
  paste0(StandardizeDecimal(S.median, preciSR), " [", 
         StandardizeDecimal(S.lower, preciSR), "; ",
         StandardizeDecimal(S.upper, preciSR), "]")

Rx.median <- res[[paste0("Rx", age, ".cqt")]][include.c, 2, t.select]
Rx.lower <- res[[paste0("Rx", age, ".cqt")]][include.c, 1, t.select]
Rx.upper <- res[[paste0("Rx", age, ".cqt")]][include.c, 3, t.select]
table.excDf.df[, paste("Estimated/Expected female", ageGroupName.a[a])] <- 
  paste0(StandardizeDecimal(Rx.median, preciSR), " [", 
         StandardizeDecimal(Rx.lower, preciSR), "; ",
         StandardizeDecimal(Rx.upper, preciSR), "]")

excQf.median <- res[[paste0("excQ", age, "f.cqt")]][include.c, 2, t.select]
excQf.lower <- res[[paste0("excQ", age, "f.cqt")]][include.c, 1, t.select]
excQf.upper <- res[[paste0("excQ", age, "f.cqt")]][include.c, 3, t.select]
table.excDf.df[, paste("Excess female", ageGroupName.a[a], "(per 1000)")] <- 
  paste0(StandardizeDecimal(excQf.median * Qunit, preciQ), " [", 
         StandardizeDecimal(excQf.lower * Qunit, preciQ), "; ", 
         StandardizeDecimal(excQf.upper * Qunit, preciQ), "]")

excDf.selectCqt <- res[[paste0("excD", age, "f.cqt")]][include.c, , ]
excDf.selectCq <- signif(round(excDf.selectCqt[, , t.select]), 3)
# for the UI bounds, we should end up with the same order of magnitude,
# e.g. if point estimate is 123, we want to avoid reporting 0.456 for a bound
# because we do not have that level of precision. So the order of magnitude
# should follow from the median.
for (all in 1:dim(excDf.selectCq)[1]) {
  for (q in c(1, 3)) {
    excDf.selectCq[all, q] <- StandardizeSignifDigit(
      number.in = excDf.selectCq[all, q],
      number.ref = excDf.selectCq[all, 2])
  }#end of q loop
}#end of all loop

for (all in 1:dim(excDf.selectCq)[1]) {
  for (q in 1:Per) {
    excDf.selectCq[all, q] <- prettyNum(excDf.selectCq[all, q], width = 3, big.mark = ",")
  }#end of q loop
}#end of all loop

table.excDf.df[, paste("Excess female death", ageGroupName.a[a])] <-
  paste0(excDf.selectCq[, 2], " [", excDf.selectCq[, 1], "; ", excDf.selectCq[, 3], "]") 

table.excDf.df[ExternalMakeCountryNamesFull(name.c)[include.c],
               paste("% of death", ageGroupName.a[a])] <-
  StandardizeDecimal(res[[paste0("excD", age, "f.cqt")]][include.c, 2, t.select] /
                       (res[[paste0("D", age, "f.cqt")]][include.c, 2, t.select] +
                          res[[paste0("D", age, "m.cqt")]][include.c, 2, t.select]) * 100,
                     preciDchange)

write.csv(table.excDf.df, 
          paste0(output.dir, "paper_table3_", ageGroupName.a[a], ".csv"), 
          row.names = TRUE)

## helpful code
# res2 <- (format(res2, scientific=NA, digits = 3))#,big.mark = ",")#format(res2, digits = 3)#, scientific = TRUE)
# res2
# res2 <- signif(res2,3)
# res2*10
# ?format
# res2 <- prettyNum(res2, width = 3,big.mark = ",")#format(res2, digits = 3)#, scientific = TRUE)
# res2
# as.character(paste(res2[2]," [", res2[1], "; ", res2[2], "]", sep = ""), length = 4)
# res3<- as.character(paste(res2[2,]," [", res2[1,], "; ", res2[3,], "]", sep = ""), length = 4)
# format(1, nsmall = 3)

## the end ##


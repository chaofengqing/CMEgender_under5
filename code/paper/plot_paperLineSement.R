

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_paperLineSement.R
# 
# This script is to get Line segment style plots for main paper and Appendix.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script.
# PlotCIsegments(7)
# InternalMakeCountryNamesShort(4)
# 
# input data: data/output/M49/:
# 1. cis_M49_full.rda
# 2. indicesforexcess.1.all.rda
# 3. indicesforexcess.4.all.rda
# 4. indicesforexcess.5.all.rda
#
# output plots in fig/paper/:
# 1. paper_S1_M49_1990or2012sign_WorldRegion.pdf                - main Figure 1;
# 2. paper_S4_M49_1990or2012sign_WorldRegion.pdf                - main Figure 1;
# 3. paper_S5_M49_1990or2012sign_WorldRegion.pdf                - main Figure 1;
# 4. paper_AllAgeGroups_M49_1990or2012sign_1990order_female.pdf - main Figure 3;
# 6. paper_AllAgeGroups_M49_1990or2012_WorldRegion.pdf        - Appendix Figure 5;
# 5. paper_AllAgeGroups_M49_1990or2012sign_1990order_male.pdf - Appendix Figure 6;
#
###############################################################################

## import country indices with outlying sex ratio in 1990 and 2012 ##
load(file = paste0(output.dir, "indicesforexcess.1.all.rda")) #indicesforexcess.1.all
load(file = paste0(output.dir, "indicesforexcess.4.all.rda")) #indicesforexcess.4.all
load(file = paste0(output.dir, "indicesforexcess.5.all.rda")) #indicesforexcess.5.all

###################
## main Figure 1 ##
years.select <- c(1990, 2012) + 0.5
t.select <- which(is.element(years.t, c(1990.5, 2012.5)))

for (age in ages.a) {
  a <- which(ages.a == age)
  select.t <- is.element(years.t, years.select)
  
  ######################
  ## world & region S ##
  pdf(paste0(paperplot.dir, "paper_S", age, "_", runname,
             "_1990or2012sign_WorldRegion.pdf"), height = 5.5, width = 9)
  par(mar = c(3, 13, 1, 0.5), cex.lab = 1.1, mgp = c(1.6, 0.3, 0),
      cex.main = 1.5, cex.axis = 1, tcl = -0.3)  
  S.df <- array(NA, c(1 + R, Per, Tend))
  dimnames(S.df)[[1]] <- c("world", rep("region", R))
  
  S.df["world", , ] <- res[[paste0("S", age, ".qt" )]]
  S.df[dimnames(S.df)[[1]] == "region", , ] <-
    res[[paste0("S", age, ".rqt")]]
  
  ## above cutoff ##
  PlotCIsegments(
    if.oneSource = FALSE, select.t = select.t, if.noOrder = TRUE,
    countryName.c = c("World", regions.r), data.cqt = S.df, cutoff = cutoffS,
    xlab = paste("Sex ratio", ageGroupName.a[ages.a == age]), 
    colinfo = c("lightgrey", "hotpink", "hotpink4"), 
    lwd.main = 4, lwd.cutoff = 2, cex = 1.5
  )  
  legend("topright", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 5, cex = 1, bg = "white")
  dev.off()
    
}#end of age loop


###################
## main Figure 3 ##
years.select <- c(1990, 2012) + 0.5
t.select <- which(is.element(years.t, c(1990.5, 2012.5)))

pdf(paste0(paperplot.dir, "paper_AllAgeGroups_", runname, 
           "_1990or2012sign_1990order_female.pdf"),
    height = 29 * cm, width = 21 * cm)
par(mfrow = c(3, 2), mar = c(3, 7.7, 0.5, 0.8), cex.lab = 1.3, mgp = c(1.3, 0.3, 0), 
    cex.main = 1, cex.axis = 1.1, tcl = -0.3)
sum.1 <- sum(indicesforexcess.1.all$f1990.all[name.c] | indicesforexcess.1.all$f2012.all[name.c])
sum.4 <- sum(indicesforexcess.4.all$f1990.all[name.c] | indicesforexcess.4.all$f2012.all[name.c])
sum.5 <- sum(indicesforexcess.5.all$f1990.all[name.c] | indicesforexcess.5.all$f2012.all[name.c])

layout(matrix(c(rep(c(1, 2), sum.1),
                rep(c(3, 4), sum.4),
                rep(c(5, 6), sum.5)
),
              nr = sum(sum.1, sum.4, sum.5), nc = 2, byrow = TRUE), respect = FALSE)

for (age in ages.a) {
  
  a <- which(ages.a == age)
  select.t <- is.element(years.t, years.select)
  
  ## select countries with outlying sex ratio ##
  eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", age, ".all")))
  select.c <- (indicesforexcess[[paste0("f", floor(years.t[t.select[1]]), ".all")]] |
    indicesforexcess[[paste0("f", floor(years.t[t.select[2]]), ".all")]])[name.c]
  
  excQf.cqt <- res[[paste0("excQ", age, "f.cqt")]] * Qunit
  
  
  #####################
  ## excess female Q ##
  PlotCIsegments(
    countryName.c = InternalMakeCountryNamesShort(name.c),
    cutoff = cutoffQ, order = 2, data.cqt = excQf.cqt,
    select.c = select.c, select.t = select.t, yearOrder = 1990.5,
    xlab = paste("Excess female", ageGroupName.a[a], "(per 1,000)"), 
    colinfo = c("lightgrey", "hotpink", "hotpink4"), 
    lwd.main = 2.5, lwd.cutoff = 1.5, cex = 2
  )
  legend("bottomright", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
  ########
  ## Rx ##
  Rx.cqt <- res[[paste0("Rx", age, ".cqt")]]
  PlotCIsegments(
    data.cqt = Rx.cqt, if.AtoZ = FALSE,
    select.c = select.c, select.t = select.t, yearOrder = 1990.5,
    countryName.c = InternalMakeCountryNamesShort(name.c),
    cutoff = 1 / cutoffP,
    lwd.main = 2.5, lwd.cutoff = 1.5, cex = 2,
    xlab = paste("Estimated/Expected female", ageGroupName.a[a]), 
    colinfo = c("lightgrey", "hotpink", "hotpink4")
  )  
  legend("bottomright", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
}#end of age loop

dev.off()


#######################
## Appendix Figure 5 ##
## region segment plot ##
pdf(paste0(paperplot.dir, "paper_AllAgeGroups_", runname, 
           "_1990or2012_WorldRegion.pdf"),
    height = 29 * cm, width = 21 * cm)
par(mfrow = c(3, 2), mar = c(3, 0.5, 0.5, 0.1), oma = c(0, 14, 0.5, 0.5),
    cex.lab = 1.3, mgp = c(1.3, 0.3, 0), 
    cex.main = 1, cex.axis = 1.1, tcl = -0.3)

for (age in ages.a) {
  
  a <- which(ages.a == age)
  select.t <- is.element(years.t, years.select)
  
  excQf.ALLqt <- array(NA, c(1 + R, Per, Tend))
  dimnames(excQf.ALLqt)[[1]] <- c("world", rep("region", R))
  excQf.ALLqt[dimnames(excQf.ALLqt)[[1]] == "world", , ] <-
    res[[paste0("excQ", age, "f.qt")]] * Qunit
  excQf.ALLqt[dimnames(excQf.ALLqt)[[1]] == "region", , ] <-
    res[[paste0("excQ", age, "f.rqt")]] * Qunit
  
  
  #####################
  ## excess female Q ##
  PlotCIsegments(
    countryName.c = c("World", regions.r), if.noOrder = TRUE, if.oneSource = FALSE,
    cutoff = cutoffQ, order = 2, data.cqt = excQf.ALLqt,select.t = select.t,
    xlab = paste("Excess female", ageGroupName.a[a], "(per 1,000)"), 
    colinfo = c("lightgrey", "hotpink", "hotpink4"), 
    lwd.main = 2.5, lwd.cutoff = 1, cex = 2
  )
  legend("bottomright", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
  ########
  ## Rx ##
  Rx.ALLqt <- array(NA, c(1 + R, Per, Tend))
  dimnames(Rx.ALLqt)[[1]] <- c("world", rep("region", R))
  Rx.ALLqt[dimnames(Rx.ALLqt)[[1]] == "world", , ] <-
    res[[paste0("Rx", age, ".qt")]]
  Rx.ALLqt[dimnames(Rx.ALLqt)[[1]] == "region", , ] <-
    res[[paste0("Rx", age, ".rqt")]]
  
  PlotCIsegments(
    data.cqt = Rx.ALLqt, if.noOrder = TRUE, Yaxis = FALSE, if.oneSource = FALSE,
    select.t = select.t, countryName.c = c("World", regions.r),
    cutoff = 1 / cutoffP, lwd.main = 2.5, lwd.cutoff = 1, cex = 2,
    xlab = paste("Estimated/Expected female", ageGroupName.a[a]), 
    colinfo = c("lightgrey", "hotpink", "hotpink4")
  )
  legend("bottomright", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
}#end of age loop
dev.off()


#######################
## Appendix Figure 6 ##
pdf(paste0(paperplot.dir, "paper_AllAgeGroups_", runname, 
           "_1990or2012sign_1990order_male.pdf"),
    height = 29 * cm, width = 21 * cm)
par(mfrow = c(3, 2), mar = c(3, 8.5, 0.5, 0.8), cex.lab = 1.3, mgp = c(1.3, 0.3, 0), 
    cex.main = 1, cex.axis = 1.1, tcl = -0.3)
sum.1 <- sum(indicesforexcess.1.all$m1990.all[name.c] | indicesforexcess.1.all$m2012.all[name.c])
sum.4 <- sum(indicesforexcess.4.all$m1990.all[name.c] | indicesforexcess.4.all$m2012.all[name.c])
sum.5 <- sum(indicesforexcess.5.all$m1990.all[name.c] | indicesforexcess.5.all$m2012.all[name.c])

layout(matrix(c(rep(c(1, 2), sum.1),
                rep(c(3, 4), sum.4),
                rep(c(5, 6), sum.5)
),
              nr = sum(sum.1, sum.4, sum.5), nc = 2, byrow = TRUE), respect = FALSE)

for (age in ages.a) {
  
  a <- which(ages.a == age)
  select.t <- is.element(years.t, years.select)
  
  ## select countries with outlying sex ratio ##
  eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", age, ".all")))
  select.c <- (indicesforexcess[[paste0("m", floor(years.t[t.select[1]]), ".all")]] |
    indicesforexcess[[paste0("m", floor(years.t[t.select[2]]), ".all")]])[name.c]
  
  excQf.cqt <- res[[paste0("excQ", age, "f.cqt")]] * Qunit
  
  
  #####################
  ## excess female Q ##
  PlotCIsegments(
    countryName.c = InternalMakeCountryNamesShort(name.c),
    cutoff = cutoffQ, order = 2, data.cqt = excQf.cqt,
    select.c = select.c, select.t = select.t, yearOrder = 1990.5,
    xlab = paste("Excess female", ageGroupName.a[a], "(per 1,000)"), 
    colinfo = c("lightgrey", "hotpink", "hotpink4"), 
    lwd.main = 2.5, lwd.cutoff = 1.5, cex = 2
  )
  legend("topleft", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
  ########
  ## Rx ##
  Rx.cqt <- res[[paste0("Rx", age, ".cqt")]]
  PlotCIsegments(
    data.cqt = Rx.cqt, if.AtoZ = FALSE,
    select.c = select.c, select.t = select.t, yearOrder = 1990.5,
    countryName.c = InternalMakeCountryNamesShort(name.c),
    cutoff = 1 / cutoffP,
    lwd.main = 2.5, lwd.cutoff = 1.5, cex = 2,
    xlab = paste("Estimated/Expected female", ageGroupName.a[a]), 
    colinfo = c("lightgrey", "hotpink", "hotpink4")
  )  
  legend("topleft", c("1990", "2012"), col = c("hotpink", "hotpink4"), 
         lty = 1, pch = 19, lwd = 3, cex = 1, bg = "white")
  
}#end of age loop

dev.off()

## the end ##


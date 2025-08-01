

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_CountryCIcompare_FullandExcl.R
# 
# This script is to compare country-specific S, W, P between Main.run and Excl.run.
#
# used for which run: Excl.run
#
# this script is called by any other scripts: main49_excl_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIbandwithDataseries(4)
# InternalMakeCountryNamesShort(2)
# 
# input data in folder data/output/M49_excl/:
# 1. countryCI/cis_M49_excl.rda
#
# output plots in folder fig/M49_excl/
# 1. CIsM49_exclCoutlier_compareSandP.pdf
# 2. CIsM49_exclCoutlier_compareWandP.pdf
# 
###############################################################################

iso.excl <- res.excl[["iso.c"]]
iso.full <- res.full[["iso.c"]]

##############################################
## full plot with data serise and estimates ##
pdf(paste0(fig.dir, "CIs", runname, "_compareSandP.pdf"), height = 12, width = 20)
for (c in 1:C) {
  par(mfrow = c(2, 3), cex.lab = 2, cex.axis = 2, mar = c(5, 5, 5, 1), oma = c(3, 3, 3, 3))

  selectCountry.i <- (iso.i == paste(iso.excl[c]))
  cfull <- which(iso.excl[c] == iso.full)
  
  for (age in ages.a) {
    Sexcl.qt <- res.excl[[paste0("S", age, ".cqt")]][c    , , ]
    Sfull.qt <- res.full[[paste0("S", age, ".cqt")]][cfull, , ]
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    # sort by survey date to get nicer legend
    selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    
    
    PlotCIbandwithDataseries(
     if.xlimFix = TRUE,
      datalim = c(0.95, 1.2), CI1s = Sfull.qt, CI2s = Sexcl.qt, baseSeries = "VR",
      Source = surveyplot.i, x = year.i, select.x = selectCountryAge.i,
      nameCI1 = "full", nameCI2 = "exclude extreme country",
      ylab = paste0("S", age), xlab = "Year", cutoff = cutoffS, colCI = c("red", "blue")
    )    
  }#end of age loop
  
  for (age in ages.a) {
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    
    Pexcl.qt <- res.excl[[paste0("P", age, ".cqt")]][c    , , ]
    Pfull.qt <- res.full[[paste0("P", age, ".cqt")]][cfull, , ]
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, CI1s = Pfull.qt, CI2s = Pexcl.qt, x = year.i, select.x = selectCountryAge.i,
      datalim = c(0.8, 1.2), nameCI1 = "full", nameCI2 = "exclude extreme country",
      ylab = paste0("P", age), xlab = "Year", cutoff = cutoffP, colCI = c("black", "darkolivegreen3")
    )      
  }#end of age loop
  
  title(main = InternalMakeCountryNamesShort(name.c)[c],
        line = -1, cex.main = 4.5, outer = TRUE)
}#end of c loop
dev.off()

##############################################
## full plot with data serise and estimates ##
pdf(paste0(fig.dir, "CIs", runname, "_compareWandP.pdf"), height = 12, width = 20)
for (c in 1:length(iso.excl)) {
  par(mfrow = c(2, 3), cex.lab = 2, cex.axis = 2, mar = c(5, 5, 5, 1), oma = c(3, 3, 3, 3))

  selectCountry.i <- (iso.i == paste(iso.excl[c]))
  cfull <- which(iso.excl[c] == iso.full)
  for (age in ages.a) {
    Wexcl.qt <- res.excl[[paste0("W", age, ".cqt")]][c    , , ]
    Wfull.qt <- res.full[[paste0("W", age, ".cqt")]][cfull, , ]
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    # sort by survey date to get nicer legend
    selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, baseSeries = "VR", datalim = c(0.9, 1.2),
      CI1s = Wfull.qt, CI2s = Wexcl.qt, colCI = c("red", "blue"),
      Source = surveyplot.i, x = year.i, select.x = selectCountryAge.i,
      nameCI1 = "full", nameCI2 = "exclude extreme country",
      ylab = paste0("W", age), xlab = "Year", cutoff = cutoffS
    )    
  }#end of age loop
  
  for (age in ages.a) {
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    
    Pexcl.qt <- res.excl[[paste0("P", age, ".cqt")]][c    , , ]
    Pfull.qt <- res.full[[paste0("P", age, ".cqt")]][cfull, , ]
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, CI1s = Pfull.qt, CI2s = Pexcl.qt,
      x = year.i, select.x = selectCountryAge.i, datalim = c(0.8, 1.2),
      nameCI1 = "full", nameCI2 = "exclude extreme country",
      ylab = paste0("P", age), xlab = "Year", cutoff = cutoffP,
      colCI = c("black", "darkolivegreen3")
    )      
  }#end of age loop
  
  title(main = InternalMakeCountryNamesShort(name.c)[c],
        line = -1, cex.main = 4.5, outer = TRUE)
}#end of c loop
dev.off()

## the end ##


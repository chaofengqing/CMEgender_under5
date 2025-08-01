

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_DataseriesandCountryCI_country.R
# 
# This script plots summarize plots for country estimates.
#
# used for which run: Main.run; Validation.run
#
# this script is called by any other scripts:
# 1. main48_output.R;
# 2. main49_vali_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIbandwithDataseries(4)
# InternalMakeCountryNamesShort(3) - to standardize country names displayed.
# 
# input data from folder data/output/
# 1. M49/countryCI/cis_runname_2012.rda
# 2. M49_vali/Results_ErrorRelativeError_TestingSet.csv (Validation.run only).
#
# output plots in folder fig/:
# 1. M49/countryplot/*.pdf: country-specific estimated sex ratio (with data
# series and non-imputed SE), expected sex ratio, and P multiplier.
# 2. M49_vali/countryplot/*.pdf: country-specific estimated sex ratio (with
# data series from training set in color, left-out data series in gray,
# non-imputed SE, and predictions of left-out data in seashell color), and
# expected sex ratio.
# 
###############################################################################

if (Validation.run) {
  ErrorCoverage.df <- read.csv(
    file = paste0(output.dir, "Results_ErrorRelativeError_TestingSet.csv"),
    stringsAsFactors = FALSE) #construct_ErrorRelativeErrorCoverage_TestingSet.R
  s.l <- ErrorCoverage.df[, "s.90PI.Lower"]
  s.u <- ErrorCoverage.df[, "s.90PI.Upper"]
  s.m <- ErrorCoverage.df[, "s.Median"    ]
}


##############################################
## full plot with data serise and estimates ##
for (c in 1:C) {
  if (Validation.run) {
    pdf(paste0(countryplot.dir, InternalMakeCountryNamesShort(name.c)[c], ".pdf"),
        height = 20, width = 12)
    par(mfrow = c(3, 2), cex.lab = 2.5, cex.axis = 2,
        mar = c(4.5, 6, 3, 1), oma = c(0.2, 2.5, 3, 3), mgp = c(3.5, 1, 0))
  } else {
    pdf(paste0(countryplot.dir, InternalMakeCountryNamesShort(name.c)[c], ".pdf"),
        height = 12, width = 20)
    par(mfrow = c(2, 3), cex.lab = 2.5, cex.axis = 2, 
        mar = c(4.5, 6, 3, 1), oma = c(0.2, 2.5, 3, 3), mgp = c(3.5, 1, 0))
  }
  
  selectCountry.i <- (iso.i == iso.c[c])
  unique.sources <- 
    unique(surveyplot.i[
      which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
      ])
  
  for (age in ages.a) { # remove q4 here and in lists
    S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
    W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    # sort by survey date to get nicer legend
    select.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, if.SurveyLegend = TRUE,
      dataseries = s.i, dataseriesSE = SEnoimpute.i,
      CI1s = S.qt, CI2s = W.qt, baseSeries = "VR",
      Source = surveyplot.i,  x = year.i, select.x = select.i,
      nameCI1 = "Country estimate (S)", nameCI2 = "Expected (W)",
      ylab = paste0("S", age), xlab = "Year", cutoff = cutoffS,
      unique.sources = unique.sources, lwd.CI1 = 5, lwd.CI2 = 3
    )
    
    if (Validation.run) {
      selectCountryAge.e <- seq(1, E)[(iso.e == iso.c[c]) & (agecat.e == age)]
      
      ## add in predicted left-out and CI ##
      for (sur in as.character(unique(surveyplot.e[selectCountryAge.e]))) {
        select.sur <- selectCountryAge.e[surveyplot.e[selectCountryAge.e] == sur]
        Left.qt <- matrix(NA, nr = Per, nc = length(select.sur))
        Left.qt[1, ] <- s.l[select.sur]
        Left.qt[3, ] <- s.u[select.sur]
        PlotCIbandwithDataseries(
          if.NewPlot = FALSE, 
          dataseries = s.m[select.sur], dataseriesSE = SEnoimpute.e[select.sur],
          Source = surveyplot.e[select.sur], baseSeries = sur,
          colbase = "seashell2", CI1s = Left.qt, x = year.e[select.sur],
          seq.years = year.e[select.sur], colCI = "seashell2"
        )  
      }#end of sur loop 
      
      ## add in left-out observations ##      
      PlotCIbandwithDataseries(
        if.xlimFix = TRUE, if.SurveyLegend = TRUE, 
        if.NewPlot = FALSE, if.sepLegendPage = TRUE,
        dataseries = s.e, dataseriesSE = SEnoimpute.e,
        Source = surveyplot.e, baseSeries = as.character(unique(surveyplot.e)),
        colbase = "slategray", x = year.e, select.x = selectCountryAge.e
      )      
    }#end of if(Validation.run)
  }#end of age loop
  
  if (!Validation.run) {
    for (age in ages.a) { # remove Q4 here and in list
      selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
      
      P.qt <- res[[paste0("P", age, ".cqt")]][c, , ]
      PlotCIbandwithDataseries(
        if.xlimFix = TRUE, CI1s = P.qt, x = year.i, select.x = selectCountryAge.i,
        cutoff = cutoffS, ylab = paste0("P", age), xlab = "Year", colCI = "black"
      )
    }#end of age loop    
  }#end of if(!Validation.run)
  
  title(main = InternalMakeCountryNamesShort(name.c)[c],
        line = -1, cex.main = 4.5, outer = TRUE)
  dev.off()
  
}#end of c loop

## the end ##


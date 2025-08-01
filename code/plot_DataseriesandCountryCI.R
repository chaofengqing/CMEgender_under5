

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_DataseriesandCountryCI.R
# 
# This script plots summarize plots for world/region/country estimates.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main48_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIbandwithDataseries(11)
# InternalMakeCountryNamesShort(2) - to standardize country names displayed.
# 
# input data: data/output/M49/countryCI/cis_M49_full.rda
#
# output plots in folder fig/M49/:
# 1. CIsM49(NOimputeSE)_allResults_WorldRegionCountry.pdf: world, region, and
# country-specific estimated sex ratio (with data series and SE), expected sex
# ratio, ratio of estimated to expected female mortality (Rx), estimated male
# mortality, estimated female mortality, excess female mortality, and excess
# female death with their 90% credible bounds.
# 2. CIs_QfmM49.pdf: country-specific male/female/(male-female) mortality with
# observed data series.
# note: in the plot name, "NOimputeSE" means the standard error for data series
# shown in the plot are observed instead of imputed SE which is used to fit the
# JAGS model.
# 
###############################################################################


##############################################
## full plot with data serise and estimates ##
pdf(paste0(fig.dir, "CIs", runname,
           "(NOimputeSE)_allResults_WorldRegionCountry.pdf"),
    height = 29 * cm, width = 21 * cm)

# to force axis showing numeric instead of scientific values for
# number smaller than 10^6.
options(scipen = 1000000)

par(mfrow = c(4, 3), cex.lab = 1.3, cex.axis = 1, tcl = -0.3,
    mar = c(3, 2.7, 0.5, 0.4), oma = c(0, 1, 2.2, 1), mgp = c(1.3, 0.3, 0))

###################
## world results ##
## S and W ##
datalim.SW <- range(res$S1.qt, res$S4.qt, res$S5.qt,
                    res$Wx1.qt, res$Wx4.qt, res$Wx5.qt,
                    na.rm = TRUE) + c(-0.1, 0)
for (age in ages.a) {
  a <- which(ages.a == age)
  
  S.qt <- res[[paste0("S", age, ".qt")]][, is.element(years.t, yearSex)]
  # do not plot CI for W
  Wx.qt <- res[[paste0("Wx", age, ".qt")]][, is.element(years.t, yearSex)]
  Wx.qt[1, ] <- Wx.qt[3, ] <- NA
  
  PlotCIbandwithDataseries(
    datalim = datalim.SW, CI1s = S.qt, CI2s = Wx.qt,
    ylab = paste("sex ratio", ageGroupName.a[a]), xlab = "Year",
    year.t = floor(yearSex), x = floor(yearSex), cutoff = cutoffS,
    lwd.CI1 = 3, lwd.CI2 = 3, cex.legend = 1, cex.dataseries = 0.7
  )
  # give legend for U5MR group is enough. No need to show up for each group
  if (a == A) {
    legend("bottomright", legend = c("Estimated (S)", "Expected (W)"),
           col = c("red", "darkgreen"), lwd = 5, lty = 1, cex = 1)
  }
  
}#end of age loop

## excess Qfemale, Qfemale, Qmale ##
datalim.Q <- range(res$Q1m.qt[, is.element(years.t, yearSex)],
                   res$Q4m.qt[, is.element(years.t, yearSex)],
                   res$Q5m.qt[, is.element(years.t, yearSex)],
                   res$Q1f.qt[, is.element(years.t, yearSex)],
                   res$Q4f.qt[, is.element(years.t, yearSex)],
                   res$Q5f.qt[, is.element(years.t, yearSex)],
                   res$excQ1f.qt[, is.element(years.t, yearSex)],
                   res$excQ4f.qt[, is.element(years.t, yearSex)],
                   res$excQ5f.qt[, is.element(years.t, yearSex)],
                   na.rm = TRUE) * Qunit
for (age in ages.a) {
  a <- which(ages.a == age)
  
  Qm.qt <- (
    res[[paste0("Q", age, "m.qt")]][, is.element(years.t, yearSex)]
  ) * Qunit
  Qf.qt <- (
    res[[paste0("Q", age, "f.qt")]][, is.element(years.t, yearSex)]
  ) * Qunit
  excQf.qt <- (
    res[[paste0("excQ", age, "f.qt")]][, is.element(years.t, yearSex)]
  ) * Qunit
  
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE, datalim = datalim.Q,
    CI1s = excQf.qt, CI2s = Qf.qt, CI3s = Qm.qt, cutoff = cutoffQ,
    year.t = floor(yearSex), x = floor(yearSex), legendCI.posi = "topright",
    ylab = paste(ageGroupName.a[a], "* 1000"), xlab = "Year",
    colCI = c("limegreen", "hotpink", "deepskyblue"),
    lwd.CI1 = 3, lwd.CI2 = 2, lwd.CI3 = 2, cex.legend = 1
  )
  # give legend for U5MR group is enough. No need to show up for each group
  if (a == A) {
    legend("topright",
           legend = c("Excess female", "Estimated female", "Estimated male"),
           col = c("limegreen", "hotpink", "deepskyblue"),
           lwd = 5, lty = 1, cex = 1)
  }
  
}#end of age loop

## Rx ##
datalim.Rx <- range(res$Rx1.qt, res$Rx4.qt, res$Rx5.qt,
                    na.rm = TRUE) + c(-0.1, 0.1)
for (age in ages.a) {
  a <- which(ages.a == age)
  Rx.qt <- res[[paste0("Rx", age, ".qt")]]
  
  PlotCIbandwithDataseries(
    datalim = datalim.Rx, CI1s = Rx.qt, x = floor(yearSex),
    ylab = paste("Estimated / Expected female", ageGroupName.a[a]),
    xlab = "Year", cutoff = cutoffP, colCI = "midnightblue", lwd.CI1 = 3
  )      
}#end of age loop

## excess female deaths ##
for (age in ages.a) {
  a <- which(ages.a == age)
  excDf.qt <- res[[paste0("excD", age, "f.qt")]]
  
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE, CI1s = excDf.qt, x = floor(yearSex),
    ylab = paste("Excess female death", ageGroupName.a[a]), lwd.CI1 = 3,
    xlab = "Year", cutoff = cutoffD, colCI = "black"
  )
}#end of age loop
title(main = "World", line = 0.3, cex.main = 2.2, outer = TRUE)

####################
## region results ##
for (r in 1:R) {
  
  ## S and W ##
  datalim.SW <- range(
    res$S1.rqt[r, , ], res$S4.rqt[r, , ], res$S5.rqt[r, , ],
    res$Wx1.rqt[r, , ], res$Wx4.rqt[r, , ], res$Wx5.rqt[r, , ],
    na.rm = TRUE) + c(-0.1, 0)
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    S.qt <- res[[paste0("S", age, ".rqt")]][r, , is.element(years.t, yearSex)]
    # do not plot CI for W
    Wx.qt <- res[[paste0("Wx", age, ".rqt")]][r, , is.element(years.t, yearSex)]
    Wx.qt[1, ] <- Wx.qt[3, ] <- NA
    
    PlotCIbandwithDataseries(
      datalim = datalim.SW, CI1s = S.qt, CI2s = Wx.qt,
      ylab = paste("sex ratio", ageGroupName.a[a]), xlab = "Year",
      x = floor(yearSex), year.t = floor(yearSex), cutoff = cutoffS,
      lwd.CI1 = 3, lwd.CI2 = 3, cex.legend = 1, cex.dataseries = 0.7
    )
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("bottomright", legend = c("Estimated (S)", "Expected (W)"),
             col = c("red", "darkgreen"), lwd = 5, lty = 1, cex = 1)
    }
    
  }#end of age loop
  
  ## excess Qfemale, Qfemale, Qmale ##
  datalim.Q <- range(res$Q1m.rqt[r, , is.element(years.t, yearSex)],
                     res$Q4m.rqt[r, , is.element(years.t, yearSex)],
                     res$Q5m.rqt[r, , is.element(years.t, yearSex)],
                     res$Q1f.rqt[r, , is.element(years.t, yearSex)],
                     res$Q4f.rqt[r, , is.element(years.t, yearSex)],
                     res$Q5f.rqt[r, , is.element(years.t, yearSex)],
                     res$excQ1f.rqt[r, , is.element(years.t, yearSex)],
                     res$excQ4f.rqt[r, , is.element(years.t, yearSex)],
                     res$excQ5f.rqt[r, , is.element(years.t, yearSex)],
                     na.rm = TRUE) * Qunit
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    Qm.qt <- (
      res[[paste0("Q", age, "m.rqt")]][r, , is.element(years.t, yearSex)]
    ) * Qunit
    Qf.qt <- (
      res[[paste0("Q", age, "f.rqt")]][r, , is.element(years.t, yearSex)]
    ) * Qunit
    excQf.qt <- (
      res[[paste0("excQ", age, "f.rqt")]][r, , is.element(years.t, yearSex)]
    ) * Qunit
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, datalim = datalim.Q,
      CI1s = excQf.qt, CI2s = Qf.qt, CI3s = Qm.qt, cutoff = cutoffQ,
      year.t = floor(yearSex), x = floor(yearSex), legendCI.posi = "topright",
      ylab = paste(ageGroupName.a[a], "* 1000"), xlab = "Year",
      colCI = c("limegreen", "hotpink", "deepskyblue"),
      lwd.CI1 = 3, lwd.CI2 = 2, lwd.CI3 = 2, cex.legend = 1
    )
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("topright",
             legend = c("Excess female", "Estimated female", "Estimated male"),
             col = c("limegreen", "hotpink", "deepskyblue"),
             lwd = 5, lty = 1, cex = 1)
    }
    
  }#end of age loop
  
  ## Rx ##
  datalim.Rx <- range(
    res$Rx1.rqt[r, , ], res$Rx4.rqt[r, , ], res$Rx5.rqt[r, , ],
    na.rm = TRUE) + c(-0.1, 0.1)
  
  for (age in ages.a) {
    a <- which(ages.a == age)
    Rx.qt <- res[[paste0("Rx", age, ".rqt")]][r, , ]
    
    PlotCIbandwithDataseries(
      datalim = datalim.Rx, CI1s = Rx.qt, x = floor(yearSex),
      ylab = paste("Estimated / Expected female", ageGroupName.a[a]),
      xlab = "Year", cutoff = cutoffP, colCI = "midnightblue", lwd.CI1 = 3
    )
  }#end of age loop
  
  ## excess female deaths ##
  for (age in ages.a) {
    a <- which(ages.a == age)
    excDf.qt <- res[[paste0("excD", age, "f.rqt")]][r, , ]
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, CI1s = excDf.qt, x = floor(yearSex),
      ylab = paste("Excess female death", ageGroupName.a[a]), lwd.CI1 = 3,
      xlab = "Year", cutoff = cutoffD, colCI = "black"
    )  
  }#end of age loop
  
  title(main = regions.r[r], line = 0.3, cex.main = 2.2, outer = TRUE)
}#end of r loop

#####################
## country results ##
for (c in 1:C) {
  
  selectCountry.i <- (iso.i == iso.c[c])
  unique.sources <- 
    unique(surveyplot.i[
      which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
      ])
  
  ## S and W ##
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
    # do not plot CI for W
    W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]
    W.qt[1, ] <- W.qt[3, ] <- NA
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    # sort by survey date to get nicer legend
    selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    
    
    PlotCIbandwithDataseries(
      ylab = paste("sex ratio", ageGroupName.a[a]), xlab = "Year",
      dataseries = s.i, dataseriesSE = SEnoimpute.i,
      Source = surveyplot.i, x = year.i, select.x = selectCountryAge.i,
      baseSeries = c("VR", "VR "),
      #nameCI1 = "Country estimate (S)", nameCI2 = "Expected (W)",
      lwd.CI1 = 3, lwd.CI2 = 2, cex.legend = 1, cex.dataseries = 0.7,
      cutoff = cutoffS, CI1s = S.qt, CI2s = W.qt, unique.sources = unique.sources
    )
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("bottomright", legend = c("Estimated (S)", "Expected (W)"),
             col = c("red", "darkgreen"), lwd = 5, lty = 1, cex = 1)
    }
    
  }#end of age loop
  
  ## excess Qfemale, Qfemale, Qmale ##
  datalim.Q <- range(res$Q1m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q4m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q5m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q1f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q4f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q5f.cqt[c, , is.element(years.t, yearSex)],
                     res$excQ1f.cqt[c, , is.element(years.t, yearSex)],
                     res$excQ4f.cqt[c, , is.element(years.t, yearSex)],
                     res$excQ5f.cqt[c, , is.element(years.t, yearSex)],
                     na.rm = TRUE) * Qunit
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    Qm.qt <- (
      res[[paste0("Q", age, "m.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    Qf.qt <- (
      res[[paste0("Q", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    excQf.qt <- (
      res[[paste0("excQ", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, datalim = datalim.Q,
      CI1s = excQf.qt, CI2s = Qf.qt, CI3s = Qm.qt, cutoff = cutoffQ,
      year.t = floor(yearSex), x = floor(yearSex), legendCI.posi = "topright",
      ylab = paste(ageGroupName.a[a], "* 1000"), xlab = "Year",
      colCI = c("limegreen", "hotpink", "deepskyblue"),
      #nameCI1 = "excess female", nameCI2 = "female", nameCI3 = "male",
      lwd.CI1 = 3, lwd.CI2 = 2, lwd.CI3 = 2, cex.legend = 1
    )
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("topright",
             legend = c("Excess female", "Estimated female", "Estimated male"),
             col = c("limegreen", "hotpink", "deepskyblue"),
             lwd = 5, lty = 1, cex = 1)
    }
    
  }#end of age loop
  
  ## Rx ##
  datalim.Rx <- range(
    res$Rx1.cqt[c, , ], res$Rx4.cqt[c, , ], res$Rx5.cqt[c, , ],
    na.rm = TRUE) + c(-0.1, 0.1)
  
  for (age in ages.a) {
    a <- which(ages.a == age)
    Rx.qt <- res[[paste0("Rx", age, ".cqt")]][c, , ]
    
    PlotCIbandwithDataseries(
      datalim = datalim.Rx, CI1s = Rx.qt, x = floor(yearSex),
      ylab = paste("Estimated / Expected female", ageGroupName.a[a]),
      xlab = "Year", cutoff = cutoffP, colCI = "midnightblue", lwd.CI1 = 3
    )      
  }#end of age loop
  
  ## excess female deaths ##
  for (age in ages.a) {
    a <- which(ages.a == age)
    excDf.qt <- res[[paste0("excD", age, "f.cqt")]][c, , ]
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, CI1s = excDf.qt, x = floor(yearSex),
      ylab = paste("Excess female death", ageGroupName.a[a]), lwd.CI1 = 3,
      xlab = "Year", cutoff = cutoffD, colCI = "black"
    )  
  }#end of age loop
  
  title(main = ExternalMakeCountryNamesFull(name.c)[c],
        line = 0.3, cex.main = 2.2, outer = TRUE)
}#end of c loop

dev.off()



##############################
## plot for Qf.ct and Qm.ct ##
if (getQbysex) {
  pdf(paste0(fig.dir, "CIs_Qfm", runname, ".pdf"), height = 12, width = 25)
  
  for (c in 1:C) {
    par(mfrow = c(3, 4), cex.lab = 2.5, cex.axis = 2, 
        mar = c(4.5, 5.5, 2, 1), oma = c(0.2, 1.7, 3, 1), mgp = c(3, 1, 0))
    
    selectCountry.i <- (iso.i == paste(iso.c[c]))
    unique.sources <- 
      unique(surveyplot.i[
        which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
        ])
    
    for (age in ages.a) {
      a <- which(ages.a == age)
      
      Qf.i <- eval(parse(text = paste0("Q", age, "f.i"))) * Qunit
      Qm.i <- eval(parse(text = paste0("Q", age, "m.i"))) * Qunit
      Qd.i <- Qm.i - Qf.i
      
      Qf.qt <- (res[[paste0("Q", age, "f.cqt")]][c, , ]) * Qunit
      Qm.qt <- (res[[paste0("Q", age, "m.cqt")]][c, , ]) * Qunit
      Qd.qt <- (res[[paste0("Q", age, "d.cqt")]][c, , ]) * Qunit
      
      selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]      
      # sort by survey date to get nicer legend
      selectCountryAge.i <-
        selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]
      
      PlotCIbandwithDataseries(
        if.xlimFix = TRUE, CI1s = Qf.qt, x = year.i,
        dataseries = Qf.i, baseSeries = "VR",
        datalim = c(cutoffQ,
                    max(ifelse(length(selectCountryAge.i) == 0,
                               c(Qf.qt), Qm.i[selectCountryAge.i]),
                        c(Qf.qt), na.rm = TRUE)),
        cutoff = cutoffQ, Source = surveyplot.i, select.x = selectCountryAge.i,
        ylab = paste0("female ", ageGroupName.a[a], "*1000"),
        xlab = "Year", colCI = "hotpink", unique.sources = unique.sources
      )
      
      PlotCIbandwithDataseries(
        if.xlimFix = TRUE, select.x = selectCountryAge.i,
        dataseries = Qm.i, baseSeries = "VR",
        datalim = c(cutoffQ, max(ifelse(length(selectCountryAge.i) == 0, 
                                        c(Qm.qt), Qm.i[selectCountryAge.i]),
                                 c(Qm.qt), na.rm = TRUE)),
        CI1s = Qm.qt, cutoff = cutoffQ, Source = surveyplot.i, x = year.i, 
        ylab = paste0("male ", ageGroupName.a[a], "*1000"), 
        xlab = "Year", colCI = "royalblue", unique.sources = unique.sources
      )
      
      PlotCIbandwithDataseries(
        if.SurveyLegend = TRUE, if.xlimFix = TRUE, if.sepLegendPage = TRUE,
        x = year.i, unique.sources = unique.sources,
        dataseries = Qd.i, baseSeries = "VR",
        datalim = c(
          min(ifelse(length(selectCountryAge.i) == 0,
                     c(Qd.qt), Qd.i[selectCountryAge.i]),
              c(Qd.qt), na.rm = TRUE),
          max(ifelse(length(selectCountryAge.i) == 0,
                     c(Qd.qt), Qd.i[selectCountryAge.i]),
              c(Qd.qt), na.rm = TRUE)),
        CI1s = Qd.qt, cutoff = cutoffQ, Source = surveyplot.i, xlab = "Year",
        ylab = paste0("(male-female) ", ageGroupName.a[a], "*1000"),
        select.x = selectCountryAge.i, colCI = "black", cex.legend = 2
      )
      
    }#end of age loop
    
    title(main = InternalMakeCountryNamesShort(name.c)[c],
          line = -1, cex.main = 4, outer = TRUE)
    
  }#end of c loop
  dev.off()
  
}#end of if getQbysex

## the end ##



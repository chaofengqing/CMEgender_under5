

## plots for 2014 May UNICEF TAG meeting ##
tag.plot.dir <- paste0(fig.dir, "extra_plots/plots for 2014 May UNICEF TAG/")


for (c in 1:C) {
  pdf(paste0(tag.plot.dir, "Qfemale/EstandExpQfemale_",
             ExternalMakeCountryNamesFull(name.c)[c], ".pdf"),
      height = 7.5 * cm, width = 21 * cm)
  par(mfrow = c(1, 3), cex.lab = 1.3, cex.axis = 1, tcl = -0.3,
      mar = c(3, 2.7, 0.5, 0.4), oma = c(0, 1, 2.2, 1), mgp = c(1.3, 0.3, 0))
  
  selectCountry.i <- (iso.i == iso.c[c])
  unique.sources <- 
    unique(surveyplot.i[
      which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
      ])
    
  ## excess Qfemale, Qfemale, Qmale ##
  datalim.Q <- range(res$Q1m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q4m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q5m.cqt[c, , is.element(years.t, yearSex)],
                     res$Q1f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q4f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q5f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ1f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ4f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ5f.cqt[c, , is.element(years.t, yearSex)],
                     na.rm = TRUE) * Qunit
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    Qm.qt <- (
      res[[paste0("Q", age, "m.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    Qf.qt <- (
      res[[paste0("Q", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    expQf.qt <- (
      res[[paste0("expQ", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, datalim = datalim.Q,
      CI1s = expQf.qt, CI2s = Qf.qt, CI3s = Qm.qt, cutoff = cutoffQ,
      year.t = floor(yearSex), x = floor(yearSex), legendCI.posi = "topright",
      ylab = paste(ageGroupName.a[a], "* 1000"), xlab = "Year",
      colCI = c("limegreen", "hotpink", "deepskyblue"),
      #nameCI1 = "excess female", nameCI2 = "female", nameCI3 = "male",
      lwd.CI1 = 3, lwd.CI2 = 2, lwd.CI3 = 2, cex.legend = 1)
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("topright",
             legend = c("Expected female", "Estimated female", "Estimated male"),
             col = c("limegreen", "hotpink", "deepskyblue"),
             lwd = 5, lty = 1, cex = 1)
    }#end of if(a == A) 
  }#end of age loop
  title(main = ExternalMakeCountryNamesFull(name.c)[c],
        line = 0.3, cex.main = 2.2, outer = TRUE)
  
  dev.off()
}#end of c loop



for (c in 1:C) {
  pdf(paste0(tag.plot.dir, "QfemaleANDmale/EstandExpQfemaleANDmale_",
             ExternalMakeCountryNamesFull(name.c)[c], ".pdf"),
      height = 7.5 * cm, width = 21 * cm)
  par(mfrow = c(1, 3), cex.lab = 1.3, cex.axis = 1, tcl = -0.3,
      mar = c(3, 2.7, 0.5, 0.4), oma = c(0, 1, 2.2, 1), mgp = c(1.3, 0.3, 0))
  
  selectCountry.i <- (iso.i == iso.c[c])
  unique.sources <- 
    unique(surveyplot.i[
      which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
      ])
  
  ## excess Qfemale, Qfemale, Qmale ##
  datalim.Q <- range(res$Q1f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q4f.cqt[c, , is.element(years.t, yearSex)],
                     res$Q5f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ1f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ4f.cqt[c, , is.element(years.t, yearSex)],
                     res$expQ5f.cqt[c, , is.element(years.t, yearSex)],
                     na.rm = TRUE) * Qunit
  for (age in ages.a) {
    a <- which(ages.a == age)
    
    Qf.qt <- (
      res[[paste0("Q", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    expQf.qt <- (
      res[[paste0("expQ", age, "f.cqt")]][c, , is.element(years.t, yearSex)]
    ) * Qunit
    
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, datalim = datalim.Q,
      CI1s = expQf.qt, CI2s = Qf.qt, cutoff = cutoffQ,
      year.t = floor(yearSex), x = floor(yearSex), legendCI.posi = "topright",
      ylab = paste(ageGroupName.a[a], "* 1000"), xlab = "Year",
      colCI = c("limegreen", "hotpink", "deepskyblue"),
      lwd.CI1 = 3, lwd.CI2 = 2, lwd.CI3 = 2, cex.legend = 1)
    # give legend for U5MR group is enough. No need to show up for each group
    if (a == A) {
      legend("topright",
             legend = c("Expected female", "Estimated female"),
             col = c("limegreen", "hotpink", "deepskyblue"),
             lwd = 5, lty = 1, cex = 1)
    }#end of if(a == A) 
  }#end of age loop
  title(main = ExternalMakeCountryNamesFull(name.c)[c],
        line = 0.3, cex.main = 2.2, outer = TRUE)
  
  dev.off()
}#end of c loop


## GBD vs IGME for India ##
## read in GBD data ##
ihme.df <- read.csv(paste0(output.dir, "Results_SexRatioIGMEandGBD.csv"),
                    header = TRUE, stringsAsFactors = FALSE, 
                    strip.white = TRUE)
ihme.sr   <- ihme.df[, "SexRatio.IHME"]
ihme.yr   <- ihme.df[, "Year"         ]
ihme.iso  <- ihme.df[, "ISO.Code"     ]
ihme.name <- ihme.df[,"Country.Name"  ]

dataS5all <- as.data.frame(read.csv("data/interim/dataset_S5all.csv",
                                    header = TRUE, stringsAsFactors = FALSE, 
                                    strip.white = TRUE))
I            <- dim(dataS5all)[1]
s.i          <- dataS5all[, "Sex.Ratio"     ]
agecat.i     <- dataS5all[, "agecat.i"      ]
name.i       <- dataS5all[, "Country.Name"  ]
year.i       <- dataS5all[, "Reference.Date"]
iso.i        <- dataS5all[, "Country.Code"  ]
method.i     <- dataS5all[, "method.i"      ]
typename.i   <- dataS5all[, "typename.i"    ]
surveyplot.i <- dataS5all[, "surveyplot.i"  ]
surveyyear.i <- dataS5all[, "Series.Year"   ]
SEnoimpute.i <- dataS5all[, "Sex.Ratio.SE"  ]


pdf(paste0(tag.plot.dir, "GBDcompare_OppositeCountry2010_India.pdf"), 
    height = 6, width = 7)
par(cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.5, las = 1, tcl = -0.3,
    mar = c(3.2, 3.5, 2, 0.5), mgp = c(2.2, 0.3, 0))
#based on plot at the end
c <- which(name.c == "India")

S.qt <- res[["S5.cqt"]][c, , ]

selectCountry.i <- (iso.i == paste(iso.c[c]))     
selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == ages.a[A]]
# sort by survey date to get nicer legend
selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    

PlotCIbandwithDataseries(
  if.xlimFix = TRUE, main = ExternalMakeCountryNamesFull(name.c)[c],
  dataseries = s.i, baseSeries = c("VR", "VR "), datalim = c(0.6, 2), 
  CI1s = S.qt, Source = surveyplot.i, x = year.i,
  select.x = selectCountryAge.i, ylab = "Sex Ratio U5MR", xlab = "Year",
  cutoff = cutoffS, lwd.CI1 = 5, cex.dataseries = 0.9, lwd.dataseries = 2
)

select.ihme <- ihme.iso==iso.c[c] & !is.na(ihme.sr)
lines(floor(ihme.yr[select.ihme]), ihme.sr[select.ihme], 
      col = "royalblue", lwd = 5)
legend("bottomleft", c("UN IGME (2013)", "GBD (2012)"), lty = 1, lwd = 5, 
       col = c("red", "royalblue"), cex = 1.2)

dev.off()


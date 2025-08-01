
# for Berkeley Brown bag
########################
## W1/4 and log(Q1/4) ##
pdf(paste0(fig.dir, "extra_plots/PAA2014_W1and4", runname, "log.pdf"), 
    height = 9, width = 18)
par(mfrow = c(1, 2), cex.lab = 2.5, cex.axis = 2.2, mar = c(5, 6, 3, 1), 
    cex.main = 3, mgp = c(3.6, 1, 0), tcl = -0.4, las = 1)
## plot W1/4/5 ~ log(Q1/4/5) and CI for W1/4/5 ##
xlim      <- c(300 / Qunit, 5 / Qunit)
ylim      <- c(0.8, 1.5)

for (age in ages.a[-3]) {
  a <- which(ages.a == age)
  
  PlotCIandLoess (
    x = log(res[[paste0("Q", age, ".i")]][agecat.i == age]),
    y = s.i[agecat.i == age], labelx = labelu,
    labelxlocation = labellogu, main = ageGroupName.a[a],
    xlab = "Total Mortality Rate*1,000 (log-scale)",
    ylab = "Sex Ratio", xlim = log(xlim), ylim = ylim,
    lwd.estimate = 7, lwd.CI = 5, cex.point = 0.8,
    if.redoXaix = TRUE, if.CI = TRUE, cutoff = cutoffS,
    CI = res[[paste0("W", age, ".qk")]], CIx = res[[paste0("logQ", age, ".k")]]
  )  
}#end of age loop

dev.off()

## THE END ##


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

for (country in name.c) {
  
  c <- which(name.c == country)
  
  pdf(paste0(fig.dir, "extra_plots/country/PAA2014_",
             InternalMakeCountryNamesShort(name.c)[c], "_fullS5observation.pdf"),
      height = 12, width = 20)
  par(mfrow = c(2, 3), cex.lab = 3, cex.axis = 2.5, cex.main = 3, las = 1,
      oma = c(0.2, 1, 4.5, 1), mgp = c(4, 1, 0))
  
  selectCountry.i <- (iso.i == iso.c[c])
  unique.sources <- 
    unique(surveyplot.i[
      which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
      ])
  
  for (age in ages.a) { # remove q4 here and in lists
    a <- which(ages.a == age)
    
    S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
    W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    # sort by survey date to get nicer legend
    select.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]
    par(mar = c(5.2, 6.5, 3.6, 1))
    if (a == 1) {
    	legendS <- "Country estimate (S)"
    	legendW <- "Expected (W)"
    } else {
    	legendS <- legendW <- NULL
    }

    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, main = ageGroupName.a[a],#if.SurveyLegend = TRUE,
      dataseries = s.i, dataseriesSE = SEnoimpute.i, datalim = c(0.8, 1.2),
      CI1s = S.qt, CI2s = W.qt, baseSeries = c("VR", "VR "), cex.dataseries = 1.3,
      Source = surveyplot.i,  x = year.i, select.x = select.i,
      nameCI1 = legendS, nameCI2 = legendW, cex.legend = 2,
      ylab = "Sex Ratio", xlab = "Year", cutoff = cutoffS, lwd.dataseries = 2,
      unique.sources = unique.sources, lwd.CI1 = 5, lwd.CI2 = 3
    )
  }#end of age loop
  
  for (age in ages.a) { # remove Q4 here and in list
    par(mar = c(5.3, 6.5, 0.8, 1))
    
    selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
    
    P.qt <- res[[paste0("P", age, ".cqt")]][c, , ]
    PlotCIbandwithDataseries(
      if.xlimFix = TRUE, CI1s = P.qt, x = year.i, select.x = selectCountryAge.i,
      cutoff = cutoffS, ylab = paste0("Country Multiplier P", age),
      xlab = "Year", colCI = "black", datalim = c(0.8, 1.2)
    )
  }#end of age loop    
  
  title(main = ExternalMakeCountryNamesFull(name.c)[c],
        line = 1, cex.main = 4.5, outer = TRUE)
  dev.off()
  
}#end of country loop

##############################
## plot for India IMR group ##
country.list <- c("India", "Malaysia", "Mongolia")
for (country in country.list) {
  pdf(paste0(fig.dir, "extra_plots/PAA2014_", country, "_IMR.pdf"),
      height = 14, width = 8)
  par(mfrow = c(2, 1), cex.lab = 2.3, cex.axis = 2, cex.main = 2.6, las = 1,
      mgp = c(4.5, 1, 0)) #oma = c(0, 0, 3, 0), 
  
  c <- which(name.c == country)
  selectCountry.i <- (iso.i == iso.c[c])
  age <- 1
  a <- which(ages.a == age)
  
  S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
  W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]
  
  selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
  # sort by survey date to get nicer legend
  select.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]
  par(mar = c(3, 6.5, 3, 1))
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE, main = country,
    dataseries = s.i, dataseriesSE = SEnoimpute.i, datalim = c(0.8, 1.2),
    CI1s = S.qt, CI2s = W.qt, baseSeries = c("VR", "VR "),
    cex.dataseries = 1.2, lwd.CI1 = 6, lwd.CI2 = 4, lwd.dataseries = 2,
    Source = surveyplot.i,  x = year.i, select.x = select.i,
    nameCI1 = "Country estimate (S)", nameCI2 = "Expected (W)",
    ylab = "Sex Ratio IMR", xlab = "", cutoff = cutoffS
  )
  
  par(mar = c(5.5, 6.5, 0, 1))
  selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
  P.qt <- res[[paste0("P", age, ".cqt")]][c, , ]
  
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE, CI1s = P.qt, x = year.i, select.x = selectCountryAge.i,
    cutoff = cutoffS, ylab = paste0("Country Multiplier P", age),
    xlab = "Year", colCI = "black", datalim = c(0.7, 1), lwd.CI1 = 7)
  
  dev.off()
}#end of country loop


##################################
## plot for full s5 data series ##
country.plot <- c("United States of America", "India", "Jordan")
for (country in country.plot) {
  pdf(paste0(fig.dir,
             paste0("extra_plots/PAA2014_full_obsS5_", country, ".pdf")),
      height = 8, width = 8)
  par(mfrow = c(1, 1), cex.lab = 2.3, cex.axis = 2, cex.main = 2.6, las = 1,
      mgp = c(4.5, 1, 0), mar = c(3, 6.5, 3, 1)) #oma = c(0, 0, 3, 0), 
  
c <- which(name.c == country)
selectCountry.i <- (iso.i == iso.c[c])
age <- ages.a[A]
a <- which(ages.a == age)

S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]

selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
# sort by survey date to get nicer legend
select.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]

PlotCIbandwithDataseries(
  if.xlimFix = TRUE, main = country,#if.SurveyLegend = TRUE, 
  dataseries = s.i, dataseriesSE = SEnoimpute.i, datalim = c(0.9, 1.1),
  baseSeries = c("VR", "VR "), alpha.dataseries = 1,#CI1s = S.qt, CI2s = W.qt, 
  cex.dataseries = 1.2, lwd.CI1 = 6, lwd.CI2 = 4, lwd.dataseries = 2.5,
  Source = surveyplot.i,  x = year.i, select.x = select.i,
  ylab = "Sex Ratio U5MR", xlab = "", cutoff = cutoffS
)
dev.off()

}#end of country loop


## the end ##




##############################################
## full plot with data serise and estimates ##
for (c in 1:C) {
  pdf(paste0(ccplot.dir, iso.c[c], ".pdf"), height = 15, width = 18)
  par(mfrow = c(2, 2), cex.lab = 2.5, cex.axis = 2, 
      mar = c(4.5, 6, 3, 1), oma = c(0.2, 2.5, 3, 3), mgp = c(3.5, 1, 0))
  
  selectnotyetageTandF <- (iso.i == paste(iso.c[c]))   
  for (age in ages[-2]) { # remove q4 here and in lists
    S.qt <- res[[paste0("S", age, ".cqt")]][c, , ]
    W.qt <- res[[paste0("W", age, ".cqt")]][c, , ]
    
    select <- seq(1, I)[selectnotyetageTandF & agecat.i == age]
    # sort by survey date to get nicer legend
    select <- select[order(surveyyear.i[select])]    
    
    PlotCIbandwithDataseries(
      if.dataseries = TRUE, if.dataseriesSE = TRUE, if.CI1s = TRUE, 
      if.CI2s = TRUE, if.baseSeries = TRUE, if.SurveyLegend = TRUE,
      if.sepLegendPage = FALSE, if.xlimFix = TRUE, if.CILegend = TRUE,
      dataseries = s.i, dataseriesSE = SEnoimpute.i,
      CI1s = S.qt, CI2s = W.qt, baseSeries = "VR",
      Source = surveyplot.i, x = year.i, select.x = select,
      nameCI1 = "Country estimate (S)", nameCI2 = "Expected (W)",
      ylab = paste0("S", age), xlab = "Year", cutoff = 1
    )
    
  }#end of age loop
  
  for (age in ages[-2]){ # remove Q4 here and in list
    select <- seq(1, I)[selectnotyetageTandF & agecat.i == age]
    
    P.qt <- res[[paste0("P", age, ".cqt")]][c, , ]
    PlotCIbandwithDataseries(
      if.CI1s = TRUE, if.xlimFix = TRUE,
      CI1s = P.qt, x = year.i, select.x = select,
      ylab = paste0("P", age), xlab = "Year",
      cutoff = 1, colCI = c("black", NULL)
    )      
  }#end of P1/4/5 loop
  
  title(main = name.c[c], line = -1, cex.main = 4.5, outer = TRUE)
  dev.off()
  
}#end of country loop

## the end ##



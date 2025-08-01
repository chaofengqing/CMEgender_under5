
###############################################################################
## only plot countries with total disadvantage for girls or boys ##
## plot for P1/4/5 in selected years ##
years.select <- c(1990, 2000, 2010, 2012) + 0.5

for (year in years.select) {
  for (age in ages.a) {
    P.cqt <- res[[paste0("P" ,age, ".cqt")]]
    
    pdf(paste0(Pplot.dir, "P", age, "_", runname, "_", round(year), ".pdf"),
        height = 7, width = 10)
    par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)  
    ## below cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.plotall = FALSE, select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c), data.cqt = P.cqt,
      cutoff = cutoffP, order = 1, cex = 1.5, xlab = paste0("P", age),
      main = paste0("Female disadvantage (", round(year), ")"),
      colinfo = c("lightpink", "maroon2"), lwd.main = 4, lwd.cutoff = 2      
    )
    ## above cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.plotall=FALSE, select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c), data.cqt = P.cqt,
      cutoff = cutoffP, order = 2, xlab = paste0("P", age), cex = 1.5,
      main = paste0("Male disadvantage (", round(year), ")"),
      colinfo = c("slategray1", "mediumblue"), lwd.main = 4, lwd.cutoff = 2
    )
    dev.off()
    
    ########################
    ## plot all countries ##
    pdf(paste0(Pplot.dir, "P", age, "_", runname, "_allcountry_",
               round(year), ".pdf"), height = 45, width = 10)
    par(mfrow = c(1, 1), mar = c(3, 13, 1.5, 1), cex.lab = 1.4,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)
    ## below cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c), data.cqt = P.cqt,
      cutoff = cutoffP, order = 1, cex = 1.5, xlab = paste0("P", age),
      main = paste0("Female disadvantage (", round(year), ")"),
      colinfo = c("lightpink", "maroon2"), lwd.main = 4, lwd.cutoff = 2
    )
    dev.off()
    
  }#end of age loop
}#end of t loop


#################################################################
## countries are included only if P sign. Different from 1 AND
## difference in Qfemale and Qmale is at least 5 per 1,000 ##
q.limit <- c(10 / Qunit, 20 / Qunit)

for (year in years.select) {
  for (age in ages.a) {
    a <- which(ages.a == age)
    P.cqt <- res[[paste0("P", age, ".cqt")]]
    Q.ct <- list(Q1.ct, Q4.ct, Q5.ct)[[a]]
    
    for (pick in 1:2) {
      
      pdf(paste0(Pplot.dir, "P", age, "_", runname, "_", round(year),
                 "_Q", age, "above", q.limit[pick] * Qunit, ".pdf"),
          height = 7, width = 10)
      par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
          mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)
      ## below cutoff ##
      PlotCIsegments(
        Xaxis = TRUE, if.plotall = FALSE, data.cqt = P.cqt,
        select.c = (Q.ct[, t] >= q.limit[pick]), select.t = (year == years.t),
        countryName.c = InternalMakeCountryNamesShort(name.c),
        cutoff = cutoffP, lwd.cutoff = 2, order = 1, xlab = paste0("P", age),
        main = paste0("Female disadvantage (", round(year), ")"),
        colinfo = c("lightpink", "maroon2"), lwd.main = 4, cex = 1.5
      )
      ## above cutoff ##
      PlotCIsegments(
        Xaxis = TRUE, if.plotall = FALSE, data.cqt = P.cqt,
        select.c = (Q.ct[, t] >= q.limit[pick]), select.t = (year == years.t),
        countryName.c = InternalMakeCountryNamesShort(name.c),
        cutoff = cutoffP, cex = 1.5, order = 2, xlab = paste0("P", age),
        main = paste0("Male disadvantage (", round(year), ")"),
        colinfo = c("slategray1", "mediumblue"), lwd.main = 4, lwd.cutoff = 2
      )
      dev.off()
    }#end of pick loop
  }#end of age loop
}#end t loop


#################################################################
## countries are included only if P sign.
# at least Ppercent% different from 1
Ppercent <- 10

for (year in years.select) {
  ## only plot countries with total disadvantage for girls or boys ##
  for (age in ages.a) {
    P.cqt <- res[[paste0("P", age, ".cqt")]]
    
    pdf(paste0(Pplot.dir, "P", age, "_", runname, "_", round(year),
               "_bigger", Ppercent, "percent_absdiff.pdf"),
        height = 7, width = 10)
    par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)
    
    ## below cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.plotall = FALSE, data.cqt = P.cqt,
      select.c = (P.cqt[, 2, select.t] < 1 * (100 - Ppercent) / 100),
      select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c),
      cutoff = cutoffP, cex = 1.5, order = 1, xlab = paste0("P", age),
      main = paste0("Female disadvantage (", round(year), ")"),
      colinfo = c("lightpink", "maroon2"), lwd.main = 4, lwd.cutoff = 2
    )
    ## above cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.plotall = FALSE, data.cqt = P.cqt,
      select.c = (P.cqt[, 2, t] > 1 * (100 + Ppercent) / 100),
      select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c),
      cutoff = cutoffP, cex = 1.5, order = 2, xlab = paste0("P", age),
      main = paste0("Male disadvantage (", round(year), ")"),
      colinfo = c("slategray1", "mediumblue"), lwd.main = 4,lwd.cutoff = 2
    ) 
    dev.off()
  }#end of age loop
}#end year loop

## the end ##

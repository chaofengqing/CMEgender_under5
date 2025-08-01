###############################################################################
## only plot countries with total disadvantage for girls or boys ##
## plot for excess female Q1/4/5 in selected years ##
years.select <- c(1990, 2000, 2010, 2012) + 0.5

for (year in years.select) {
  for (age in ages.a) {
    excQf.cqt <- res[[paste0("excQ", age, "f.cqt")]] * Qunit
    
    pdf(paste0(excQplot.dir, "excQfemale", age, "_", runname, "_",
               round(year), ".pdf"), height = 7, width = 10)
    par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)  
    ## below cutoff ##
    PlotCIsegments(
      if.plotall = FALSE, data.cqt = excQf.cqt, select.t = (year == years.t),
      countryName.c = InternalMakeCountryNamesShort(name.c),
      cutoff = cutoffQ, order = 2, cex = 1.5, lwd.main = 4, lwd.cutoff = 2,
      xlab = paste("excess female", ageGroupName.a[age == ages.a],
                   "(per 1000)"), colinfo = c("lightpink", "maroon2"),
      main = paste0("Female disadvantage (", round(year), ")")
    )
    ## above cutoff ##
    PlotCIsegments(
      if.plotall=FALSE, select.t = (year == years.t), data.cqt = excQf.cqt,
      countryName.c = InternalMakeCountryNamesShort(name.c),
      cutoff = cutoffQ, order = 1, cex = 1.5, lwd.main = 4, lwd.cutoff = 2,
      xlab = paste("excess female", ageGroupName.a[age == ages.a],
                   "(per 1000)"), colinfo = c("slategray1", "mediumblue"),
      main = paste0("Male disadvantage (", round(year), ")")
    )
    dev.off()
    
    ########################
    ## plot all countries ##
    pdf(paste0(excQplot.dir, "excQfemale", age, "_", runname, "_allcountry_",
               round(year), ".pdf"), height = 45, width = 10)
    par(mfrow = c(1, 1), mar = c(3, 13, 1.5, 1), cex.lab = 1.4,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)
    ## below cutoff ##
    PlotCIsegments(
      select.t = (year == years.t), data.cqt = excQf.cqt,
      countryName.c = InternalMakeCountryNamesShort(name.c),
      cutoff = cutoffQ, order = 1, cex = 1.5, lwd.main = 4, lwd.cutoff = 2,
      xlab = paste("excess female", ageGroupName.a[age == ages.a],
                    "(per 1000)"), colinfo = c("lightpink", "maroon2"),
      main = paste0("Female disadvantage (", round(year), ")") 
    )
    dev.off()
    
  }#end of age loop
}#end of t loop


#################################################################
## countries are included only if excQfemale sign. Different from 1 AND
## difference in Qfemale and Qmale is at least 5 per 1,000 ##
q.limit <- c(10 / Qunit, 20 / Qunit)

for (year in years.select) {
  for (age in ages.a) {
    a <- which(ages.a == age)
    excQf.cqt <- res[[paste0("excQ", age, "f.cqt")]] * Qunit
    Q.ct <- list(Q1.ct, Q4.ct, Q5.ct)[[a]]
    
    for (pick in 1:2) {
      
      pdf(paste0(excQplot.dir, "excQfemale", age, "_", runname, "_",
                 round(year), "_Q", age, "above", q.limit[pick] * Qunit,
                 ".pdf"), height = 7, width = 10)
      par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
          mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)
      ## below cutoff ##
      PlotCIsegments(
        if.plotall = FALSE, data.cqt = excQf.cqt,
        select.c = (Q.ct[, t] >= q.limit[pick]), select.t = (year == years.t),
        countryName.c = InternalMakeCountryNamesShort(name.c),
        cutoff = cutoffQ, lwd.cutoff = 2, order = 2, lwd.main = 4, cex = 1.5,
        xlab = paste("excess female", ageGroupName.a[age == ages.a],
                     "(per 1000)"), colinfo = c("lightpink", "maroon2"),
        main = paste0("Female disadvantage (", round(year), ")")
      )
      ## above cutoff ##
      PlotCIsegments(
        if.plotall = FALSE, data.cqt = excQf.cqt,
        select.c = (Q.ct[, t] >= q.limit[pick]), select.t = (year == years.t),
        countryName.c = InternalMakeCountryNamesShort(name.c),
        cutoff = cutoffQ, cex = 1.5, order = 1, lwd.main = 4, lwd.cutoff = 2,
        xlab = paste("excess female", ageGroupName.a[age == ages.a],
                     "(per 1000)"), colinfo = c("slategray1", "mediumblue"),
        main = paste0("Male disadvantage (", round(year), ")")
      )
      dev.off()
    }#end of pick loop
  }#end of age loop
}#end t loop

## the end ##


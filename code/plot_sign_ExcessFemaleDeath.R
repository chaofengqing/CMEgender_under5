

##############################################################
## plot for excess female deaths 1/4/5 in selected years ##
years.select <- c(1990, 2000, 2010, 2012) + 0.5

for (year in years.select) {
  for (age in ages.a) {
    excDf.cqt <- res[[paste0("excD", age, "f.cqt")]]
    select.t <- (year == years.t)
    # only include countries with UIs outside [-1, 1]
    select.c <- apply(abs(excDf.cqt[, , select.t]) > 1, 1, prod) == 1
    
    #####################################################################
    ## only plot countries with total difference from 0 for excDfemale ##
    pdf(paste0(excDplot.dir, "excDfemale", age, "_", runname, "_",
               round(year), "(log10-scale).pdf"), height = 7, width = 10)
    par(mfrow = c(1, 2), mar = c(3, 7.5, 1.5, 1), cex.lab = 1,
        mgp = c(1.5, 0.2, 0), cex.main = 1, cex.axis = 0.9, tcl = -0.3)  
    ## above cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.log10Scale = TRUE, if.plotall = FALSE,
      data.cqt = excDf.cqt, select.t = select.t, select.c = select.c,
      countryName.c = InternalMakeCountryNamesShort(name.c), cex = 1.5,
      cutoff = 1, order = 2, lwd.main = 4, lwd.cutoff = 2,
      xlab = paste("Excess Female Deaths", age, "(log10-scale)"), 
      main = paste0("Female disadvantage (", round(year), ")"),
      colinfo = c("lightpink", "maroon2")
    )  
    
    ## below cutoff ##
    PlotCIsegments(
      Xaxis = TRUE, if.log10Scale = TRUE, if.plotall = FALSE,
      data.cqt = excDf.cqt, select.t = select.t, select.c = select.c,
      countryName.c = InternalMakeCountryNamesShort(name.c), cex = 1.5,
      cutoff = -1, order = 1, lwd.main = 4, lwd.cutoff = 2,
      xlab = paste("Excess Female Deaths", age, "(negative log10-scale)"), 
      main = paste0("Male disadvantage (", round(year), ")"),
      colinfo = c("slategray1", "mediumblue")
    )     
    dev.off()
    
  }## end of age loop  
}#end of year loop


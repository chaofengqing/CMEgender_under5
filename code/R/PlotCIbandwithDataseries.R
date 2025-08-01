
PlotCIbandwithDataseries <- function(
  plotLegendOnly = FALSE,
  if.NewPlot = TRUE,        # if start a new plot
  if.SurveyLegend = FALSE,  # if plot legend for surveys
  if.sepLegendPage = FALSE, # if plot legend on a seperate page
  if.xlimFix = FALSE,       # if fix xlim for the plot
  if.rescaleXaxis = FALSE,  # if re-scale x-axis
  if.rescaleYaxis = FALSE,  # if re-scale y-axis  
  unique.sources = NULL,    # need all unique sources to make sure colors match up across plots
  datalim = c(0.5, 1.2),    # ylim for data series
  SElim = c(-0.1, 5),       # ylim for SE of data series
  CI1s = NULL, CI2s = NULL, CI3s = NULL, # CIs for estimates; able to plot three CIs at most
  dataseries = NULL,   # data seris, i.e. observed data
  dataseriesSE = NULL, # standard error for data series, dataseries -/+ 1.96 * dataseriesSE
  year.t = years.t,    # x position for CI
  Source = NULL,       # type of source for data series
  baseSeries = NULL,   # specify the name of base data series
  x,
  select.x = seq(1, length(x)), # select data from one coutry if x is a vector from multiple countries
  cutoff = c(0, 1),
  
  ## the following arguments are about plotting features:
  nameCI1 = NULL, nameCI2 = NULL, nameCI3 = NULL, # names for CI1/2/3 for the legend
  main = NULL, ylab = NULL, xlab = NULL,
  lwd.dataseries = 1, # lwd for dataseries connection line
  lwd.CI1 = 5, lwd.CI2 = 4, lwd.CI3 = 4, # lwd for median/best estimate line of CIs
  cex.legend = 1.5, # font size of both lengend for CIs and data series
  legendCI.posi = "bottomright",    # position of legend for CIs
  legendSurvey.posi = "bottomleft", # position of legend for data series, i.e. surveys
  cex.dataseries = 1, # dot size for data series
  colbase = "black",  # col for the base data series
  colCI = c("red", "darkgreen", "limegreen"), # col for CI1/2/3 and their estimates
  # level of transparency of:
  alpha.CI      = 0.35, # CI band
  alpha.line    = 0.4,  # median/best estimate line of CIs
  alpha.polygon = 0.08, # SE band of data series
  alpha.dataseries = 0.5, # connection line for data series
  alpha.point   = 0.95  # dots of data series
) {
  
  ## plot function for 95% (multiple) CIs
  # with/without (multiple) data series and connection lines ##
  
  if.CI1s <- ifelse(!is.null(CI1s), TRUE, FALSE)
  if.CI2s <- ifelse(!is.null(CI2s), TRUE, FALSE)
  if.CI3s <- ifelse(!is.null(CI3s), TRUE, FALSE)          
  if.dataseries <- ifelse(!is.null(dataseries), TRUE, FALSE)    
  if.dataseriesSE <- ifelse(!is.null(dataseriesSE), TRUE, FALSE)
  if.baseSeries <- ifelse(!is.null(baseSeries), TRUE, FALSE)
  if.CILegend <- ifelse(prod(is.null(c(nameCI1, nameCI2, nameCI3))) == 1, 
                        FALSE, TRUE)
  
  # 23 colors for multiple surveys from data series.
  # number of colors is enough: Bangladesh has the largest number of
  # unique surveys across age groups for 2013 project which is 13.  
  colchart <- c("darkorange1",
                "darkorchid",
                "goldenrod3",
                "hotpink2",
                "seagreen3",
                "slateblue",
                "yellowgreen",
                "springgreen4",
                "salmon",
                "lightgoldenrod4",
                "brown1",
                "darkblue",
                "darkgreen",
                "olivedrab3",
                "orangered",
                "palegreen3",
                "peru",
                "maroon",
                "cornflowerblue",
                "chartreuse3",
                "hotpink",
                "indianred1",
                "plum4"
  ) # FQ: rearrange colors to look nicer
  
  # set at the beginning in case no input data series for a country when we
  # only want to plot data series.
  collegend <- legendnames <- NULL
  
  if (plotLegendOnly) {
    #LA: not sure if this is the best idea....
    return() 
  }
  
  if (
    (if.dataseries & (length(x) == 0 | length(select.x) == 0)) & 
      is.null(CI1s) & is.null(CI2s) & is.null(CI3s)
  ) {
    if (if.NewPlot) {
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = main)
    }    
  } else {
    
    xLim <- range(x, na.rm = TRUE) # xlim when considering all x across grop
    x <- x[select.x]
    
    if (if.xlimFix) {
      xmin <- xLim[1]
      xmax <- xLim[2]
    } else {
      xmin <- min(x, year.t[!is.na(CI1s[1, ])], na.rm = TRUE)
      xmax <- max(x, year.t[!is.na(CI1s[1, ])], na.rm = TRUE)
    }
    
    Source <- Source[select.x]
    dataseries <- dataseries[select.x]
    # need to construct SE.qy in the standard way no matter we want to
    # plot SE for data series or not. If if.dataseriesSE = FALSE, still
    # need to assign dataseriesSE <- NA since its default value is NULL
    if (!if.dataseriesSE) {
      dataseriesSE <- NA
    }
    
    # *.qy: row in percentiles = c(0.025, 0.5, 0.975)
    # column in observed year for a specific data series (instead of years.t)
    SE.qy <- rbind(dataseries - 1.96 * dataseriesSE[select.x],
                   dataseries,
                   dataseries + 1.96 * dataseriesSE[select.x])
    
    # add max to plotting ylims when SEs are included
    if (if.dataseriesSE & if.dataseries) {
      Ylim = c(max(SElim[1], min(datalim[1], SE.qy, dataseries, cutoff,
                                 CI1s, CI2s, CI3s, na.rm = TRUE)),
               min(SElim[2], max(datalim[2], SE.qy, dataseries, cutoff,
                                 CI1s, CI2s, CI3s, na.rm = TRUE)))      
    }
    if(!(if.dataseriesSE) & if.dataseries) {
      Ylim = c(max(datalim[1], min(cutoff, dataseries,
                                   CI1s, CI2s, CI3s, na.rm = TRUE)),
               min(datalim[2], max(cutoff, dataseries,
                                   CI1s, CI2s, CI3s, na.rm = TRUE)))
    }
    if(!(if.dataseriesSE) & !(if.dataseries)) {
      Ylim = c(min(datalim[1], cutoff, CI1s, CI2s, CI3s, na.rm = TRUE),
               max(datalim[2], cutoff, CI1s, CI2s, CI3s, na.rm = TRUE))
    }
    
    
    if(if.NewPlot) {
      plot(1, type = "n", xlim = c(xmin, xmax), ylim = Ylim,
           xaxt = ifelse(if.rescaleXaxis, "n", "s"),
           yaxt = ifelse(if.rescaleYaxis, "n", "s"),
           ylab = ylab, xlab = xlab, main = main)
    }
    
    abline(h = cutoff)
    
    
    sources <- unique(Source)
    
    if (!is.null(unique.sources)) {
      colchart <- colchart[1:length(unique.sources)]
      names(colchart) <- unique.sources
    } else {
      colchart <- colchart[1:length(sources)]
      names(colchart) <- sources
    }#end of ifelse (!is.null(unique.sources))
    
    if (if.dataseries | if.dataseriesSE) {
      sources <- as.character(sources[!is.na(sources)])
      
      for (sur in sources) {
        select.i <- which(Source == sur)
        
        col.sur <- ifelse(if.baseSeries & is.element(sur, baseSeries),
                          colbase, colchart[sur])
        
        PlotCIbands(if.CI = if.dataseriesSE, if.line = if.dataseries,
                    CIs.qt = SE.qy[, select.i], year.t = x[select.i],
                    col = col.sur, alpha.CI = alpha.polygon,
                    lwd.CI = lwd.dataseries, alpha.line = alpha.dataseries)
        # alpha.polygon < alpha.CI, SE is lighter than CIs from CIs1/2/3.        
      }#end of sources loop
      
    }#end of if (if.dataseries | if.dataseriesSE)
    
    ############################
    ## plot CIs and estimates ##
    PlotCIbands(if.CI = if.CI1s, if.line = if.CI1s, alpha.line = alpha.point,
                year.t = year.t, CIs.qt = CI1s, col = colCI[1], lwd.CI = lwd.CI1)
    
    PlotCIbands(if.CI = if.CI2s, if.line = if.CI2s, alpha.line = alpha.point,
                year.t = year.t, CIs.qt = CI2s, col = colCI[2], lwd.CI = lwd.CI2)
    
    PlotCIbands(if.CI = if.CI3s, if.line = if.CI3s, alpha.line = alpha.point,
                year.t = year.t, CIs.qt = CI3s, col = colCI[3], lwd.CI = lwd.CI3)    
    
    #############################
    ## plot data series points ##
    for (sur in sources) {
      col.pt <- adjustcolor(ifelse(if.baseSeries & is.element(sur, baseSeries),
                                   colbase, colchart[sur]),
                            alpha.f = alpha.point)
      
      select.i <- which(Source == sur)
      order.x <- select.i[order(x[select.i])]
      
      if (if.dataseries) {
        points(dataseries[order.x] ~ x[order.x],
               col = col.pt, pch = 19, cex = cex.dataseries)
      }
      legendnames <- c(legendnames, sur)
      collegend <- c(collegend, col.pt)  
    }#end of sur loop
    
  }#end of if (length(x)!=0 & length(select.x)>0) loop
  
  #################
  ## plot legend ##
  if (if.CILegend) {
    legend(legendCI.posi, legend = c(nameCI1, nameCI2, nameCI3),
           col = colCI, lwd = 5, pch = -1, lty = c(1, 1), cex = cex.legend)
  }#end of if (if.CILegend)
  
  if(if.SurveyLegend) {
    if (if.sepLegendPage) {
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    }
    if (length(legendnames) > 0) {
      legend(ifelse(if.sepLegendPage, "left", legendSurvey.posi),
             legend = legendnames, col = collegend, cex = cex.legend,
             lwd = 4, lty = 1, bty = ifelse(if.sepLegendPage, "n", "o"))
    }#end of if (length(legendnames) > 0)    
  }#end of if.SurveyLegend  
  
}#end of PlotCIbandwithDataseries function


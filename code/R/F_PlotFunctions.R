

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 13 Mar 2014
# 
# F_PlotFunctions.R
# 
# This script contains all functions related to plot
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# PlotTrace(..)
# PlotPostSDWithGammaPrior(..)
# PlotPostWithUnifPrior(..)
# PlotCIbands(..)
# PlotCIandLoess(..)
# PlotCIsegments(..)
# PlotCIbandwithDataseries(.., PlotCIbands(4), ..)
# 
###############################################################################


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

PlotTrace <- function(#Traceplot for one parameter
  ## Trace plot for one parameter and add loess smoother for each chain ##
  parname, mcmc.array,##<< needs to be 3-dimensional array!
  n.chains = NULL, n.sim = NULL, main = NULL){
  if (is.null(main)) main <- parname
  if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
  if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[, 1, parname]), type = "l", ylab = parname,  main = main,
       #        ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname]))
       ylim = range(c(mcmc.array[, , parname]), na.rm = TRUE))
  for (chain in 1:n.chains) {
    lines(c(mcmc.array[, chain, parname]), type = "l", col = chain)
  }
  for (chain in 1:n.chains) {
    curve(predict(loess(c(mcmc.array[, chain, parname]) ~ seq(1, n.sim)), x), 
          lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
  
}#end of PlotTrace function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

PlotPostSDWithGammaPrior <- function(
  ### Plot histogram of posterior sample of a SD parameter,
  # and add the prior, which is based on a Gamma for the precision
  post.samp, ##<< Posterior sample from MCMC.
  priorshape, ##<< Prior shape parameter of Gamma distribution
  priorrate, ##<< Prior rate parameter of Gamma distribution
  parname = NULL ##<< Parameter name for x-axis label.
) {
  # more precisely, variance ~ InvGamma
  # often used: shape = halfnu0, rate = halfnu0*sigma2
  par(mar = c(5, 5, 1, 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
  minx <- ifelse(min(post.samp) < 0, 1.1 * min(post.samp), 0.9 * min(post.samp))
  maxx <- ifelse(max(post.samp) < 0, 0.9 * max(post.samp), 1.1 * max(post.samp))
  minxprior <- ifelse(min(post.samp) < 0, 1.2 * min(post.samp), 0.8 * min(post.samp))
  maxxprior <- ifelse(max(post.samp) < 0, 0.8 * max(post.samp), 1.2 * max(post.samp))
  
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE,
       main = "", xlim = c(minx, maxx))
  # note: when using "density", use to and from if sample has large negative/positive values!
  lines(density(1 / sqrt(rgamma(10^6, shape = priorshape, rate = priorrate)),
                from = minx, to = maxx), col = 2, lwd = 3)
  
}#end of PlotPostSDWithGammaPrior function

# PlotPostSDWithGammaPrior(post.samp = runif(100,0.01,1), priorshape = 0.5,
# priorrate = 0.5, parname = "test")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

PlotPostWithUnifPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, ##<< Posterior sample from MCMC.
  priorlow, ##<< Prior lower bound of uniform distribution.
  priorup, ##<< Prior upper bound of uniform distribution.
  parname = NULL ##<< Parameter name for x-axis label.
){
  par(mar = c(5, 5, 1, 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp) < 0, 1.1 * min(post.samp), 0.9 * min(post.samp))
  maxx <- ifelse(max(post.samp) < 0, 0.9 * max(post.samp), 1.1 * max(post.samp))
  
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE,
       main = "", xlim = c(minx, maxx))
  h <- 1 / (priorup - priorlow)
  segments(priorlow, h, priorup, h, col = 2)
  
}#end of PlotPostWithUnifPrior function

# PlotPostWithUnifPrior(post.samp = rnorm(100,0), priorlow = -2,
#                       priorup = 10, parname = "test")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

PlotCIbands <- function(
  if.CI = TRUE, # if plot CI band
  if.line = TRUE, # if plot median or best estimate of CI
  CIs.qt, # q is 3; row 1-lower, row 2-median, row 3-upper
  year.t = years.t,
  col = "red", # color of the CI band
  alpha.CI = 0.35,
  alpha.line = 0.4,
  lwd.CI = 1 #lwd for line of median/best estimates
) {
  
  ## 95% CI plolygon plot over time ##
  if (!if.CI & !if.line) {
    return(invisible) #FQ: need to check if it is ok
  } else {
    order.year <- order(year.t)
    year.t <- year.t[order.year]
    
    if (if.CI) {
      if (is.vector(CIs.qt)) {
        CI.low.t <- CIs.qt[1]
        CI.up.t  <- CIs.qt[3]
      }
      if (is.matrix(CIs.qt)) {
        CIs.qt <- CIs.qt[, order.year]
        CI.low.t <- CIs.qt[1, ]
        CI.up.t  <- CIs.qt[3, ]
      }
      
      x.full <- c(year.t, rev(year.t), year.t[1])
      y.full <- c(CI.low.t, rev(CI.up.t), CI.low.t[1])
      
      #only plot points with non-missing (x,y)
      nonNA <- !is.na(y.full) & !is.na(x.full)
      x.plot <- x.full[nonNA]
      y.plot <- y.full[nonNA]
      polygon(
        x.plot, y.plot,
        border = adjustcolor(col = col,
                             alpha.f = ifelse(length(x.plot) == 2,
                                              alpha.line, alpha.CI / 10)),
        col = adjustcolor(col = col, alpha.f = alpha.CI))
    }#end of if.CI
    
    if (if.line & is.matrix(CIs.qt)) {
      noNA <- !is.na(year.t) #connect all non-NA data
      if (if.CI) {
        estimate.t <- CIs.qt[2, noNA]
      } else {
        estimate.t <- CIs.qt[2, order.year[noNA]]
      }#end of ifelse(if.CI)

      lines(estimate.t ~ year.t[noNA],
            lwd = lwd.CI, col = adjustcolor(col = col, alpha.f = alpha.line))
    }#end of if.line
    
  }#end of ifelse (!if.CI & !if.line)
  
}#end of PlotCIbands function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


PlotCIandLoess <- function(
  x, y, labelx, labelxlocation,
  xlim = rev(range(x, na.rm = TRUE)), ylim = c(0, 2),
  xlab = "", ylab = "",
  if.redoXaix = FALSE, #if replot x-axis with new scale
  if.loess = FALSE,    #if plot loess curve for obs
  if.CI = FALSE,       #if plot estimated CI bonds
  cutoff = NULL,       #horizontal line
  colCI = "purple",
  CI,                #must have 3 rows: 1.upper;2.best;3.lower
  CIx,                #x values correspond to estimated CI
  main = NULL,
  lwd.estimate = 5,
  lwd.CI = 3,
  cex.point = 0.5 #dot size for observations
) {
  ## plot observation dots, estimated CI, loess for obs ##
  
  plot(y ~ x, xlim = xlim, main = main, ylim = ylim, cex = cex.point,
       xaxt = ifelse(if.redoXaix, "n", "s"), xlab = xlab,
       ylab = ylab, col = "lavenderblush3", pch = 19)
  
  if (if.redoXaix) {
    axis(1, at = labelxlocation, label = labelx, las = 1)
  }
  abline(h = cutoff, col = 1, lwd = 3)
  
  # plot loess for 90% of q's to avoid the effect from outliers
  if (if.loess) {
    xtemp <- x
    Q <- quantile(xtemp, probs = c(0.05, 0.95), na.rm = TRUE) 
    curve(
      predict(loess(y ~ xtemp, na.action = "na.omit", family = "symmetric"), x),
      add = TRUE, col = "springgreen4", lwd = 9, from = Q[1], to = Q[2])
  }#end of if.loss
  
  lines(CI[2, ] ~ CIx, col = colCI, lwd = lwd.estimate)
  
  if (if.CI) {
    lines(CI[1, ] ~ CIx, col = colCI, lwd = lwd.CI, lty = 2)
    lines(CI[3, ] ~ CIx, col = colCI, lwd = lwd.CI, lty = 2)    
  }#end of if.CI
  
}#end of PlotCIandLoess function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------



PlotCIsegments <- function (
  if.plotall = TRUE, # if plot all of data.cqt[select.c, , select.t]
  if.oneSource = TRUE, # if only country data or a mix,
  # e.g. world & region & country?
  if.AtoZ = TRUE, # smallest at the top (when if.plotall = TRUE)
  if.noOrder = FALSE, # order by values by default;
  # if TRUE, use order of input data.
  if.log10Scale = FALSE, #whether to plot input data on log10 scale;
  data.cqt, # older version is "x"
  select.t = seq(1, dim(data.cqt)[3]), # default to select all years
  select.c = seq(1, dim(data.cqt)[1]), # default to select all countries
  year.t = years.t, # data.cqt, the indices of t
  yearOrder = NULL, # order countries based on values in
  # this year (only when if.oneSource = TRUE)
  order = 1, # further away from cutoff at the top
  plot.xaxis = TRUE, # whether to plot x-axis
  Xaxis = FALSE,     # when plot.xaxis = TRUE, whether to reformat x-axis
  Yaxis = TRUE,      # whether to plot y-axis
  countryName.c = NULL, # input of y-axis
  xlab = NULL, main = NULL,
  cutoff = NULL,
  lwd.main = 8, lwd.cutoff = 2, pch = 19, cex = 4,
  colinfo = c("slategray1", # background col
              "royalblue4", # primary line segment col, the one yearOrder is
              NULL          # secondary line col
  )) {
  
  ## 95% CI line segment plot for all counties/paras in one plot ##
  
  if (sum(select.c) == 0) {
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = main)    
  }
  
  data.nqt <- data.cqt[select.c, , ]
  name.n <- countryName.c[select.c]
  
  # plot multiple CIs within a plot
  if.addCI <- ifelse(sum(select.t) == 1, FALSE, TRUE)
  
  if (if.log10Scale) {
    sign.nqt <- (data.nqt < 0) * (-1)
    sign.nqt[sign.nqt == 0] <- 1
    data.nqt <- abs(log10(abs(data.nqt))) * sign.nqt
    normal.scale.cutoff <- cutoff
    cutoff <- log10(abs(normal.scale.cutoff)) * 
      ifelse(normal.scale.cutoff < 0, -1, 1)
  }
  
  if (length(dim(data.nqt)) == 2) { # data.nqt: the prime order and CI
    if (if.addCI) {
      if (if.noOrder) {
        CI1.t <- rep(FALSE, length(year.t))
        CI1.t[which(select.t)[1]] <- TRUE
      } else {
        CI1.t <- year.t == yearOrder
      }
      signCI1 <- ifelse(which(CI1.t) == min(which(select.t)), 1, -1)
      signCI2 <- signCI1 * (-1)
      CI2.t <- select.t + CI1.t == 1
      
      lower.n <- data.nqt[1, CI1.t]
      median.n <- data.nqt[2, CI1.t]
      upper.n <- data.nqt[3, CI1.t]
    } else {
      lower.n <- data.nqt[1, select.t]
      median.n <- data.nqt[2, select.t]
      upper.n <- data.nqt[3, select.t]
    }
    
    N <- 1
    
  }#end of if (length(dim(data.nqt)) == 2)
  
  if (length(dim(data.nqt)) == 3) { # data.nqt: the prime order and CI
    if (if.addCI) {
      if (if.noOrder) {
        CI1.t <- rep(FALSE, length(year.t))
        CI1.t[which(select.t)[1]] <- TRUE
      } else {
        CI1.t <- year.t == yearOrder
      }
      signCI1 <- ifelse(which(CI1.t) == min(which(select.t)), 1, -1)
      signCI2 <- signCI1 * (-1)
      CI2.t <- select.t + CI1.t == 1
      
      lower.n <- data.nqt[, 1, CI1.t]
      median.n <- data.nqt[, 2, CI1.t]
      upper.n <- data.nqt[, 3, CI1.t]
    } else {
      lower.n <- data.nqt[, 1, select.t]
      median.n <- data.nqt[, 2, select.t]
      upper.n <- data.nqt[, 3, select.t]
    }
    
    N <- dim(data.nqt)[1]
    
  }#end of if (length(dim(data.nqt)) == 3)
  
  
  
  if (order == 1) {
    if (if.plotall) {
      select.n <- rep(TRUE, N)
    } else {
      select.n <- upper.n < cutoff
    } 
    
    M <- sum(select.n)
    labels.m <- name.n[select.n]
    
    if (if.oneSource) {
      order.m <- rev(order(median.n[select.n]))
    } else {
      source.n <- dimnames(data.nqt)[[1]]
      source.s <- unique(source.n)
      S <- length(source.s)
      order.m <- NULL
      for (s in S:1) {
        n.selectSource <- which(source.n == source.s[s])
        order.m <- 
          c(order.m, n.selectSource[rev(order(median.n[n.selectSource]))])
      }#end of s loop
    }#end of if.oneSource
    
    if (if.plotall) {
      xmin <- ifelse(if.addCI, min(data.nqt[select.n, , ], na.rm = TRUE),
                     min(lower.n[select.n], na.rm = TRUE))
      xmax <- ifelse(if.addCI, max(data.nqt[select.n, , ], na.rm = TRUE),
                     max(upper.n[select.n], na.rm = TRUE))
    } else {
      xmin <- ifelse(if.addCI, min(data.nqt[select.n, , ], na.rm = TRUE),
                     min(lower.n[select.n], na.rm = TRUE))
      xmax <- ifelse(if.addCI, max(data.nqt[select.n, , ], na.rm = TRUE),
                     cutoff)
    }
    
    by <- ifelse(xmax - xmin < 0.3, 0.05, 0.1)
    nb <- trunc((xmax - xmin) / by)
    x2 <- ifelse(if.plotall, cutoff + nb * by, cutoff)
    x1 <- cutoff - nb * by
    
  }#end of if(order==1)
  
  if (order == 2) {
    if (if.plotall) {
      select.n <- rep(TRUE, N)
    } else {
      select.n <- lower.n > cutoff
    }
    
    M <- sum(select.n)
    labels.m <- name.n[select.n]
    
    if (if.oneSource) {
      order.m <- order(median.n[select.n])
    } else {
      source.n <- dimnames(data.nqt)[[1]]
      source.s <- unique(source.n)
      S <- length(source.s)
      order.m <- NULL
      for (s in S:1) {
        n.selectSource <- which(source.n == source.s[s])
        order.m <- c(order.m, n.selectSource[order(median.n[n.selectSource])])
      }#end of s loop
    }
    
    if(if.plotall) {
      xmin <- ifelse(if.addCI, min(data.nqt[select.n, , ], na.rm = TRUE),
                     min(lower.n[select.n], na.rm = TRUE))
      xmax <- ifelse(if.addCI, max(data.nqt[select.n, , ], na.rm = TRUE),
                     max(upper.n[select.n], na.rm = TRUE))
    } else {
      xmin <- ifelse(if.addCI, min(data.nqt[select.n, , ], na.rm = TRUE),
                     cutoff)
      xmax <- ifelse(if.addCI, max(data.nqt[select.n, , ], na.rm = TRUE),
                     max(upper.n[select.n], na.rm = TRUE))
    }
    
    by <- ifelse(xmax - xmin < 0.3, 0.05, 0.1)
    na <- trunc((xmax-xmin) / by)
    x1 <- ifelse(if.plotall, cutoff - na * by, cutoff)
    x2 <- cutoff + na * by
    
  }#end of if(order==2)
  
  
  if (!if.AtoZ) {
    order.m <- rev(order.m)
  }
  if (if.noOrder) {
    order.m <- rev(seq(1, length(order.m)))
  }
  
  labelsOrdered.m <- labels.m[order.m]
  
  
  if (M == 0) {
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = main)    
  }
  
  if (M > 0) {
    lower.m <- c(lower.n[select.n])[order.m]
    upper.m <- c(upper.n[select.n])[order.m]
    median.m  <- c(median.n[select.n])[order.m]
    
    temp <- rnorm(M, 0, 1)
    temp.iq <- cbind(temp * runif(M, 0.7, 0.9), temp,
                     temp * runif(M, 1.05, 1.3))
    add <- 3 / M
    
    if (length(colinfo) == 2) {
      colinfo <- c(colinfo, substr(colinfo[2], 1, nchar(colinfo[2]) - 1))
    }
    
    plot(seq(1, M) ~ temp.iq[, 2], type = "n", ylab = "", yaxt = "n", 
         xaxt = ifelse(plot.xaxis, ifelse(Xaxis, "n", "s"), "n"),
         xlim = c(xmin, xmax),
         ylim = c(
           ifelse(M <= 12, 1 - add, 1 - add / 3),
           ifelse(if.oneSource, M + add / 3, M + 1 / 2 + 2 / S * (S - 1))),
         xlab = xlab, main = main)
    
    if (Xaxis & !if.log10Scale) {
      axis(1, las = 1, at = seq(x1, x2, by), label = seq(x1, x2, by),
           tick = TRUE)
      abline(v = seq(x1, x2, by), col = "lightgrey", lty = 2)
    }
    if (Xaxis & if.log10Scale) {
      if (normal.scale.cutoff < 0) {
        xlabel <- c(-1 * 10^seq(4, 0))
      } else {
        xlabel <- c(10^seq(0, 6))
      }
      LOG10xlabel <- log10(abs(xlabel)) * (1 - 2 * (xlabel < 0))
      axis(1, at = LOG10xlabel, label = xlabel, las = 1)
      abline(v = LOG10xlabel, col = "lightgrey", lty = 2)
    }#end of if (Xaxis & if.log10Scale)
    
    if (if.oneSource) {
      if (Yaxis) {
        axis(2, las = 1, at = seq(1, M), label = labelsOrdered.m, tick = TRUE)
      }
      for (i in seq(1, M, 2)) {
        polygon(-0.5 + c(abs(xmin) * (-2), abs(xmin) * (-2),
                         abs(xmax) * 2, abs(xmax) * 2,
                         abs(xmin) * (-2)) + c(-1, -1, 1, 1, -1) * 10^5,
                i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
                col = adjustcolor(colinfo[1], alpha.f = 0.8), border = NA)
      }#end of i loop
      abline(v = cutoff, col = colinfo[2], lwd = lwd.cutoff)
      
    } else {
      abline(v = cutoff, col = colinfo[2], lwd = lwd.cutoff)
      
      add.source <- 2 / S
      source.start.point <- 0
      for (s in S:1) {
        n.s <- sum(source.n == source.s[s])
        for (i in seq(1 + source.start.point, n.s + source.start.point, 2)) {
          polygon(
            -0.5 + c(abs(xmin) * (-2), abs(xmin) * (-2), abs(xmax) * 2,
                     abs(xmax) * 2, abs(xmin) * (-2)) +
              c(-1, -1, 1, 1, -1) * 10^5,
            i + c(-0.5, 0.5, 0.5, -0.5, -0.5) + add.source * abs(s - S),
            col = adjustcolor(
              colinfo[1],
              alpha.f = ifelse(s == 1, 1, 0.2 + abs(s - S) * 0.5)),
            border = "grey")
          # add cutoff line segment that is blocked by background polygon
          segments(x0 = cutoff, y0 = i - 0.5 + add.source * abs(s - S),
                   x1 = cutoff, y1 = i + 0.5 + add.source * abs(s - S),
                   col = colinfo[2], lwd = lwd.cutoff)
        }#end of i loop
        
        if (Yaxis) {
          axis(2, las = 1, 
               at = seq(1 + source.start.point, n.s + source.start.point) +
                 add.source * abs(s - S), 
               label = labelsOrdered.m[seq(1 + source.start.point,
                                           n.s + source.start.point)],
               tick = TRUE)
        }#end of if (Yaxis)
        
        # have a nicer box around the plots.
        box()
        
        ## prime CI ##        
        segments(
          lower.m[seq(1 + source.start.point, n.s + source.start.point)],
          seq(1 + source.start.point, n.s + source.start.point) + 
            ifelse(if.addCI, add / 2 * signCI1, 0) + add.source * abs(s - S),
          upper.m[seq(1 + source.start.point, n.s + source.start.point)],
          seq(1 + source.start.point, n.s + source.start.point) + 
            ifelse(if.addCI, add / 2 * signCI1, 0) + add.source * abs(s - S),
          lwd = lwd.main, col = colinfo[2]) 
        points(
          seq(1 + source.start.point, n.s + source.start.point) + 
            add.source * abs(s - S) + ifelse(if.addCI, add / 2 * signCI1, 0) ~
            median.m[seq(1 + source.start.point, n.s + source.start.point)], 
          col = colinfo[2], pch = pch, cex = cex)
        
        if (if.addCI) {
          ## B/W estimates and UI ##
          segments(
            lower.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add / 2 * signCI1 + add.source * abs(s - S),
            upper.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add / 2 * signCI1 + add.source * abs(s - S), 
            lwd = 1, col = 1)
          points(
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI1 ~
              median.m[seq(1 + source.start.point, n.s + source.start.point)],
            col = 1, pch = 21, bg = colinfo[2], cex = cex - 1 / M)
          
          ## the secondary CI ##
          lower2.m <- c(data.nqt[select.n, 1, CI2.t])[order.m]
          upper2.m <- c(data.nqt[select.n, 3, CI2.t])[order.m]
          median2.m <- c(data.nqt[select.n, 2, CI2.t])[order.m]
          
          ## colorful estimates and UI ##
          segments(
            lower2.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2,
            upper2.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2,
            lwd = lwd.main, col = colinfo[3])
          points(
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2 ~
              median2.m[seq(1 + source.start.point, n.s + source.start.point)],
            col = colinfo[3], pch = pch, cex = cex)
          
          ## B/W estimates and UI ##
          segments(
            lower2.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2,
            upper2.m[seq(1 + source.start.point, n.s + source.start.point)],
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2,
            lwd = 1, col = 1)
          points(
            seq(1 + source.start.point, n.s + source.start.point) + 
              add.source * abs(s - S) + add / 2 * signCI2 ~
              median2.m[seq(1 + source.start.point, n.s + source.start.point)],
            col = 1, pch = 21, bg = colinfo[3], cex = cex - 1 / M)
          
        }#end of if (if.addCI)
        
        source.start.point <- source.start.point + n.s
        
      }#end of s loop
    }#end of ifelse(if.oneSource)
    
    if (if.oneSource) {
      ## the prime CI ##
      segments(
        lower.m, seq(1, M) +
          ifelse(if.addCI, add / 2 * signCI1, 0) / ifelse(M <= 10, 2, 1),
        upper.m, seq(1, M) +
          ifelse(if.addCI, add / 2 * signCI1, 0) / ifelse(M <= 10, 2, 1),
        lwd = lwd.main, col = colinfo[2])
      points(
        seq(1, M) +
          ifelse(if.addCI, add / 2 * signCI1, 0) / ifelse(M <= 10, 2, 1) ~
          median.m, col = colinfo[2], pch = pch, cex = cex)
      
      if (if.addCI) {
        ## B/W estimates and UI ##
        segments(
          lower.m, seq(1, M) + (add / 2 * signCI1) / ifelse(M <= 10, 2, 1),
          upper.m, seq(1, M) + (add / 2 * signCI1) / ifelse(M <= 10, 2, 1),
          lwd = 1, col = 1)
        points(
          seq(1, M) + (add / 2 * signCI1) / ifelse(M <= 10, 2, 1) ~ median.m, 
          col = 1, pch = 21, bg = colinfo[2], cex = cex - 1 / M)
        
        ## the secondary CI ##
        lower2.m <- c(data.nqt[select.n, 1, CI2.t])[order.m]
        upper2.m <- c(data.nqt[select.n, 3, CI2.t])[order.m]
        median2.m  <- c(data.nqt[select.n, 2, CI2.t])[order.m]
        ## colorful estimates and UI ##
        segments(
          lower2.m, seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1),
          upper2.m, seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1), 
          lwd = lwd.main, col = colinfo[3])
        points(
          seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1) ~
            median2.m, col = colinfo[3], pch = pch, cex = cex)
        ## B/W estimates and UI ##
        segments(
          lower2.m, seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1),
          upper2.m, seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1),
          lwd = 1, col = 1)
        points(
          seq(1, M) + (add / 2 * signCI2) / ifelse(M <= 10, 2, 1) ~
            median2.m, col = 1, pch = 21, bg = colinfo[3], cex = cex - 1 / M)
        
      }#end of if(if.addCI)
      
    }#end of if(if.oneSource) 
    
  }##end of if(length(select.n)>0)
}#end of PlotCIsegments function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

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


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


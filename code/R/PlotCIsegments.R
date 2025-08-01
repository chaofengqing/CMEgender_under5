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
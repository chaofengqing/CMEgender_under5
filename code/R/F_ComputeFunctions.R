

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# F_ComputeFunctions.R
# 
# This script contains all functions for computations relate to splines,
# JAGS output and its convergence, getting output for full observation period.
# note: for computation of mortality related functions, see F_GenderFunctions.R
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# GetSplines(..)
# GetSplines_Constantafterx0(..)
# GetSplines_ConstantBeforexstartAndAfterx0(..)
# GetSplinesResults(.., GetSplines_ConstantBeforexstartAndAfterx0(1), ..)
# Rhat1(..)
# Rhat(.., Rhat1(3), ..)
# getPostInfo(.., Rhat(1), ..)
# ReadJagsOutput(..)
# InternalMakeCountryNamesShort(..)
# ExternalMakeCountryNamesFull(..)
# InternalGetARTrajectories(..)
# StandardizeSignifDigit(..)
# StandardizeDecimal(..)
# 
###############################################################################


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetSplines <- function(
  x.t, # x (without NAs) for which splines need to be calculated (determines the number of rows of B.tk)
  x0 = NULL, # determines knot placement (for default, see below)
  IntervalLength = 2.5 # interval length between two knots
){
  
  # function used to get B-splines (that adds in additional splines at boundary to avoid boundary conditions)
  if (sum(is.na(x.t) != 0)) {
    print("Stop: no missing values allowed for x.t")
    return()
  }
  if (is.null(x0)) { # default setting: knot is placed half-interval before last observation
    x0 <- max(x.t) - 0.5 * IntervalLength
  } 
  # get knots, given that one knot needs to be in x0
  # do NOT make any assumptions about where x0 is compared to x.t!  
  knots <- seq(x0 - 1000 * IntervalLength, x0 + 1000 * IntervalLength,
               IntervalLength)
  
  while (min(x.t) < knots[1]) {
    knots <- c(seq(knots[1] - 1000 * IntervalLength, knots[1] - IntervalLength,
                   IntervalLength), knots)
  }
  while (max(x.t) > knots[length(knots)]) {
    knots <- c(knots,
               seq(knots[length(knots)] + IntervalLength,
                   knots[length(knots)] + 1000 * IntervalLength, IntervalLength))
  }
  
  # note that intercept is false, and knots are exactly as specified in "knots"
  Btemp.tk <-  bs(x.t, knots = knots[-c(1, length(knots))],  
                  Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.tk, 2, sum) > 0)
  
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol   <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.tk            <- Btemp.tk[, startnonzerocol:endnonzerocol]
  alphax.k        <- knots[startnonzerocol:endnonzerocol]
  ux.q            <- alphax.k[-c(1, length(alphax.k))]
  
  # return arguments:
  return(list(B.tk = B.tk, # matrix with one row for each element of x.t, and each column is a spline
              alphax.k = alphax.k, # the knots (x values)
              ux.q = ux.q # same as alphax.k, but excluding the first and last knot
  ))
  
}#end of GetSplines function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetSplines_Constantafterx0 <- function(
  x.t, # x (without NAs) for which splines need to be calculated (determines the number of rows of B.tk)
  x0, 
  # determines knot placement 
  # no longer allowed to be null: no new splines added after xo!
  
  IntervalLength = 2.5 # interval length between two knots
){
  if (sum(is.na(x.t) != 0)) {
    print("Stop: no missing values allowed for x.t")
    return()
  }
  # if (is.null(x0)){ # default setting: knot is placed half-interval before last observation
  #   x0 = max(x.t)-0.5*IntervalLength
  # } 
  # get knots, given that one knot needs to be in x0
  # do NOT make any assumptions about where x0 is compared to x.t!  
  knots <- seq(x0 - 1000 * IntervalLength, x0 + 1000 * IntervalLength,
               IntervalLength)
  
  while (min(x.t) < knots[1]) {
    knots <- c(seq(knots[1] - 1000 * IntervalLength, knots[1] - IntervalLength,
                   IntervalLength), knots)
  }
  while (max(x.t) > knots[length(knots)]) {
    knots <- c(knots,
               seq(knots[length(knots)] + IntervalLength,
                   knots[length(knots)] + 1000 * IntervalLength, IntervalLength))
  }
  
  # note that intercept is false, and knots are exactly as specified in "knots"
  Btemp.tk <-  bs(x.t, knots = knots[-c(1, length(knots))],  
                  Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.tk,2,sum) > 0)
  
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol   <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.tk            <- Btemp.tk[, startnonzerocol:endnonzerocol]
  alphax.k        <- knots[startnonzerocol:endnonzerocol]
  # change:
  # add up the columns of Btemp2.tk for all k with alphaxtemp2.k > x0
  lastk  <- which(abs(alphax.k - x0) == min(abs(alphax.k - x0)))#which(round(alphax.k,3)==x0)
  Ktemp2 <- dim(B.tk)[2]
  if (lastk < Ktemp2) { # need to combine columns
    B.tk     <- cbind(B.tk[, 1:(lastk - 1)], apply(B.tk[, (lastk:Ktemp2)], 1, sum))
    alphax.k <- alphax.k[1:lastk]
  }
  # end change
  ux.q <- alphax.k[-c(1, length(alphax.k))]
  # return arguments:
  return(list(B.tk = B.tk, # matrix with one row for each element of x.t, and each column is a spline
              alphax.k = alphax.k, # the knots (x values)
              ux.q = ux.q # same as alphax.k, but excluding the first and last knot
  ))
  
}#end of GetSplines_Constantafterx0 function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetSplines_ConstantBeforexstartAndAfterx0 <- function(
  x.t, # x (without NAs) for which splines need to be calculated (determines the number of rows of B.tk)
  x0 , 
  
  # determines knot placement 
  # no longer allowed to be null: no new splines added after (splines closest to) xo!
  xstart, # no splines before splines (splines closest to) xstart
  IntervalLength = 2.5 # interval length between two knots
){
  if (sum(is.na(x.t) != 0)) {
    print("Stop: no missing values allowed for x.t")
    return()
  }
  # if (is.null(x0)){ # default setting: knot is placed half-interval before last observation
  #   x0 = max(x.t)-0.5*IntervalLength
  # } 
  # get knots, given that one knot needs to be in x0
  # do NOT make any assumptions about where x0 is compared to x.t!  
  knots <- seq(x0 - 1000 * IntervalLength, x0 + 1000 * IntervalLength,
               IntervalLength)
  while (min(x.t) < knots[1]) {
    knots <- c(seq(knots[1] - 1000 * IntervalLength, knots[1] - IntervalLength,
                   IntervalLength), knots)
  }
  while (max(x.t) > knots[length(knots)]) {
    knots <- c(knots,
               seq(knots[length(knots)] + IntervalLength,
                   knots[length(knots)] + 1000 * IntervalLength, IntervalLength))
  }
  
  # note that intercept is false, and knots are exactly as specified in "knots"
  Btemp.tk <-  bs(x.t, knots = knots[-c(1, length(knots))],  
                  Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.tk, 2, sum) > 0)
  
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol   <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.tk            <- Btemp.tk[, startnonzerocol:endnonzerocol]
  alphax.k        <- knots[startnonzerocol:endnonzerocol]
  # change:
  # add up the columns of B.tk for all k with alphaxtemp2.k > x0
  lastk <- which(abs(alphax.k - x0) == min(abs(alphax.k - x0)))#which(round(alphax.k,3)==x0)
  Ktemp2 <- dim(B.tk)[2]
  if (lastk < Ktemp2) { # need to combine columns
    B.tk     <- cbind(B.tk[, 1:(lastk - 1)], apply(B.tk[, (lastk:Ktemp2)], 1, sum))
    alphax.k <- alphax.k[1:lastk]
  }
  # add up columns for all k with alphax.k <xtstart
  firstk <- which(abs(alphax.k - xstart) == min(abs(alphax.k - xstart)))#which(round(alphax.k,3)==x0)
  Ktemp2 <- dim(B.tk)[2]
  if (firstk > 1) { # need to combine columns
    B.tk     <- cbind(apply(B.tk[, 1:(firstk)], 1, sum), B.tk[, (firstk + 1):Ktemp2])
    alphax.k <- alphax.k[firstk:Ktemp2]
  }
  
  ux.q <- alphax.k[-c(1, length(alphax.k))]
  
  # return arguments:
  return(list(B.tk = B.tk, # matrix with one row for each element of x.t, and each column is a spline
              alphax.k = alphax.k, # the knots (x values)
              ux.q = ux.q # same as alphax.k, but excluding the first and last knot
  ))
  
}#end of GetSplines_ConstantBeforexstartAndAfterx0 function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetSplinesResults <- function (
  Qsplines.input = Q1.ct,
  Qobserve.input = Q1.i,
  QlowerBound = Q4LowerBound,
  diff = d, # difference penalty setting
  cutoff = NULL, # construct finer grid for splines (mainly for illustration)
  plot = FALSE # whether to display illustration plot
) {
  ## get all splines-related results (with display plot) ##
  
  Qtemp <- c(Qsplines.input)
  
  if (is.null(cutoff)) {
    logQ.k <- 
      log(1 / Qunit * 
            seq(floor(Qunit * max(QlowerBound, min(Qtemp, na.rm = TRUE))),
                ceiling(Qunit * max(Qtemp, na.rm = TRUE))))
  } else {
    logQ.k <- log(1 / Qunit * c(
      seq(floor(Qunit * max(QlowerBound, min(Qtemp, na.rm = TRUE))), cutoff, 0.1),
      seq(cutoff + 1, ceiling(Qunit * max(Qtemp, na.rm = TRUE)))))
  }#end of ifelse(is.null(cutoff))
  
  k <- length(logQ.k)
  
  res.splines <- GetSplines_ConstantBeforexstartAndAfterx0(
    x.t = logQ.k,
    xstart = log(Qstartsplines),
    x0 = round(quantile(log(Qobserve.input), 
                        probs = Qpercentilesplinesend, na.rm = TRUE), 3),  
    IntervalLength = IntervalLength)
  
  K.knot <- length(res.splines$alphax.k) # number of knots
  B.tk   <- res.splines$B.tk
  D2     <- diff(diag(K.knot), diff = diff)
  G      <- cbind(rep(1, K.knot), seq(1, K.knot) - K.knot / 2)
  Dcomb  <- t(D2) %*% solve(D2 %*% t(D2))
  Z.tk   <- B.tk %*% Dcomb
  Q      <- dim(Z.tk)[2]
  BG.tm  <- B.tk %*% G
  
  if (plot) {
    colchart <- c(
      "black", "darkorange1", "seagreen3", "royalblue", "indianred2",
      "yellowgreen", "springgreen4", "purple", "lightgoldenrod4",
      "brown1", "maroon", "lightgoldenrod4")
    
    plot(B.tk[, 1] ~ logQ.k, type = "n",
         xlab = paste0(ageGroupName.a[ages.a == age], "*1,000 (log-scale)"), 
         ylim = c(0, 1), ylab = "Splines", xlim = range(logQ.k, na.rm = TRUE), xaxt = "n")
    axis(1, at = labellogu, label = labelu, las = 1)
    # plot knot position
    abline(v = res.splines$alphax.k, col = "grey")
    for (knot in 1:K.knot) {
      lines(B.tk[, knot] ~ logQ.k, type = "l", col = colchart[knot], lwd = 8)
    }#end of knot loop
    
  }#end of if (plot)
  
  return(list(logQ.k = logQ.k, k = k, BG.tm = BG.tm, Z.tk = Z.tk, Q = Q))
  
}#end of GetSplinesResults function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

Rhat1 <- function(mat) {
  ## compute R hat for mcmc.array to check convergence ##
  m <- ncol(mat)
  n <- nrow(mat)
  b <- apply(mat, 2, mean)
  B <- sum((b - mean(mat))^2) * n / (m - 1)
  w <- apply(mat, 2, var)
  W <- mean(w)
  
  s2hat <- (n - 1) / n * W + B / n
  Vhat  <- s2hat + B / m / n 
  covWB <- n / m * (cov(w, b^2) - 2 * mean(b) * cov(w, b))
  varV  <- (n - 1)^2 / n^2 * var(w) / m +
    (m + 1)^2 / m^2 / n^2 * 2 * B^2 / (m - 1) +
    2 * (m - 1) * (n - 1) / m / n^2 * covWB
  
  df <- 2 * Vhat^2 / varV
  R  <- sqrt((df + 3) * Vhat / (df + 1) / W)
  
  return(R)
  
}#end of Rhat1 function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


Rhat <- function(arr) {
  dm <- dim(arr)
  
  if (length(dm) == 2) return(Rhat1(arr))
  if (dm[2] == 1) return(NULL)
  if (dm[3] == 1) return(Rhat1(arr[, , 1]))
  
  return(apply(arr, 3, Rhat1))
  
}#end of Rhat function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

getPostInfo <- function(
  mcmc.array,
  percentile.p = percentiles) {
  
  ## compute R hat and median & CI for post for all para ##
  Rhat      <- Rhat(mcmc.array)
  Per       <- length(percentile.p)
  POST.INFO <- matrix(NA, nr = dim(mcmc.array)[3], nc = Per)
  for (i in 1:dim(mcmc.array)[3]) {
    post <- c(mcmc.array[, , i])
    POST.INFO[i, ] <- quantile(post, probs = percentile.p)
  }
  
  post.full <- cbind(POST.INFO, Rhat)
  dimnames(post.full)[[2]] <- c("lower", "median", "upper", "Rhat")
  
  return(post.full)
  
}#end of getPostInfo function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ReadJagsOutput <- function(
  n.steps, n.chains = 3, #n.thin=1,
  maxiter = 7500,  
  # note 1: may be slightly more because of rounding
  # note 2: if you get over maxiter, the thinning can be off at end and start of step
  # note 3: still better than thinning after reading in all the steps because array becomes too large
  ChainNums = NULL,  #note: ChainNums overwrites n.chains if provided
  start.step = 1, #by default start reading in the first step
  runname, output.dir = "output/"){
  ## read in JAGS output ##
  
  if (is.null(ChainNums)) {
    ChainNums <- seq(1, n.chains)
  } else {
    n.chains <- length(ChainNums)
  }
  chain <- ifelse(length(ChainNums) == 1, 1, ChainNums[1])
  
  load(paste0(output.dir, "temp.JAGSobjects/jags_mod", runname,
              chain, "update_", start.step, ".Rdata"))
  n.iter.perstep <- dim(mod.upd$BUGSoutput$sims.array)[1]
  #   ntotaltemp <- n.iter.perstep*(n.steps-start.step+1)*n.chains
  #   print(paste("A total of ", n.iter.perstep*(n.steps-start.step+1)*n.chains, "samples were obtained."))
  #   ntotal <- min(maxiter, ntotaltemp)
  #   print(paste("A total of ", ntotal, "samples are saved."))
  nsavepersteptemp <- min(n.iter.perstep,
                          ceiling(maxiter * 1 / (n.steps - start.step + 1) * 1 / n.chains))
  N.THIN         <- floor(n.iter.perstep / nsavepersteptemp)
  iter.pick      <- seq(1, n.iter.perstep, N.THIN)
  n.iter.perstep <- length(iter.pick)
  n.sim          <- (n.steps - start.step + 1) * n.iter.perstep
  n.par          <- dim(mod.upd$BUGSoutput$sims.array)[3]
  print(paste("Additional thinning of:", N.THIN ))
  print(paste("A total of", n.sim * n.chains, "samples will be saved."))
  
  mcmc.array <- array(NA, c(n.sim, n.chains, n.par))
  dimnames(mcmc.array) <- list(NULL, NULL, names(mod.upd$BUGSoutput$sims.array[1, 1, ]))
  for (chain in 1:n.chains) {
    chain_saved <- ifelse(length(ChainNums) == 1, 1, ChainNums[chain])
    cat(paste0("Reading in chain number ", chain_saved, " (step ", start.step, ")"), "\n")
    load(paste0(output.dir, "temp.JAGSobjects/jags_mod", runname,
                chain_saved, "update_", start.step, ".Rdata"))
    mcmc.array[1:n.iter.perstep,chain, ] <- mod.upd$BUGSoutput$sims.array[iter.pick, 1, ]
    if (n.steps > 1) {
      for (step in (start.step + 1):n.steps) {
        cat(paste0("Reading in chain number ", chain_saved, " (step ", step, ")"), "\n")
        load(paste0(output.dir, "temp.JAGSobjects/jags_mod", runname,
                    chain_saved, "update_", step, ".Rdata"))
        mcmc.array[((step - start.step) * n.iter.perstep + 1):((step - start.step + 1) * n.iter.perstep),
                   chain, ] <- mod.upd$BUGSoutput$sims.array[iter.pick, 1, ]
      }#end of step loop
    }#end of if (n.steps > 1)
  }#end of chain loop
  
  #   n.sample.max = 10000000
  #   if (n.sim > n.sample.max){
  #     mcmc.array <- mcmc.array[seq(1, n.sample.max, length.out = n.sample.max), , ]  
  #   }
  
  return(mcmc.array)
  
}#end of ReadJagsOutput function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

InternalMakeCountryNamesShort <- function(name.c) {
  
  ## Shorten country names (and make consistent) ##
  
  name.c <- ifelse(name.c == "Cote d'Ivoire", #"Cote d Ivoire"; "C\364te d'Ivoire"
                  paste("Cote d'Ivoire"), paste(name.c))
  name.c <- ifelse(name.c == "Sao Tome and Principe" | name.c == "Sao Tome & Principe",
                   paste("Sao Tome Pr"), paste(name.c))
  name.c <- ifelse(name.c == "Vietnam", paste("Viet Nam"), paste(name.c))
  name.c <- ifelse(name.c == "Gambia The", paste("Gambia"), paste(name.c))
  name.c <- ifelse(name.c == "Brunei Darussalam", paste("Brunei"), paste(name.c))
  name.c <- ifelse(name.c == "Saint Kitts and Nevis",
                   paste("Saint Kitts & Nevis"), paste(name.c))
  name.c <- ifelse(name.c == "Timor Leste", paste("Timor-Leste"), paste(name.c))
  name.c <- ifelse(name.c == "Dominican Rep.", paste("Dominican Republic"), paste(name.c))
  name.c <- ifelse(name.c == "Syrian Arab Republic", paste("Syria"), paste(name.c))
  name.c <- ifelse(name.c == "GuineaBissau", paste("Guinea-Bissau"), paste(name.c))
  name.c <- ifelse(name.c == "Libyan Arab Jamahiriya", paste("Libya"), paste(name.c))
  name.c <- ifelse(name.c == "Ukraine ", paste("Ukraine"), paste(name.c))
  name.c <- ifelse(name.c == "Republic of Moldova" | name.c == "Moldova, Rep. of",
                   paste("Moldova"), paste(name.c))
  name.c <- ifelse(name.c == "Federated States of Micronesia" |
                     name.c == "Micronesia (Federated States of )" |
                     name.c == "Micronesia, Federated States of" |
                     name.c == "Micronesia, Fed. States of" |
                     name.c == "Micronesia (Fed. States of)",
                   paste("Micronesia"), paste(name.c))
  name.c <- ifelse(name.c == "United Kingdom", paste("U.K."), paste(name.c))
  name.c <- ifelse(name.c == "United States of America" | name.c == "United States",
                   paste("U.S."), paste(name.c))
  name.c <- ifelse(name.c == "Congo, Dem. Rep." |
                     name.c=="Democratic Republic of the Congo" | name.c=="Congo DR",
                  paste("DRC"), paste(name.c))
  name.c <- ifelse(name.c == "The former Yugoslav Republic of Macedonia" |
                     name.c == "TFYR Macedonia", paste("Macedonia"), paste(name.c))
  name.c <- ifelse(name.c == "Bosnia and Herzegovina" |
                     name.c == "Bosnia & Herzegovina",
                   paste("Bosn&Herze"), paste(name.c))
  name.c <- ifelse(name.c == "Trinidad and Tobago" | name.c == "Trinidad & Tobago",
                   paste("Trinidad&T"), paste(name.c))
  name.c <- ifelse(name.c == "China, Hong Kong SAR", paste("Hong Kong"), paste(name.c))
  name.c <- ifelse(name.c == "United Repulic of Tanzania",
                   paste("Tanzania"), paste(name.c))
  name.c <- ifelse(name.c == "United States Virgin Islands",
                   paste("US Virgin Isl."), paste(name.c))
  name.c <- ifelse(name.c == "United Arab Emirates", paste("Arab Emirates"), paste(name.c))
  name.c <- ifelse(name.c == "Lao People's Democratic Republic" |
                     name.c == "Lao People's Dem. Rep." |
                     name.c == "Lao PDR", paste("Laos"), paste(name.c))
  name.c <- ifelse(name.c == "Republic of Korea" | name.c == "Republic of Korea " |
                     name.c == "Korea Rep" | name.c == "Korea, Rep. of",
                   paste("South Korea"), paste(name.c))
  name.c <- ifelse(name.c == "Democratic People's Republic of Korea"|name.c=="Korea DPR"|
                    name.c == "Dem. People's Republic of Korea"|name.c=="Korea, Dem. People's Rep.",
                  paste("North Korea"), paste(name.c))
  name.c <- ifelse(name.c == "Central African Republic" | name.c == "Central African Rep.",
                   paste("CAR"), paste(name.c))
  name.c <- ifelse(name.c == "Iran (Islamic Republic of)"|
                     name.c == "Iran, Islamic Republic of",
                  paste("Iran"), paste(name.c))
  name.c <- ifelse(name.c == "United Republic of Tanzania" |
                     name.c=="Tanzania, United Republic of",
                  paste("Tanzania"), paste(name.c))
  name.c <- ifelse(name.c == "Venezuela (Bolivarian Republic of)", paste("Venezuela"), paste(name.c))
  name.c <- ifelse(name.c == "Bolivia (Plurinational State of)", paste("Bolivia"), paste(name.c))
  name.c <- ifelse(name.c == "Antigua and Barbuda" | name.c == "Antigua & Barbuda",
                   paste("Antigua and B."), paste(name.c))
  name.c <- ifelse(name.c == "Northern Mariana Islands", paste("N. Mariana Isl."), paste(name.c))
  name.c <- ifelse(name.c == "Occupied Palestinian Territory" | name.c == "OPT", 
                  #paste("Occ. Palestinian Terr."), 
                  paste("OPT"), paste(name.c))
  name.c <- ifelse(name.c == "Saint Vincent and the Grenadines" |
                     name.c == "Saint Vincent & the Grenadines" |
                     name.c == "Saint Vincent/Grenadines" |
                     name.c == "St Vincent & the Grenadines",
                   paste("St. Vincent & Gren."), paste(name.c))
  ##value<< Vector with length of \code{name.c}, but some names replaced
  
  return(name.c)
  
}#end of InternalMakeCountryNamesShort function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ExternalMakeCountryNamesFull <- function(name.c) {
  
  ## Full form of country names (and make consistent) ##
  
  name.c <- ifelse(name.c == "Antigua & Barbuda", paste("Antigua and Barbuda"), paste(name.c))
  name.c <- ifelse(name.c == "Bolivia", paste("Bolivia (Plurinational State of)"), paste(name.c))
  name.c <- ifelse(name.c == "Bosnia & Herzegovina", paste("Bosnia and Herzegovina"), paste(name.c))
  name.c <- ifelse(name.c == "Cote d Ivoire", paste("Cote d'Ivoire"), paste(name.c))
  name.c <- ifelse(name.c == "Gambia The", paste("The Gambia"), paste(name.c))
  name.c <- ifelse(name.c == "Congo DR", paste("Democratic Republic of the Congo"), paste(name.c))
  name.c <- ifelse(name.c == "Iran", paste("Iran (Islamic Republic of)"), paste(name.c))
  name.c <- ifelse(name.c == "Korea DPR", paste("Democratic People's Republic of Korea"), paste(name.c))
  name.c <- ifelse(name.c == "Korea Rep", paste("Republic of Korea"), paste(name.c))
  name.c <- ifelse(name.c == "Lao PDR", paste("Lao People's Democratic Republic"), paste(name.c))
  name.c <- ifelse(name.c == "Moldova", paste("Republic of Moldova"), paste(name.c))
  name.c <- ifelse(name.c == "Saint Kitts & Nevis", paste("Saint Kitts and Nevis"), paste(name.c))
  name.c <- ifelse(name.c == "Sao Tome & Principe", paste("Sao Tome and Principe"), paste(name.c))
  name.c <- ifelse(name.c == "St Vincent & the Grenadines", paste("Saint Vincent and the Grenadines"), paste(name.c))
  name.c <- ifelse(name.c == "Trinidad & Tobago", paste("Trinidad and Tobago"), paste(name.c))
  name.c <- ifelse(name.c == "Venezuela", paste("Venezuela (Bolivarian Republic of)"), paste(name.c))
  
  return(name.c)
  
}#end of ExternalMakeCountryNamesFull function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# mleShapeRateforGamma<-function(sigma){
#   require(maxLik)
#   require(MCMCpack)
#   ## construct gamma parameter as prior for AR run ##
#   
#   ## fitting is: sigma^2 ~ Inverse Gamma (shape=shape,scale=rate)
#   ## i.e. tau ~ Gamma (shape=nu,rate=rate)
#   loglikelihood<-function(par){shape<-par[1];rate<-par[2];
#                                sum(log(dinvgamma(sigma^2,shape=shape,scale=rate)))}
#   A=diag(2)
#   B=-0.00001
#   param1<-maxLik(logLik=loglikelihood,start=c(shape=0.0001,rate=0.0001),
#                  constraints=list(ineqA=A,ineqB=B))
#   shape<-coef(param1)["shape"]
#   rate<-coef(param1)["rate"]
#   return(c(shape,rate))
# }


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# From ContraceptiveUse - version 1
InternalGetARTrajectories <- function( # Construct AR(1) trajectories
  ### Construct posterior sample of AR(1) trajectories, given samples at (unequally spaced) time points
  rho.s, ##<< Posterior sample of autoregressive parameter
  sigma.s, ##<< Posterior sample of autoregressive sd
  eps.is, ##<< Posterior sample of AR(1) for obs years i=1,..., I 
  years.i, ##<< Obs years i=1,..., I
  start.year, ##<< First year where posterior sample is needed
  end.year##<< Last year where posterior sample is needed
){
  ###  Note: years.i and start/end year are centered at midpoint calendar year
  
  
  nsample <- length(rho.s)
  # Note: some obs years could be before start year or after end year
  # Don't throw them out because they inform the eps in start/end year
  # Inefficient but simple coding: 
  # first construct eps from min(years.i,start year) to max(years.i, end year)
  # then select the years we need
  start.year.temp <- min(years.i, start.year)
  end.year.temp   <- max(years.i, end.year)
  nyears          <-  end.year.temp - start.year.temp + 1
  #eps.st <- matrix(NA, nsample, nyears)
  obs.years.indices <- years.i - start.year.temp + 1
  n                 <- length(obs.years.indices)
  
  nrepeatARsampling <- 1 # no longer used
  # create nrepeatARsampling trajectories for each posterior sample
  eps.str <- array(NA, c(nsample, nyears, nrepeatARsampling))
  
  
  for (i in 1:n) {
    for (r in 1:nrepeatARsampling) {
      eps.str[, obs.years.indices[i], r] <- eps.is[i, ]
    }#end of r loop
  }#end of i loop
  
  # SAMPLING STARTS
  # create nrepeatARsampling trajectories for each posterior sample
  # speed all this up!?
  
  # add samples at start
  if (obs.years.indices[1] > 1) {
    for (k in seq(obs.years.indices[1] - 1, 1, -1)) {
      for (r in 1:nrepeatARsampling) {
        eps.str[, k, r] <- rnorm(nsample, rho.s * eps.str[, k + 1, r], sigma.s) 
      }#end of r loop
    }#end of k loop
  }#end of if (obs.years.indices[1] > 1)
  
  # add samples at end
  if (obs.years.indices[n] < nyears) {
    for (k in (obs.years.indices[n] + 1):nyears) {
      for (r in 1:nrepeatARsampling) {
        eps.str[, k, r] <- rnorm(nsample, rho.s * eps.str[, k - 1, r], sigma.s) 
      }#end of r loop
    }#end of k loop
  }#end of if (obs.years.indices[n] < nyears)
  
  if (n > 1) { # is there more than 1 obs?
    for (j in 1:(n - 1)) {
      if ((obs.years.indices[j + 1] - obs.years.indices[j]) > 1) {    
        # are there are eps's missing between the two observed eps's?
        for (t in (obs.years.indices[j] + 1):(obs.years.indices[j + 1] - 1)) {
          A <- rho.s^(2 * (obs.years.indices[j + 1] - t + 1))
          varZ.s <- sigma.s^2 / (1 - rho.s^2) * (
            1 - 1 / (1 - A) * (rho.s^2 - 2 * A + rho.s^(2 * (obs.years.indices[j + 1] - t)))
          )
          
          for (r in 1:nrepeatARsampling) {
            Zhat.s <- 1 / (1 - A) * (
              eps.str[, t - 1, r] * rho.s * (1 - rho.s^(2 * (obs.years.indices[j + 1] - t))) +
                eps.str[, obs.years.indices[j + 1], r] * rho.s^(obs.years.indices[j + 1] - t) *
                (1 - rho.s^2))
            eps.str[, t, r] <- rnorm(nsample, Zhat.s, sd = sqrt(varZ.s))
          }#end of r loop
        }#end of t loop
      }#end of if ((obs.years.indices[j + 1] - obs.years.indices[j]) > 1)
    }#end of j loop
  }#end of if (n > 1)
  
  # select the corresponding years
  # note that 3rd dimension drops out if it's 1
  #A <- array(seq(1,12), c(3,4,1))
  #A
  #array(A[,-1,], dim(A[,-1,]))
  #array(A[,-1,], c(dim(A[,-1,]),1))
  
  select <- is.element(seq(start.year.temp, end.year.temp), seq(start.year, end.year))
  if (nrepeatARsampling != 1) {
    eps.str.final <- eps.str[, select, ]
  } else {
    eps.str.final <- array(eps.str[, select, ], c(dim(eps.str[, select, ]), 1))
  }
  #print(dim(eps.str.final))
  # note: the 3rd dimension is still lost when repeat=1
  
  return(eps.str.final[, , 1])
  
  ##value<<
  ## matrix of size $s$ times $t$ for years (start.year:end.year)
  
}#end of InternalGetARTrajectories function

# eps <- InternalGetARTrajectories(
#   rho.s = c(0.5,0.5),
#   sigma.s= c(0.5,0.5),
#   eps.is = matrix(c(1,1), 1,2),
#   years.i = 2,
#   start.year = 0,
#   end.year = 3,
#   nrepeatARsampling=1)
# eps[1,1,1]

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

StandardizeSignifDigit <- function (
  number.in, # input number that you want to standardize
  number.ref # standardize the number.in to have the same number of significant
             # digits as number.ref.
  ) {
  ## make number.in to have the same number of significant digits as number.ref
  for (power in c(5:1)) {
    if (number.ref %% 10^power == 0) {
      break
    } else {
      power <- 0
    }
  }#end of power loop
  
  number.out <- round(number.in / 10^power) * 10^power
  
  return(number.out)
  
}#end of StandardizeSignifDigit function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

StandardizeDecimal <- function(
  number.in,
  n.round) {
  ## make number.in to round to n.round. If resulting number has decimal places
  # smaller than rounded number, add 0 behind. E.g. round(1.999, 2) to be 2.00
  # instead of 2
  number.out <- format(as.numeric(round(number.in, n.round)), nsmall = n.round)
  
  return(number.out)
}#end of StandardizeDecimal()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------




###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_writeJAGSmodel.R
# 
# This script writes out JAGS model file.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: jags_setupMCMC.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: most information are from source_BasicSetup and jags_setupMCMC.R.
#
# output data: data/output/runname/runname.txt
#
###############################################################################

# JAGS may not work well on sum with many elements. So we split into several
# sums with fewer element within each sum.
COUNT.1 <- floor(Q.1 / NoTermPerLine) + 
  ifelse(Q.1 / NoTermPerLine == round(Q.1 / NoTermPerLine), 0, 1)
COUNT.4 <- floor(Q.4 / NoTermPerLine) + 
  ifelse(Q.4 / NoTermPerLine == round(Q.4 / NoTermPerLine), 0, 1)

# note: spaces in cat matter!
cat("
model {
    for (k in 1:k1) {
      logf1.k[k] <- (mge1_pred.k[k]", paste0(" + mre1_pred", seq(1, COUNT.1), ".k[k]"), ")",
    sep    = "",
    append = FALSE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
)
cat("
      mge1_pred.k[k] <- (b1.k[1] * BG.tm.1[k, 1] + b1.k[2] * BG.tm.1[k, 2])",
    sep    = "",
    append = TRUE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
)

if(COUNT.1 > 1) {
  for(count in 1:(COUNT.1 - 1)) {
    cat("
      mre1_pred", count, ".k[k] <- (0",
        paste0(" + u1.q[", seq((count - 1) * NoTermPerLine + 1, count * NoTermPerLine), "]", 
               " * Z.tk.1[k, ", seq((count - 1) * NoTermPerLine + 1, count * NoTermPerLine), "]"), ")",
        sep    = "",
        append = TRUE,
        file   = file.path(output.dir, JAGSmodel.name),
        fill   = FALSE
        )
  }#end of count loop
}#end of if COUNT.1>1

cat("
      mre1_pred", COUNT.1, ".k[k] <- (0",
    paste0(" + u1.q[", seq((COUNT.1 - 1) * NoTermPerLine + 1, Q.1), "]", 
           " * Z.tk.1[k, ", seq((COUNT.1 - 1) * NoTermPerLine + 1, Q.1), "]"), ")",
    "\n", "    }#end of k loop",
    sep    = "",
    append = TRUE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
    )

cat("
    for (k in 1:k4) {
      logf4.k[k] <- (mge4_pred.k[k]", paste0(" + mre4_pred", seq(1, COUNT.4), ".k[k]"), ")",
    sep    = "",
    append = TRUE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
)
cat("
      mge4_pred.k[k] <- (b4.k[1] * BG.tm.4[k, 1] + b4.k[2] * BG.tm.4[k, 2])",
    sep    = "",
    append = TRUE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
)

if(COUNT.4 > 1) { 
  for (count in 1:(COUNT.4 - 1)) {
    cat("
      mre4_pred", count, ".k[k] <- (0",
        paste0(" + u4.q[", seq((count - 1) * NoTermPerLine + 1, count * NoTermPerLine), "]", 
               " * Z.tk.4[k, ", seq((count - 1) * NoTermPerLine + 1, count * NoTermPerLine), "]"), ")",
        sep    = "",
        append = TRUE,
        file   = file.path(output.dir, JAGSmodel.name),
        fill   = FALSE
        )
  }#end of count loop 
}#end of if COUNT.4>1

cat("
      mre4_pred", COUNT.4, ".k[k] <- (0",
    paste0(" + u4.q[", seq((COUNT.4 - 1) * NoTermPerLine + 1, Q.4), "]", 
           " * Z.tk.4[k, ", seq((COUNT.4 - 1) * NoTermPerLine + 1, Q.4), "]"), ")",
    "\n", "    }#end of k loop",
    sep    = "",
    append = TRUE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
    )

cat("  
    ## splines parameters ##
    for (q in 1:Q.1) {
      u1.q[q] ~ dnorm(pri.u1.mean, tau1.u)
    }
    for (q in 1:Q.4) {
      u4.q[q] ~ dnorm(pri.u4.mean, tau4.u)
    }
    
    b1.k[1] ~ dunif(pri.b11.lower, pri.b11.upper)
    b4.k[1] ~ dunif(pri.b41.lower, pri.b41.upper)
    b1.k[2] ~ dunif(pri.b12.lower, pri.b12.upper)
    b4.k[2] ~ dunif(pri.b42.lower, pri.b42.upper)
    
    tau1.u  <- tau14.u 
    tau4.u  <- tau14.u 
    tau14.u <- pow(sigma.14, -2)
    sigma.14 ~ dunif(pri.sigma.14.lower, pri.sigma.14.upper)
    
    ## data model on log-scale ##
    for (h in 1:n1) {
      logs.i[geti.h1[h]] ~ dnorm(logs1hat.i[geti.h1[h]], tau1.i[geti.h1[h]])
    
      logs1hat.i[geti.h1[h]] <- (logf1.k[getk1.i[geti.h1[h]]]
      + logP1.ct[c.i[geti.h1[h]], t.i[geti.h1[h]]])
    
      tau1.i[geti.h1[h]] <- 1 / (pow(sigma1.s[source.i[geti.h1[h]]], 2) +
      pow(logSE.i[geti.h1[h]], 2))
    }#end of n1 loop
    
    for (h in 1:n4) {
      logs.i[geti.h4[h]] ~ dnorm(logs4hat.i[geti.h4[h]], tau4.i[geti.h4[h]])
    
      logs4hat.i[geti.h4[h]] <- (logf4.k[getk4.i[geti.h4[h]]]
      + logP4.ct[c.i[geti.h4[h]], t.i[geti.h4[h]]])
    
      tau4.i[geti.h4[h]] <- 1 / (pow(sigma4.s[source.i[geti.h4[h]]], 2) +
      pow(logSE.i[geti.h4[h]], 2))
    }#end of n4 loop
    
    for (h in 1:n5) {
      logs.i[geti.h5[h]] ~ dt(logs5hat.i[geti.h5[h]], tau5.i[geti.h5[h]], nu5)
    
      logs5hat.i[geti.h5[h]] <- log( 
      (1 - (1 - Qhat1M.i[geti.h5[h]]) * (1 - Qhat4M.i[geti.h5[h]])) /
      (1 - (1 - Qhat1M.i[geti.h5[h]] / s1hat.h[h]) *
      (1 - Qhat4M.i[geti.h5[h]] / s4hat.h[h])))
    
      s1hat.h[h] <- exp(logs1hat.h[h])
      s4hat.h[h] <- exp(logs4hat.h[h])
    
      logs1hat.h[h] <- (logf1.k[getk1.i[geti.h5[h]]]
      + logP1.ct[c.i[geti.h5[h]], t.i[geti.h5[h]]])
      logs4hat.h[h] <- (logf4.k[getk4.i[geti.h5[h]]]
      + logP4.ct[c.i[geti.h5[h]], t.i[geti.h5[h]]])
    
      Qhat1M.i[geti.h5[h]] <- Q1.i[geti.h5[h]] / (w1 + (1 - w1) / s1hat.h[h])
      Qhat4M.i[geti.h5[h]] <- (Q4.i[geti.h5[h]] /
      (w4.i[geti.h5[h]] + (1 - w4.i[geti.h5[h]]) / s4hat.h[h]))

      w4.i[geti.h5[h]] <- w1 * (1 - Qhat1M.i[geti.h5[h]]) / (1 - Q1.i[geti.h5[h]])
    
      tau5.i[geti.h5[h]] <- 1 / (pow(sigma5.s[source.i[geti.h5[h]]], 2) +
      pow(logSE.i[geti.h5[h]], 2))
    }#end of n5 loop
    
    for (c in 1:C) {# gett1.cz's are ordered!!! gett1.cz[c, 1] is first obs year
      logP1.ct[c, gett1.cz[c, 1]] ~ dnorm(a.c[c], tau.eps.stat)
      for (z in 2:nt1.c[c]) {
        logP1.ct[c, gett1.cz[c, z]] ~ dnorm(epshat1.cz[c, z], tau.eps1.cz[c, z])
    
        epshat1.cz[c, z] <- (a.c[c] 
        + pow(rho, abs(gett1.cz[c, z] - gett1.cz[c, z - 1]))
        * (logP1.ct[c, gett1.cz[c, z - 1]] - a.c[c]))
    
        tau.eps1.cz[c, z] <- (tau.eps.stat /
        (1 - pow(rho, 2 * abs(gett1.cz[c, z] - gett1.cz[c, z - 1]))))
      }#end of z loop
    
      logP4.ct[c, gett4.cz[c, 1]] ~ dnorm(b.c[c], tau.eps.stat)
      for (z in 2:nt4.c[c]) {
        logP4.ct[c, gett4.cz[c, z]] ~ dnorm(epshat4.cz[c, z], tau.eps4.cz[c, z])
    
        epshat4.cz[c, z] <- (b.c[c] 
        + pow(rho, abs(gett4.cz[c, z] - gett4.cz[c, z - 1]))
        * (logP4.ct[c, gett4.cz[c, z - 1]] - b.c[c]))
    
        tau.eps4.cz[c, z] <- (tau.eps.stat /
        (1 - pow(rho, 2 * abs(gett4.cz[c, z] - gett4.cz[c, z - 1]))))
      }#end of z loop
    
      a.c[c] ~ dt(pri.ac.mu, tau.a, pri.ac.v)T(, pri.ac.tranc.upper)
      b.c[c] ~ dt(pri.bc.mu, tau.b, pri.bc.v)T(, pri.bc.tranc.upper)
    }#end of c loop
    
    tau.eps.stat <- tau.eps * (1 - pow(rho, 2))
    tau.eps      <- pow(sigma.eps, -2)    
    sigma.eps     ~ dunif(pri.sigma.eps.lower, pri.sigma.eps.upper)
    rho           ~ dunif(pri.rho.lower, pri.rho.upper)
    
    sigma.a          <- 1 / sqrt(tau.a)
    sigma.b          <- 1 / sqrt(tau.b)    
    tau.a             ~ dgamma(pri.sigma.a.shape, pri.sigma.a.rate)
    tau.b             ~ dgamma(pri.sigma.b.shape, pri.sigma.b.rate)        
    pri.sigma.a.rate <- pri.sigma.a.shape * Sigma0[1, 1]
    pri.sigma.b.rate <- pri.sigma.b.shape * Sigma0[2, 2]
    
    for (s in 1:(S - 1)) { 
      sigma1.s[s] ~ dunif(pri.sigma1s.lower, pri.sigma1s.upper)
      sigma4.s[s] ~ dunif(pri.sigma4s.lower, pri.sigma4s.upper)
      sigma5.s[s] ~ dunif(pri.sigma5s.lower, pri.sigma5s.upper)
    }#end of s loop
    
    # for VR:
    sigma1.s[S] <- 0
    sigma4.s[S] <- 0
    sigma5.s[S] <- 0
    
    nu5 ~ dunif(pri.nu5.lower, pri.nu5.upper)

}",
     sep    = "",
     append = TRUE,
     file   = file.path(output.dir, JAGSmodel.name),
     fill   = TRUE
 )

## The End! ##

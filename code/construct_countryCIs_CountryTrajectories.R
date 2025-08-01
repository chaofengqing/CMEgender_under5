

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_countryCIs_CountryTrajectories.R
# 
# This script construct trajectories for each country seperately.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetS5fromQboth(2)
# GetQmale(3)
# GetQfemale(3)
# GetDeath(2)
# GetExpQf(2)
# GetQ5fromQ14(1)
# Note: for Lancet paper, GetExpQf() function is in an older version by fixing
# the gap of the expected Qfemale sequence:
# qf.f <- seq(qm/max.W, min(qm/min.W, 1), 0.00001).
# The current version is by assigning length of output expected Qfemale sequence
# based on values of Qmale. The reproducibility check shows that the two method
# are within MCMC error.
# 
# input data: data/output/M49/mcmc.array_M49.rda
#
# output data: data/output/M49/countryTrajectory/cis_M49_*_full.rda
# note: each country-specific trajectory is saved in a seperate file.
# 
###############################################################################


#first run
NODES <- seq(1, 15)
#check country result constructed for each node
for (node in NODES) {
  countries <- seq((node - 1) * floor(C / max(NODES)) + 1,
                   ifelse(node < max(NODES), node * floor(C / max(NODES)), C)) 
  print(countries)
}

# #check redo countries 
# NODES <- seq(1, 11)
# for (node in NODES) {
#   Cselect <- seq((node - 1) * 1 + 1, ifelse(node < max(NODES), node * 1, length(country.redo)))
#   countries <- country.redo[Cselect]
#   print(countries)
# }#end of node loop
# 


library(doMC)
registerDoMC()
foreach (node = NODES) %dopar% {
  
  #first run 
  countries <- seq((node - 1) * floor(C / max(NODES)) + 1, 
                   ifelse(node < max(NODES), node * floor(C / max(NODES)), C)) 
  
  #   #redo run
  #   Cselect <- seq((node-1)*1+1,ifelse(node < max(NODES), node*1, length(country.redo)))
  #   countries <- country.redo[Cselect]
  
  cutoff.yearposi <- which(years.t == startYearSex) #save 1990-2015 for sex-specific results
  
  
  for (c in countries){
    S1.lt <- S4.lt <- S5.lt <- 
      W1.lt <- W4.lt <- W5.lt <- 
      P1.lt <- P4.lt <- P5.lt <- matrix(NA, L, Tend)
    
    for (t in 1:Tend) {
      P1.lt[, t] <- exp(c(selectP[["logP1.ctl"]][c, t, ]))
      P4.lt[, t] <- exp(c(selectP[["logP4.ctl"]][c, t, ]))
      
      ## W1 ##
      pick.1 <- which(c(logQ1.k) == c(log(1 / Qunit * round(Qunit * Q1.ct[c, t]))))
      if (length(pick.1) == 1) {
        logW1.l <- c(mcmc.array[, , paste0("logf1.k[", pick.1, "]")])
      } else {
        logW1.l <- rep(NA, L)
      }
      W1.lt[, t] <- exp(logW1.l)
      
      ## W4 ##
      Q4.test <- Q4.ct[c, t]
      
      if (is.na(Q4.test)) {
        logW4.l <- rep(NA, L)
      } else {
        if (Q4.test >= Q4LowerBound) {
          pick.4 <- which(c(logQ4.k) == c(log(1 / Qunit * round(Qunit * Q4.ct[c, t])))) 
          if (length(pick.4) == 1) {
            logW4.l <- c(mcmc.array[, , paste0("logf4.k[", pick.4, "]")])
          } else {
            logW4.l <- rep(NA, L)
          }
        } else {
          pick.4 <- 1
          logW4.l <- c(mcmc.array[, , paste0("logf4.k[", pick.4, "]")])
        }
      }# end W4 stuff
      
      W4.lt[, t] <- exp(logW4.l)
      W5.lt[, t] <- GetS5fromQboth(q1both = Q1.ct[c, t], q4both = Q4.ct[c, t], 
                                   s1 = W1.lt[, t], s4 = W4.lt[, t])
    }# end t-loop
    
    S1.lt <- P1.lt * W1.lt
    S4.lt <- P4.lt * W4.lt
    for (t in 1:Tend) {
      S5.lt[, t] <- GetS5fromQboth(q1both = Q1.ct[c, t], q4both = Q4.ct[c, t], 
                                   s1 = S1.lt[, t], s4 = S4.lt[, t])    
    }#end of t loop
    
    P5.lt <- S5.lt / W5.lt
    
    ##########################
    ## get sex-specific Q's ##
    if (getQbysex) {
      q1.jt <- q4.jt <- q5.jt <-                #estimated mortality
        qhat1M.jt <- qhat4M.jt <- qhat5M.jt <-  #estimated male mortality
        qhat1F.jt <- qhat4F.jt <- qhat5F.jt <-  #estimated female mortality
        qhat1D.jt <- qhat4D.jt <- qhat5D.jt <-  #estimated mortality difference
        qexp1F.jt <- qexp4F.jt <- qexp5F.jt <-  #expected female mortality
        qexc1F.jt <- qexc4F.jt <- qexc5F.jt <-  #excess female mortality
        
        dhat1F.jt <- dhat4F.jt <- dhat5F.jt <-  #estimated female deaths
        dexp1F.jt <- dexp4F.jt <- dexp5F.jt <-  #expected female deaths
        dexc1F.jt <- dexc4F.jt <- dexc5F.jt <-  #excess female deaths
        matrix(NA, Lbysex, Tend)
      
      for (t in 1:Tend) {
        #######################################
        ## estimated male & female mortality ##
        q1.jt[, t] <- q1.j <- Q1.ctj[c, t, ]
        q4.jt[, t] <- q4.j <- Q4.ctj[c, t, ]
        q5.jt[, t] <- q5.j <- Q5.ctj[c, t, ]
        ## a=1 ##
        qhat1M.jt[, t] <- qhat1M.j <- GetQmale(qboth = q1.j, w = w1, s = S1.lt[selectl.j, t]) 
        qhat1F.jt[, t] <- qhat1F.j <- GetQfemale(qmale = qhat1M.jt[, t], s = S1.lt[selectl.j, t])
        ## a=4 ##
        w4.j <- GetWeight4(w1 = w1, q1male = qhat1M.j, q1both = q1.j)
        qhat4M.jt[, t] <- qhat4M.j <- GetQmale(qboth = q4.j, w = w4.j, s = S4.lt[selectl.j, t]) 
        qhat4F.jt[, t] <- qhat4F.j <- GetQfemale(qmale = qhat4M.j, s = S4.lt[selectl.j, t])
        ## a=5 ##
        qhat5M.jt[, t] <- qhat5M.j <- GetQmale(qboth = q5.j, w = w1, s = S5.lt[selectl.j, t])
        qhat5F.jt[, t] <- qhat5F.j <- GetQfemale(qmale = qhat5M.j, s = S5.lt[selectl.j, t]) 
        
        #############################
        ## estimated female deaths ##
        deathFhat <- GetDeath(q1 = qhat1F.j, q4 = qhat4F.j, q5 = qhat5F.j,
                              a1 = a1.c[c], a4 = a4.c[c],
                              pop1 = pop1F.ct[c, t], pop4 = pop4F.ct[c, t])
        dhat1F.jt[, t] <- deathFhat[["death1"]]
        dhat4F.jt[, t] <- deathFhat[["death4"]]
        dhat5F.jt[, t] <- deathFhat[["death5"]]        
        
        if (t >= cutoff.yearposi) {
          
          for (j in 1:Lbysex) {
            
            ###############################
            ## expected female mortality ##
            qexp1F <- qexp4F <- qexp5F <- NA
            
            ## a=1 ##
            if (!is.na(qhat1M.j[j])) {
              qexp1F.jt[j, t] <- qexp1F <- GetExpQf(qm = qhat1M.j[j], wa = w1, 
                                                    W.k = W1.lk[selectl.j[j], ], 
                                                    gridQtot.k = exp(logQ1.k))
              
            }#end of if (a=1)
            
            ## a=4 ##
            if (!is.na(qhat1M.j[j]) & !is.na(qhat4M.j[j]) & 
                  qhat4M.j[j] >= 0 & qhat4M.j[j] <= 1) {
              qexp4F.jt[j, t] <- qexp4F <- GetExpQf(qm = qhat4M.j[j], wa = w4.j[j], 
                                                    W.k = W4.lk[selectl.j[j], ], 
                                                    gridQtot.k = exp(logQ4.k))
              
            }#end of if (a=4)
            
            ## a=5 ##    
            qexp5F.jt[j, t] <- qexp5F <- GetQ5fromQ14(q1 = qexp1F.jt[j, t], q4 = qexp4F.jt[j, t])
            
            #############################
            ## expected female deaths ##
            deathFexp <- GetDeath(q1 = qexp1F, q4 = qexp4F, q5 = qexp5F,
                                  a1 = a1.c[c], a4 = a4.c[c],
                                  pop1 = pop1F.ct[c, t], pop4 = pop4F.ct[c, t])
            dexp1F.jt[j, t] <- deathFexp[["death1"]]
            dexp4F.jt[j, t] <- deathFexp[["death4"]]
            dexp5F.jt[j, t] <- deathFexp[["death5"]]        
            
          }#end of j loop            
          
        }#end of if(t >= cutoff.yearposi)
        
      }#end of t loop
      
      for (age in ages.a) {
        ## estimated mortality difference: male - female ##
        eval(parse(text = paste0("qhat", age, "D.jt <- qhat", age, "M.jt - qhat", age, "F.jt")))
        ## excess female mortality: estimated - expected ##
        eval(parse(text = paste0("qexc", age, "F.jt <- qhat", age, "F.jt - qexp", age, "F.jt")))
        ## excess female deaths: estimated - expected ##
        eval(parse(text = paste0("dexc", age, "F.jt <- dhat", age, "F.jt - dexp", age, "F.jt")))        
      }#end of age loop
      
    }#end if (getQbysex)   
    
    ## save country specific results ##
    res.c.both <- list(iso.c = iso.c[c],
                       S1.lt = S1.lt, S4.lt = S4.lt, S5.lt = S5.lt,
                       W1.lt = W1.lt, W4.lt = W4.lt, W5.lt = W5.lt,
                       P1.lt = P1.lt, P4.lt = P4.lt, P5.lt = P5.lt)
    if (getQbysex) {
      res.c.bySex <- list(    
        #estimated mortality
        Q1.jt = q1.jt, Q4.jt = q4.jt, Q5.jt = q5.jt,
        #estimated female mortality
        Q1f.jt = qhat1F.jt, Q4f.jt = qhat4F.jt, Q5f.jt = qhat5F.jt,
        #estimated male mortality
        Q1m.jt = qhat1M.jt, Q4m.jt = qhat4M.jt, Q5m.jt = qhat5M.jt,
        #estimated mortality difference (male-female)
        Q1d.jt = qhat1D.jt, Q4d.jt = qhat4D.jt, Q5d.jt = qhat5D.jt,
        
        #expected female mortality
        expQ1f.jt = qexp1F.jt, expQ4f.jt = qexp4F.jt, expQ5f.jt = qexp5F.jt,
        #excess female mortality (estimated-expected)
        excQ1f.jt = qexc1F.jt, excQ4f.jt = qexc4F.jt, excQ5f.jt = qexc5F.jt,
        
        #estimated female deaths
        D1f.jt = dhat1F.jt, D4f.jt = dhat4F.jt, D5f.jt = dhat5F.jt,
        #expected female deaths
        expD1f.jt = dexp1F.jt, expD4f.jt = dexp4F.jt, expD5f.jt = dexp5F.jt,
        #excess female deaths (estimated-expected)
        excD1f.jt = dexc1F.jt, excD4f.jt = dexc4F.jt, excD5f.jt = dexc5F.jt                
      )
    } else {
      res.c.bySex <- NULL
    }#end of if (getQbysex)
    
    res.c.full <- c(res.c.both, res.c.bySex) #results for [1950, 2015]
    save(res.c.full, file = paste0(countryTraj.dir, "cis_", runname, "_", iso.c[c], "_full.rda"))
    
  }#end of c loop    
}#end of node loop  

# #check which country result is not constructed
# country.redo <- rep(NA, C)
# for (c in 1:C) {
#   country.redo[c] <- file.access(
#     names = paste0(countryTraj.dir, "cis_", runname, "_", iso.c[c], "_full.rda"), 
#     mode = 0)
# }#end of c loop
# 
# country.redo <- which(country.redo == -1)

## the end! ##

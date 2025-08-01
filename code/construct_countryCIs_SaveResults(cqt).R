

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_countryCIs_SaveResults(cqt).R
# 
# This script saves country, region, and world results in order to get all the
# plots and tables.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main48_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(29)
# GetQfromDeath(6)
# GetS(2)
# 
# input data: data/output/M49/countryTrajectory/*.rda
#
# output data in folder data/output/M49/:
# 1. cis_M49_full.rda - world/region/country median and 90% CI 1950 - 2015;
# 2. trajectory_M49_S.rda - world/region/country trajectories of sex ratios of
#                           IMR, CMR, and U5MR 1950 - 2015;
# 3. trajectory_M49_Rx.rda - world/region/country trajectory of
#                            estimated/expected female mortality 1990 - 2015.
#
# note the indices in the output R object:
# *.cqt - country related; 
# *.rqt - region related;
# *.qt  - world related.
# 
###############################################################################


## construct empty arrays and metrics and assign dimention names ##
array.cqt <- array(NA, c(C, Per,    Tend))
array.clt <- array(NA, c(C, L,      Tend))
array.cjt <- array(NA, c(C, Lbysex, Tend))
array.rqt <- array(NA, c(R, Per,    Tend))
array.rjt <- array(NA, c(R, Lbysex, Tend))
matrix.qt <- matrix(NA, nr = Per,    nc = Tend)
matrix.jt <- matrix(NA, nr = Lbysex, nc = Tend)

dimnames(array.cqt)[[1]] <- dimnames(array.clt)[[1]] <-
  dimnames(array.cjt)[[1]] <- name.c

dimnames(array.cqt)[[2]] <- dimnames(matrix.qt)[[1]] <-
  dimnames(array.rqt)[[2]] <- percentiles

dimnames(array.cqt)[[3]] <- dimnames(array.clt)[[3]] <-
  dimnames(array.cjt)[[3]] <- dimnames(matrix.qt)[[2]] <-
  dimnames(matrix.jt)[[2]] <- dimnames(array.rqt)[[3]] <-
  dimnames(array.rjt)[[3]] <- floor(years.t)

dimnames(array.rqt)[[1]] <- dimnames(array.rjt)[[1]] <- regions.r

#############
## country ##
#estimated sex ratio
S1.cqt <- S4.cqt <- S5.cqt <-
  #expected sex ratio (only use median of Qtotal: Q1/4/5.ct)
  W1.cqt <- W4.cqt <- W5.cqt <-
  #country-specific multiplier (P = S/W)
  P1.cqt <- P4.cqt <- P5.cqt <-
  #expected sex ratio (use trajectory of Qtotal: Q1/4/5.ctj)
  Wx1.cqt <- Wx4.cqt <- Wx5.cqt <-
  #country-specific multiplier (Px = S/Wx)
  Px1.cqt <- Px4.cqt <- Px5.cqt <-
  #estimated/expected female mortality (Rx = Wx/S = 1/Px)
  Rx1.cqt <- Rx4.cqt <- Rx5.cqt <-
  
  #estimated total mortality (IGME estimates; not output of this project)
  Q1.cqt <- Q4.cqt <- Q5.cqt <-
  
  #estimated female mortality
  Q1f.cqt <- Q4f.cqt <- Q5f.cqt <-
  #estimated male mortality
  Q1m.cqt <- Q4m.cqt <- Q5m.cqt <-
  #estimated mortality difference: male - female
  Q1d.cqt <- Q4d.cqt <- Q5d.cqt <-
  
  #expected female mortality
  expQ1f.cqt <- expQ4f.cqt <- expQ5f.cqt <-
  #excess female mortality: estimated - expected
  excQ1f.cqt <- excQ4f.cqt <- excQ5f.cqt <-
  
  #estimated female deaths
  D1f.cqt <- D4f.cqt <- D5f.cqt <-
  #estimated male deaths
  D1m.cqt <- D4m.cqt <- D5m.cqt <-
  #expected female deaths
  expD1f.cqt <- expD4f.cqt <- expD5f.cqt <-
  #excess female deaths: estimated - expected
  excD1f.cqt <- excD4f.cqt <- excD5f.cqt <- array.cqt

#estimated sex ratio trajectory
S1.clt <- S4.clt <- S5.clt <- array.clt

#estimated/expected female mortality trajectory (Rx = Wx/S = 1/Px)
Rx1.cjt <- Rx4.cjt <- Rx5.cjt <-  
  #estimated male deaths trajectory
  D1m.cjt <- D4m.cjt <- D5m.cjt <- array.cjt

############
## global ##
#excess female deaths 
excD1f.jt <- excD4f.jt <- excD5f.jt <-
  #expected female deaths 
  expD1f.jt <- expD4f.jt <- expD5f.jt <-
  #estimated female deaths
  D1f.jt <- D4f.jt <- D5f.jt <-
  #estimated male deaths
  D1m.jt <- D4m.jt <- D5m.jt <-
  matrix(cutoffD, nr = Lbysex, nc = Tend)

S1.qt <- S4.qt <- S5.qt <-
  #expected sex ratio (use trajectory of Qtotal: Q1/4/5.ctj)
  Wx1.qt <- Wx4.qt <- Wx5.qt <-
  #estimated/expected female mortality (Rx = Wx/S = 1/Px)
  Rx1.qt <- Rx4.qt <- Rx5.qt <-
  #excess female mortality
  excQ1f.qt <- excQ4f.qt <- excQ5f.qt <-
  #expected female mortality
  expQ1f.qt <- expQ4f.qt <- expQ5f.qt <-
  #estimated female mortality
  Q1f.qt <- Q4f.qt <- Q5f.qt <-
  #estimated male mortality
  Q1m.qt <- Q4m.qt <- Q5m.qt <- matrix.qt

#estimated sex ratio
S1.jt <- S4.jt <- S5.jt <-
  #estimated/expected female mortality (Rx = Wx/S = 1/Px)
  Rx1.jt <- Rx4.jt <- Rx5.jt <- matrix.jt

##############
## regional ##
#excess female deaths
excD1f.rjt <- excD4f.rjt <- excD5f.rjt <-
  #expected female deaths 
  expD1f.rjt <- expD4f.rjt <- expD5f.rjt <-
  #estimated female deaths
  D1f.rjt <- D4f.rjt <- D5f.rjt <-  
  #estimated male deaths
  D1m.rjt <- D4m.rjt <- D5m.rjt <-
  array(cutoffD, c(R, Lbysex, Tend))

S1.rqt <- S4.rqt <- S5.rqt <-
  #expected sex ratio (use trajectory of Qtotal: Q1/4/5.ctj)
  Wx1.rqt <- Wx4.rqt <- Wx5.rqt <-
  #estimated/expected female mortality (Rx = Wx/S = 1/Px)
  Rx1.rqt <- Rx4.rqt <- Rx5.rqt <-
  #excess female deaths
  excD1f.rqt <- excD4f.rqt <- excD5f.rqt <-
  #expected female deaths
  expD1f.rqt <- expD4f.rqt <- expD5f.rqt <-
  #estimated female deaths
  D1f.rqt <- D4f.rqt <- D5f.rqt <-
  #estimated male deaths
  D1m.rqt <- D4m.rqt <- D5m.rqt <-
  #excess female mortality
  excQ1f.rqt <- excQ4f.rqt <- excQ5f.rqt <-
  #expected female mortality
  expQ1f.rqt <- expQ4f.rqt <- expQ5f.rqt <-
  #estimated female mortality
  Q1f.rqt <- Q4f.rqt <- Q5f.rqt <-
  #estimated male mortality
  Q1m.rqt <- Q4m.rqt <- Q5m.rqt <- array.rqt

#estimated sex ratio
S1.rjt <- S4.rjt <- S5.rjt <-
  #estimated/expected female mortality (Rx = Wx/S = 1/Px)
  Rx1.rjt <- Rx4.rjt <- Rx5.rjt <- array.rjt



## start the c loop ##

for(c in 1:C) {
  # read in country-specific trajectory
  load(file = paste0(countryTraj.dir, "cis_", runname, 
                     "_", iso.c[c], "_full.rda")) #res.c.full
  
  # identify entry position with Q1 > Q5 in IMR and U5MR trajectories ##
  setNA <- (Q1.ctj[c, , ] > Q5.ctj[c, , ]) & 
    !is.na(Q1.ctj[c, , ] + Q5.ctj[c, , ])
  NA.jt <- matrix(0, nr = Lbysex, nc = Tend)
  NA.jt[setNA] <- NA
  
  # region position based on sorted regions
  r <- which(regions.r == reg.c[c]) 
  
  
  for (age in ages.a) {
    
    ##############################
    ## non sex-specific results ##

    ## estimated sex ratio ##
    eval(parse(
      text = paste0("S", age, ".clt[c, , ] <- res.c.full$S", age, ".lt")
      ))
    
    eval(parse(
      text = paste0("S", age, ".cqt[c, , ] <- SamplesToUI(res.c.full$S", age, ".lt)")
      ))
    
    ## expected sex ratio (only use median of Qtotal: Q1/4/5.ct) ##
    eval(parse(
      text = paste0("W", age, ".cqt[c, , ] <- SamplesToUI(res.c.full$W", age, ".lt)")
    ))
    
    ## country-specific multiplier (P = S/W) ##
    eval(parse(
      text = paste0("P", age, ".cqt[c, , ] <- SamplesToUI(res.c.full$P", age, ".lt)")
    ))
    
  
    ##########################
    ## sex-specific results ##
    
    ## expected sex ratio (use trajectory of Qtotal: Q1/4/5.ctj) ##
    Wx.jt <- eval(parse(text = paste0("res.c.full$Q", age, "m.jt / res.c.full$expQ", age, "f.jt")))
    eval(parse(text = paste0("Wx", age, ".cqt[c, , ] <- SamplesToUI(Wx.jt, NA.jt = NA.jt)")))
    
    ## country-specific multiplier (Px = S/Wx) ##
    Px.jt <- eval(parse(text = paste0("res.c.full$S", age, ".lt[selectl.j, ] / Wx.jt")))
    eval(parse(text = paste0("Px", age, ".cqt[c, , ] <- SamplesToUI(Px.jt, NA.jt = NA.jt)")))
    
    ## estimated/expected female mortality (Rx = Wx/S = 1/Px) ##
    Rx.jt <- eval(parse(text = paste0("res.c.full$Q", age, "f.jt / res.c.full$expQ", age, "f.jt")))
    eval(parse(text = paste0("Rx", age, ".cjt[c, , ] <- Rx.jt + NA.jt")))
    eval(parse(text = paste0("Rx", age, ".cqt[c, , ] <- SamplesToUI(Rx.jt, NA.jt = NA.jt)")))
    
    ## estimated mortality ##
    eval(parse(text = paste0("Q", age, ".cqt[c, , ] <- SamplesToUI(res.c.full$Q", age, ".jt, NA.jt = NA.jt)")))
    
    ## estimated female mortality ##
    eval(parse(text = paste0("Q", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$Q", age, "f.jt, NA.jt = NA.jt)")))
    
    ## estimated male mortality ##
    eval(parse(text = paste0("Q", age, "m.cqt[c, , ] <- SamplesToUI(res.c.full$Q", age, "m.jt, NA.jt = NA.jt)")))
    
    ## estimated mortality difference: male - female ##
    eval(parse(text = paste0("Q", age, "d.cqt[c, , ] <- SamplesToUI(res.c.full$Q", age, "d.jt, NA.jt = NA.jt)")))
    
    ## expected female mortality ##
    eval(parse(text = paste0("expQ", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$expQ", age, "f.jt, NA.jt = NA.jt)")))
    
    ## excess female mortality: estimated - expected ##
    eval(parse(text = paste0("excQ", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$excQ", age, "f.jt, NA.jt = NA.jt)")))
    

    #############################
    ## estimated female deaths ##
    #country-specific
    eval(parse(text = paste0("D", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$D", age, "f.jt, NA.jt = NA.jt)")))
    #region-specific
    eval(parse(text = paste0("D", age, "f.rjt[r, , ] <- D", age, "f.rjt[r, , ] + res.c.full$D", age, "f.jt + NA.jt")))
    #global
    eval(parse(text = paste0("D", age, "f.jt <- D", age, "f.jt + res.c.full$D", age, "f.jt + NA.jt")))
    
    ###########################
    ## estimated male deaths ##
    #trajectory
    for (t in 1:Tend) {
      deathMhat <- GetDeath(q1 = res.c.full[["Q1m.jt"]][, t], 
                            q4 = res.c.full[["Q4m.jt"]][, t], 
                            q5 = res.c.full[["Q5m.jt"]][, t],
                            a1 = a1.c[c], a4 = a4.c[c],
                            pop1 = pop1M.ct[c, t], pop4 = pop4M.ct[c, t])
      eval(parse(text = paste0("D", age, "m.cjt[c, , t] <- deathMhat$death", age)))
    }#end of t loop
    
    #country-specific
    eval(parse(text = paste0("D", age, "m.cqt[c, , ] <- SamplesToUI(D", age, "m.cjt[c, , ], NA.jt = NA.jt)")))
    #region-specific
    eval(parse(text = paste0("D", age, "m.rjt[r, , ] <- D", age, "m.rjt[r, , ] + D", age, "m.cjt[c, , ] + NA.jt")))
    #global
    eval(parse(text = paste0("D", age, "m.jt <- D", age, "m.jt + D", age, "m.cjt[c, , ] + NA.jt")))
    
    ############################
    ## expected female deaths ##
    #country-specific
    eval(parse(text = paste0("expD", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$expD", age, "f.jt, NA.jt = NA.jt)")))
    #region-specific
    eval(parse(text = paste0("expD", age, "f.rjt[r, , ] <- expD", age, "f.rjt[r, , ] + res.c.full$expD", age, "f.jt + NA.jt")))
    #global
    eval(parse(text = paste0("expD", age, "f.jt <- expD", age, "f.jt + res.c.full$expD", age, "f.jt + NA.jt")))
    
    ################################################
    ## excess female deaths: estimated - expected ##
    #country-specific
    eval(parse(text = paste0("excD", age, "f.cqt[c, , ] <- SamplesToUI(res.c.full$excD", age, "f.jt, NA.jt = NA.jt)")))
    #region-specific
    eval(parse(text = paste0("excD", age, "f.rjt[r, , ] <- excD", age, "f.rjt[r, , ] + res.c.full$excD", age, "f.jt + NA.jt")))
    #global
    eval(parse(text = paste0("excD", age, "f.jt <- excD", age, "f.jt + res.c.full$excD", age, "f.jt + NA.jt")))
    
  }#end of age loop  
  
}#end of c loop



for (age in ages.a) {
  
  ############
  ## global ##
  # excess female deaths
  eval(parse(text = paste0("excD", age, "f.qt <- SamplesToUI(excD", age, "f.jt)")))
  # expected female deaths
  eval(parse(text = paste0("expD", age, "f.qt <- SamplesToUI(expD", age, "f.jt)")))
  # estimated female deaths
  eval(parse(text = paste0("D", age, "f.qt <- SamplesToUI(D", age, "f.jt)")))
  # estimated male deaths
  eval(parse(text = paste0("D", age, "m.qt <- SamplesToUI(D", age, "m.jt)")))
  
  ##############
  ## regional ##
  for (r in 1:R) {
    # excess female deaths
    eval(parse(text = paste0("excD", age, "f.rqt[r, , ] <- SamplesToUI(excD", age, "f.rjt[r, , ])")))
    # expected female deaths
    eval(parse(text = paste0("expD", age, "f.rqt[r, , ] <- SamplesToUI(expD", age, "f.rjt[r, , ])")))
    # estimated female deaths
    eval(parse(text = paste0("D", age, "f.rqt[r, , ] <- SamplesToUI(D", age, "f.rjt[r, , ])")))
    # estimated male deaths
    eval(parse(text = paste0("D", age, "m.rqt[r, , ] <- SamplesToUI(D", age, "m.rjt[r, , ])")))
  }#end of r loop
  
}#end of age loop

##############################################
## regional excQf, expQf, Qf, Qm, S, Wx, Rx ##
for (r in 1:R) {
  for (t in 1:Tend) {
    
    for (age in ages.a) {
      ## excess female deaths ##
      eval(parse(text = paste0("excD", age, "f.j <- excD", age, "f.rjt[r, , t]")))
      ## expected female deaths ##
      eval(parse(text = paste0("expD", age, "f.j <- expD", age, "f.rjt[r, , t]")))
      ## estimated female deaths ##
      eval(parse(text = paste0("D", age, "f.j <- D", age, "f.rjt[r, , t]")))
      ## estimated male deaths ##
      eval(parse(text = paste0("D", age, "m.j <- D", age, "m.rjt[r, , t]")))      
    }#end of age loop
    
    ## female population ##
    pick.c <- which(reg.c == regions.r[r])
    pop1F <- sum(pop1F.ct[pick.c, t])
    pop4F <- sum(pop4F.ct[pick.c, t])
    pop5F <- pop1F + pop4F
    ## male population ##
    pop1M <- sum(pop1M.ct[pick.c, t])
    pop4M <- sum(pop4M.ct[pick.c, t])
    pop5M <- pop1M + pop4M
    
    ## excess Qfemale ##
    excQf.aj <- GetQfromDeath(death1 = excD1f.j, death4 = excD4f.j, 
                              pop1 = pop1F, pop4 = pop4F, 
                              a1 = a1.c[pick.c], a4 = a4.c[pick.c])
    ## expected Qfemale ##
    expQf.aj <- GetQfromDeath(death1 = expD1f.j, death4 = expD4f.j, 
                              pop1 = pop1F, pop4 = pop4F, 
                              a1 = a1.c[pick.c], a4 = a4.c[pick.c])
    ## estimated Qfemale ##
    Qf.aj <- GetQfromDeath(death1 = D1f.j, death4 = D4f.j, 
                           pop1 = pop1F, pop4 = pop4F, 
                           a1 = a1.c[pick.c], a4 = a4.c[pick.c])
    ## estimated Qmale ##
    Qm.aj <- GetQfromDeath(death1 = D1m.j, death4 = D4m.j, 
                           pop1 = pop1M, pop4 = pop4M, 
                           a1 = a1.c[pick.c], a4 = a4.c[pick.c])
    
    for (age in ages.a) {
      eval(parse(text = paste0("excQ", age, "f.rqt[r, , t] <- SamplesToUI(excQf.aj$q", age, ")")))
      eval(parse(text = paste0("expQ", age, "f.rqt[r, , t] <- SamplesToUI(expQf.aj$q", age, ")")))
      eval(parse(text = paste0("Q", age, "f.rqt[r, , t] <- SamplesToUI(Qf.aj$q", age, ")")))
      eval(parse(text = paste0("Q", age, "m.rqt[r, , t] <- SamplesToUI(Qm.aj$q", age, ")")))

      ## S1/4/5 ##
      S.j <- eval(parse(text = paste0("GetS(qmale = Qm.aj$q", age, ", qfemale = Qf.aj$q", age, ")")))
      eval(parse(text = paste0("S", age, ".rjt[r, , t] <- S.j")))
      eval(parse(text = paste0("S", age, ".rqt[r, , t] <- SamplesToUI(S.j)")))
      
      ## Wx1/4/5 ##
      Wx.j <- eval(parse(text = paste0("Qm.aj$q", age, "/ expQf.aj$q", age)))
      eval(parse(text = paste0("Wx", age, ".rqt[r, , t] <- SamplesToUI(Wx.j)")))
      
      ## Rx1/4/5 ##
      Rx.j <- eval(parse(text = paste0("Qf.aj$q", age, " / expQf.aj$q", age)))
      eval(parse(text = paste0("Rx", age, ".rjt[r, , t] <- Rx.j")))
      eval(parse(text = paste0("Rx", age, ".rqt[r, , t] <- SamplesToUI(Rx.j)")))
      
    }#end of age loop
        
  }#end of t loop
}#end of r loop

#####################################
## global excQf, Qf, Qm, S, Wx, Rx ##
for (t in 1:Tend) {
  
  for (age in ages.a) {
    ## excess female deaths ##
    eval(parse(text = paste0("excD", age, "f.j <- excD", age, "f.jt[, t]")))
    ## expected female deaths ##
    eval(parse(text = paste0("expD", age, "f.j <- expD", age, "f.jt[, t]")))
    ## estimated female deaths ##
    eval(parse(text = paste0("D", age, "f.j <- D", age, "f.jt[, t]")))
    ## estimated male deaths ##
    eval(parse(text = paste0("D", age, "m.j <- D", age, "m.jt[, t]")))
  }#end of age loop
  
  ## female population ##
  pop1F <- sum(pop1F.ct[, t])
  pop4F <- sum(pop4F.ct[, t])
  pop5F <- pop1F + pop4F
  ## male population ##
  pop1M <- sum(pop1M.ct[, t])
  pop4M <- sum(pop4M.ct[, t])
  pop5M <- pop1M + pop4M
  
  ## excess Qfemale ##
  excQf.aj <- GetQfromDeath(death1 = excD1f.j, death4 = excD4f.j, 
                            pop1 = pop1F, pop4 = pop4F, 
                            a1 = a1.c, a4 = a4.c)
  ## expected Qfemale ##
  expQf.aj <- GetQfromDeath(death1 = expD1f.j, death4 = expD4f.j, 
                            pop1 = pop1F, pop4 = pop4F, 
                            a1 = a1.c, a4 = a4.c)
  ## estimated Qfemale ##
  Qf.aj <- GetQfromDeath(death1 = D1f.j, death4 = D4f.j, 
                         pop1 = pop1F, pop4 = pop4F, 
                         a1 = a1.c, a4 = a4.c)
  ## estimated Qmale ##
  Qm.aj <- GetQfromDeath(death1 = D1m.j, death4 = D4m.j, 
                         pop1 = pop1M, pop4 = pop4M, 
                         a1 = a1.c, a4 = a4.c)
  
  for (age in ages.a) {
    eval(parse(text = paste0("excQ", age, "f.qt[, t] <- SamplesToUI(excQf.aj$q", age, ")")))
    eval(parse(text = paste0("expQ", age, "f.qt[, t] <- SamplesToUI(expQf.aj$q", age, ")")))
    eval(parse(text = paste0("Q", age, "f.qt[, t] <- SamplesToUI(Qf.aj$q", age, ")")))
    eval(parse(text = paste0("Q", age, "m.qt[, t] <- SamplesToUI(Qm.aj$q", age, ")")))
    
    ## S1/4/5 ##
    S.j <- eval(parse(text = paste0("GetS(qmale = Qm.aj$q", age, ", qfemale = Qf.aj$q", age, ")")))
    eval(parse(text = paste0("S", age, ".jt[, t] <- S.j")))
    eval(parse(text = paste0("S", age, ".qt[, t] <- SamplesToUI(S.j)")))
    
    ## Wx1/4/5 ##
    Wx.j <- eval(parse(text = paste0("Qm.aj$q", age, "/ expQf.aj$q", age)))
    eval(parse(text = paste0("Wx", age, ".qt[, t] <- SamplesToUI(Wx.j)")))
    
    ## Rx1/4/5 ##
    Rx.j <- eval(parse(text = paste0("Qf.aj$q", age, " / expQf.aj$q", age)))
    eval(parse(text = paste0("Rx", age, ".jt[, t] <- Rx.j")))
    eval(parse(text = paste0("Rx", age, ".qt[, t] <- SamplesToUI(Rx.j)")))
    
  }#end of age loop
    
}#end of t loop


##################################
## save country combined result ##
res.both <- list(  
  #to identify all countries with estimates
  iso.c = iso.c,
  
  logQ1.k = logQ1.k, logQ4.k = logQ4.k,  
  Q1.i = Q1.i, Q4.i = Q4.i, Q5.i = Q5.i,
  W4mean.k = W4mean.k, W1mean.k = W1mean.k, W5median.i = W5median.i,
  W1.qk = W1.qk, W4.qk = W4.qk,
  
  #estimated sex ratio
  S1.cqt = S1.cqt, S4.cqt = S4.cqt, S5.cqt = S5.cqt, 
  #expected sex ratio (only use median of Qtotal: Q1/4/5.ct)
  W1.cqt = W1.cqt, W4.cqt = W4.cqt, W5.cqt = W5.cqt, 
  #country-specific multiplier (P = S/W)
  P1.cqt = P1.cqt, P4.cqt = P4.cqt, P5.cqt = P5.cqt
)

if (getQbysex) {
  res.bySex <- list(
    #estimated sex ratio
    #region
    S1.rqt = S1.rqt, S4.rqt = S4.rqt, S5.rqt = S5.rqt,
    #world
    S1.qt = S1.qt, S4.qt = S4.qt, S5.qt = S5.qt,
    
    #expected sex ratio (use trajectory of Qtotal: Q1/4/5.ctj)
    #country
    Wx1.cqt = Wx1.cqt, Wx4.cqt = Wx4.cqt, Wx5.cqt = Wx5.cqt, 
    #region
    Wx1.rqt = Wx1.rqt, Wx4.rqt = Wx4.rqt, Wx5.rqt = Wx5.rqt, 
    #world
    Wx1.qt = Wx1.qt, Wx4.qt = Wx4.qt, Wx5.qt = Wx5.qt,
    
    #country-specific multiplier (Px = S/Wx)
    Px1.cqt = Px1.cqt, Px4.cqt = Px4.cqt, Px5.cqt = Px5.cqt,
    
    #estimated/expected female mortality trajectory (Rx = Wx/S = 1/Px)
    #country
    Rx1.cqt = Rx1.cqt, Rx4.cqt = Rx4.cqt, Rx5.cqt = Rx5.cqt, 
    #region
    Rx1.rqt = Rx1.rqt, Rx4.rqt = Rx4.rqt, Rx5.rqt = Rx5.rqt, 
    #world
    Rx1.qt = Rx1.qt, Rx4.qt = Rx4.qt, Rx5.qt = Rx5.qt, 
    
    #total mortality
    Q1.cqt = Q1.cqt, Q4.cqt = Q4.cqt, Q5.cqt = Q5.cqt,
    
    #estimated female mortality  
    #country
    Q1f.cqt = Q1f.cqt, Q4f.cqt = Q4f.cqt, Q5f.cqt = Q5f.cqt, 
    #region
    Q1f.rqt = Q1f.rqt, Q4f.rqt = Q4f.rqt, Q5f.rqt = Q5f.rqt, 
    #global
    Q1f.qt = Q1f.qt, Q4f.qt = Q4f.qt, Q5f.qt = Q5f.qt, 
    
    #estimated male mortality 
    #country
    Q1m.cqt = Q1m.cqt, Q4m.cqt = Q4m.cqt, Q5m.cqt = Q5m.cqt,
    #region
    Q1m.rqt = Q1m.rqt, Q4m.rqt = Q4m.rqt, Q5m.rqt = Q5m.rqt, 
    #global
    Q1m.qt = Q1m.qt, Q4m.qt = Q4m.qt, Q5m.qt = Q5m.qt, 
    
    #estimated mortality difference (male-female)
    Q1d.cqt = Q1d.cqt, Q4d.cqt = Q4d.cqt, Q5d.cqt = Q5d.cqt,
    
    #expected female mortality
    #country
    expQ1f.cqt = expQ1f.cqt, expQ4f.cqt = expQ4f.cqt, expQ5f.cqt = expQ5f.cqt,
    #region
    expQ1f.rqt = expQ1f.rqt, expQ4f.rqt = expQ4f.rqt, expQ5f.rqt = expQ5f.rqt,
    #global
    expQ1f.qt = expQ1f.qt, expQ4f.qt = expQ4f.qt, expQ5f.qt = expQ5f.qt, 
    
    #excess female mortality (estimated-expected)
    #country
    excQ1f.cqt = excQ1f.cqt, excQ4f.cqt = excQ4f.cqt, excQ5f.cqt = excQ5f.cqt,
    #region
    excQ1f.rqt = excQ1f.rqt, excQ4f.rqt = excQ4f.rqt, excQ5f.rqt = excQ5f.rqt,
    #global
    excQ1f.qt = excQ1f.qt, excQ4f.qt = excQ4f.qt, excQ5f.qt = excQ5f.qt, 
    
    #estimated female deaths 
    #country
    D1f.cqt = D1f.cqt, D4f.cqt = D4f.cqt, D5f.cqt = D5f.cqt, 
    #region
    D1f.rqt = D1f.rqt, D4f.rqt = D4f.rqt, D5f.rqt = D5f.rqt, 
    #global
    D1f.qt = D1f.qt, D4f.qt = D4f.qt, D5f.qt = D5f.qt,       
    
    #estimated male deaths 
    #country
    D1m.cqt = D1m.cqt, D4m.cqt = D4m.cqt, D5m.cqt = D5m.cqt, 
    #region
    D1m.rqt = D1m.rqt, D4m.rqt = D4m.rqt, D5m.rqt = D5m.rqt, 
    #global
    D1m.qt = D1m.qt, D4m.qt = D4m.qt, D5m.qt = D5m.qt,       
    
    #expected female deaths
    #country
    expD1f.cqt = expD1f.cqt, expD4f.cqt = expD4f.cqt, expD5f.cqt = expD5f.cqt,
    #region
    expD1f.rqt = expD1f.rqt, expD4f.rqt = expD4f.rqt, expD5f.rqt = expD5f.rqt, 
    #global
    expD1f.qt = expD1f.qt, expD4f.qt = expD4f.qt, expD5f.qt = expD5f.qt,       
    
    #excess female deaths (estimated-expected)  
    #country
    excD1f.cqt = excD1f.cqt, excD4f.cqt = excD4f.cqt, excD5f.cqt = excD5f.cqt,
    #regional
    excD1f.rqt = excD1f.rqt, excD4f.rqt = excD4f.rqt, excD5f.rqt = excD5f.rqt,
    #global
    excD1f.qt = excD1f.qt, excD4f.qt = excD4f.qt, excD5f.qt = excD5f.qt        
  )  
} else {
  res.bySex <- NULL
}

## save country/region/world median and 90% UI for all results ##
res.full <- c(res.both, res.bySex) #results for [1950, 2015]
save(res.full, file = paste0(output.dir, "cis_", runname, "_full.rda"))


## save country/region/world S1/4/5 trajectories ##
res.Strajectory <- list(S1.jt = S1.jt, S1.rjt = S1.rjt, S1.clt = S1.clt,
                        S4.jt = S4.jt, S4.rjt = S4.rjt, S4.clt = S4.clt,
                        S5.jt = S5.jt, S5.rjt = S5.rjt, S5.clt = S5.clt)
save(res.Strajectory, file = paste0(output.dir, "trajectory_", runname, "_S.rda"))

## save Rx1/4/5 country/region/world trajectories ##
res.Rxtrajectory <- list(Rx1.cjt = Rx1.cjt, Rx4.cjt = Rx4.cjt, Rx5.cjt = Rx5.cjt,
                         Rx1.rjt = Rx1.rjt, Rx4.rjt = Rx4.rjt, Rx5.rjt = Rx5.rjt,
                         Rx1.jt  = Rx1.jt,  Rx4.jt  = Rx4.jt,  Rx5.jt  = Rx5.jt)
save(res.Rxtrajectory, file = paste0(output.dir, "trajectory_", runname, "_Rx.rda"))

## the end ##



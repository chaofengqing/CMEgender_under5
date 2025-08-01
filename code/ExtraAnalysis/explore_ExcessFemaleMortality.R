
#--------------------------------------
# construct_excessfemalemortality.R
# LA, Aug 14, 2013
#--------------------------------------
# start in main_output
# note: this is code based on medians only!

############
## PART 1 ##
load(file=paste0(output.dir,"cis_",runname,"_2012.rda")) #res2012
res <- res2012
# we can get expected female Q for given male Q for a = 1
# note: not for a=4, because w_a is not known (because Q1(M) is not known)
gridQtot.k   <- exp(logQ1.k) #0.001, 0.280
qm.b <- gridQmale.b <- c(seq(min(gridQtot.k),0.010,0.0001), seq(0.011,max(gridQtot.k),0.001)) #seq(0.001, 0.270, 0.001)
B <- length(gridQmale.b)
wa = w1
W.k <- res$ci1.qk[percentiles==0.5,]
qf.b <- rep(NA, B)
for (b in 1:B){
  qm <- gridQmale.b[b]
  # doesnt work well
  #  qf.b[b] <- exp(optim(log(qm*1.1), error, method = "L-BFGS-B", control = list(maxit = 1000),
  #                       #lower = 0.001, 
  #                       upper = 0)$par)
  qf.b[b] <- exp(optimize(f= error,interval = c(log(0.00001),0))$minimum) #0.0005
}
res1 <- list(qf.b = qf.b, qm.b = qm.b)
save(res1, file = paste0(output.dir,"expectedq1female.rda"))

plot(y=qf.b*1000, x=qm.b*1000, type="l") #xlim=c(0,4), ylim=c(0,20)

############
## PART 2 ##
qfexpected.act <- array(NA, c(3, C, Tend))
gridQ4tot.k <- exp(logQ4.k)
W4.k <- res$ci4.qk[percentiles==0.5,]

# note: for a=1, we can use the relation between q1.f and q1.m as derived in part 1
load(file = paste0(output.dir,"expectedq1female.rda")) #res1
q1f.b <- res1$qf.b
q1m.b <- res1$qm.b
wa.ct <- matrix(NA, nr=C, nc=Tend)
for (c in 1:C){
  for (t in 1:Tend){
    if (!is.na(res$Q1m.cqt[c,2,t])){
      qfexpected.act[1,c,t] <- q1f.b[which.min(abs(res$Q1m.cqt[c,2,t]-q1m.b))]
    }
  }#end of t loop
  #a=4
  # use error function, thus need to define qm and wa as well as W
  gridQtot.k <- gridQ4tot.k
  W.k <- W4.k
  for (t in 1:Tend){
    if (!is.na(res$Q1m.cqt[c,2,t]) & !is.na(res$Q4m.cqt[c,2,t]))# & res$Q4m.cqt[c,2,t]>=0.004
    {
      # for a=4, get wa from Q1male and Q1total
      qm <- res$Q4m.cqt[c,2,t]
      wa.ct[c,t] <- wa <- GetWeight4(w1=w1, q1male=res$Q1m.cqt[c,2,t], q1both=Q1.ct[c,t])
#       ##################
#       ## original way ##
#       qfexpected.act[2,c,t] <- exp(optimize(f=error, interval=range(logqf))$minimum)
#       ##################
#       
#       ##################################      
#       ## WAY 1: restricting the optim ##
#       if(qm<0.010) gridQtot.k <- gridQtot.k[1:5] #qm>0.0015 & qm<0.0025
#       qfexpected.act[2,c,t] <- exp(optimize(f=error, interval=c(log(0.00001),0))$minimum)
#       ##################################
#       
      #############################################
      ## WAY 2: new function to get optimization ##
      qf <- seq(0.0001, 1, 0.00001)
      pick <- rep(NA, length(qf))      
      for(f in 1:length(qf)){
        pick[f] <- which.min(abs(gridQtot.k - GetQboth(qm, qf[f],wa)))
      }#end of f loop
      qfexpected.act[2,c,t] <- qf[which.min(abs(qm/qf - W.k[pick]))]
      #############################################
#       
#       ########################
#       ## WAY 3: use optim() ##
#       qfexpected.act[2,c,t] <- exp(optim(log(qm*1.1), error, 
#                                          method = "L-BFGS-B", control = list(maxit = 1000),
#                                          upper = 0)$par)  
#       ########################
#       
#       ########################################################################
#       ## WAY 4: restrict interval for optimization for diff intervals of qm ##
#       if(qm <= 0.010) qfexpected.act[2,c,t] <- exp(optimize(f=error, interval=c(log(0.0001),log(0.020)))$minimum)
#       if(qm > 0.010 & qm <=0.050) qfexpected.act[2,c,t] <- exp(optimize(f=error, interval=c(log(0.005),log(0.100)))$minimum)  
#       if(qm > 0.050) qfexpected.act[2,c,t] <- exp(optimize(f=error, interval=c(log(0.020),0))$minimum)
#       ########################################################################
      
    }#end of if
  }#end of t loop
  # a = 5 use qf for a=1 and 4
  qfexpected.act[3,c,] <- GetQ5fromQ14(q1=qfexpected.act[1,c,], q4=qfexpected.act[2,c,]) 
}#end of c loop

q1totexpected.ct <- (w1*round(res$Q1m.cqt[,2,],3)+(1-w1)*qfexpected.act[1,,])

excess.act <- relexcess.act <- array(NA, c(3,C,Tend))
for (c in 1:C){
  for(age in ages){
    a <- which(ages == age)
    # observed female - expected female
    excess.act[a,c,] <- y <- eval(parse(text = paste0("1000*(res$Q",age,"f.cqt[c,2,] - qfexpected.act[",a,",c,])")))
    relexcess.act[a,c,] <- eval(parse(text = paste0("100*((excess.act[j,c,]/1000)/res$Q",age,"f.cqt[c,2,] )")))
  }#end of age loop
}#end of c loop

res_test <- list(qfexpected.act=qfexpected.act,
                 q1totexpected.ct=q1totexpected.ct,
                 excess.act=excess.act,
                 relexcess.act=relexcess.act,
                 wa.ct=wa.ct)
save(res_test,  file = paste0(output.dir,"test_ExcessExpected_origin.rda"))

############
## PART 3 ##
# not sure why we get the outliers for high q1 total?
load(file=paste0(output.dir,"test_ExcessExpected_origin.rda")) #res_test

plot(c(round(res$Q1m.cqt[,2,],3)/res_test$qfexpected.act[1,,]) ~ c(res_test$q1totexpected.ct[,]) , ylim = c(0.95, 1.3), log = "x")
qtot.b <- (w1*q1m.b+(1-w1)*q1f.b)
lines(q1m.b/q1f.b ~ qtot.b, col = 2, lwd = 3)
lines(res$ci1.qk[2,]~ exp(res$logQ1.k), col = 2, lwd = 3)


c=1
c <- which(name.c=="Jordan")
c <- which(name.c=="Nepal")
c <- which(name.c=="Pakistan")
c <- which(name.c=="China")

# something wrong for very low Q4... in optim maybe?
# so set all to NA for low Q4
c <- which(name.c=="Korea Rep")
t <- which.min(res$Q4f.cqt[c,2,]-qfexpected.act[2,c,] )
res$Q4f.cqt[c,2,t]
qfexpected.act[2,c,t]
qfexpected.act[2,c,]
plot(qfexpected.act[2,c,],type="l",lty=1)

## further explore the problem in low Q4 ##
c <- which(name.c=="Albania")
c <- which(name.c=="Antigua & Barbuda")
c <- which(name.c=="Suriname")

select.lowqm <- which(res$Q4m.cqt[c,2,]<=0.010)
logqf <- log(seq(0.0001, 1, 0.00001))#log(res_test$qfexpected.act[2,c,t])
W.k <- W4.k
gridQtot.k <- exp(logQ4.k)
ErrorPlot.ib <- matrix(NA, nr=length(select.lowqm),nc=length(logqf))
for (i in 1:dim(ErrorPlot.ib)[1]){
  t <- which(res$Q4m.cqt[c,2,]<=0.010)[i]
  wa <- res_test$wa.ct[c,t]  
  qm <- res$Q4m.cqt[c,2,t] 
  for(b in 1:length(logqf)){
    ErrorPlot.ib[i,b] <- error(logqf[b])  
  }#end of b loop  
}#end of i loop

pdf(paste0(fig.dir,"test_ErrorandQf_",name.c[c],"(1-10)_range.pdf"), height = 15, width = 20)
par(mfrow = c(2,3), cex.lab = 2.5, cex.axis = 2, cex.main=2.5,
    mar = c(4.5,6,8,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))
for(i in 1:dim(ErrorPlot.ib)[1]){
  t <- which(res$Q4m.cqt[c,2,]<=0.010)[i]
  plot(y=ErrorPlot.ib[i,], x=logqf, type="l", xlab="expected log(Q4female)", ylab="error",
       main=c(paste0("Q4male*1000=",round(res$Q4m.cqt[c,2,t]*1000,1),
                     " year=",floor(years)[t]))) #exp(logqf)*1000
  wa <- res_test$wa.ct[c,t]  
  qm <- res$Q4m.cqt[c,2,t] 
  ## use which.min() ##
  abline(v=logqf[which.min(ErrorPlot.ib[i,])], lwd=1.5)
  
  ## use optimize() ##
  if(qm <= 0.010) abline(v=optimize(f=error, interval=c(log(0.0001),log(0.020)))$minimum, col=2, lwd=1.5)
  if(qm > 0.010 & qm <=0.050) abline(v=optimize(f=error, interval=c(log(0.005),log(0.100)))$minimum, col=2, lwd=1.5)  
  if(qm > 0.050) abline(v=optimize(f=error, interval=c(log(0.020),0))$minimum, col=2, lwd=1.5)
  
#   abline(v=optimize(f=error, interval=c(log(0.001),0))$minimum, col=2, lwd=1.5)
#   abline(v=optimize(f=error, interval=c(log(0.00001),0), tol=1e-40)$minimum, col=3, lwd=1.5)
  #change the interval to be just the same as logqf to get the global minimum!
#   abline(v=optimize(f=error, interval=range(logqf))$minimum, col=2, lwd=1.5)
  
  #the optimize result is not the next several minimum near the global minimum of error
  #   abline(v=logqf[order(ErrorPlot.ib[i,])][3], lwd=1.5) #not the same as optimize
#   pick.tmp <- which(logqf> -4 & logqf< 0)
#   pick <- pick.tmp[which.min(ErrorPlot.ib[i,pick.tmp])]
#   abline(v=logqf[pick], lwd=1.5, col=3) #optimize is the local minimum within the interval [-4,0]
  
  ## use optim() ##
  abline(v=optim(log(qm*1.1), error, method = "L-BFGS-B", control = list(maxit = 1000),
                       #lower = 0.001, 
                       upper = 0)$par, col=4, lwd=1.5)
    
  legend("topright",c("which.min","optimize","optim"), lty=1,col=c(1,2,4), cex=2, lwd=5)
}#end of i loop
dev.off()



###################################
## plot country median estimates ##
pdf(paste0(fig.dir,"test_ExpectedQ4f(lowQ4m)_range.pdf"), height = 6, width = 10)
par(mfrow = c(1,2),las=1)
for(c in 1:C){
  plot(y=qfexpected.act[2,c,]*1000, x=res$Q4m.cqt[c,2,]*1000,type="l",
       lwd=2, xlab="estimated Q4male*1000", ylab="expected Q4female*1000")
  points(y=qfexpected.act[2,c,]*1000, x=res$Q4m.cqt[c,2,]*1000, col=4, pch=19,cex=0.8)
  select <- res$Q4m.cqt[c,2,]<=0.004
  if(sum(select,na.rm=TRUE)>0){
    plot(y=qfexpected.act[2,c,select]*1000, x=res$Q4m.cqt[c,2,select]*1000,type="l",xlim=c(0,4),
         lwd=3, xlab="estimated Q4male*1000", ylab="expected Q4female*1000")
    points(y=qfexpected.act[2,c,select]*1000, x=res$Q4m.cqt[c,2,select]*1000, col=2, pch=19,cex=1)      
  } else{
    plot(1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  }
  title(main=name.c[c], outer=TRUE, line=-2.5, cex.main=2)
}#end of c loop
dev.off()



plot(y=qfexpected.act[2,c,!select]*1000, x=res$Q4m.cqt[c,2,!select]*1000,type="l")

plot(y=qfexpected.act[2,c,]*1000, x=wa.ct[c,], type="l")

pdf(paste0(fig.dir,"test_ExcessFemale_range.pdf"), width = 10, height = 5)
par(mfrow= c(2,3))
for (c in 1:C){
  for (age in ages){
    a <- which(ages == age) #= c(1,4,5)[j]
    y <- excess.act[a,c,]
    x <- eval(parse(text = paste0("res$Q",age,"m.cqt[c,2,]")))
    plot(y ~ x,
         log= "x", 
         type = "l", col = 2, ylim = c(min(y,-20, na.rm = T),max(y, 20, na.rm = T)),
         xlab = paste0("Q",age," (log-scale)"),
         ylab = paste0("Excess female mortality, a = ",age), main = name.c[c])
    abline(h=0)
  }#end of age loop
  
  for(age in ages){
    a <- which(ages == age)
    y <- excess.act[a,c,]
    x <- years
    plot( y ~ x,type = "l", col = 2, 
          ylim = c(min(y,-20, na.rm = T),max(y, 20, na.rm = T)),
          xlab = "time",
          ylab = paste0("Excess female mortality, a = ",age), 
          main = name.c[c])
    abline(h=0)
  }#end of age loop
}#end of c loop
dev.off()

apply(excess.act[,,years=="2005.5"], 1, mean, na.rm = T)
hist(excess.act[1,,years=="2005.5"])
hist(excess.act[2,,years=="2005.5"])
hist(excess.act[3,,years=="2005.5"])



t = which(years=="2005.5")
select.c <- abs(exp(res$logP5.cqt[,2,t])-1)>0.05
hist(excess.act[3,select.c,years=="2005.5"])

name.c[select.c & excess.act[3,,years=="2005.5"]< -5]
name.c[select.c & excess.act[3,,years=="2005.5"]> 5]

name.c[select.c & excess.act[3,,years=="2005.5"]< -5]
name.c[select.c & excess.act[3,,years=="2005.5"]> 5]

name.c[select.c & excess.act[3,,years=="2005.5"]< -5]
name.c[select.c & excess.act[3,,years=="2005.5"]> 5]

year = 2012.5
t = which(years==year)
select.c <- abs(exp(res$logP5.cqt[,2,t])-1)>0.05
name.c[select.c & excess.act[3,,t]< -5]
name.c[select.c & excess.act[3,,t]> 5]

name.c=="Egypt"

# relative excess mortality
pdf(paste0(fig.dir,"test_RelativeExcessFemale_range.pdf"), width = 10, height = 4)
par(mfrow= c(1,3))
for (c in 1:C){
  for (age in age){
    a <- which(ages == age)
    # observed female - expected female
    y <- relexcess.act[j,c,]    
    x <- years
    
    plot( y ~ x,type = "l", col = 2, 
          ylim = c(min(y,-10, na.rm = T),max(y, 10, na.rm = T)),
          xlab = "time",
          ylab = paste("Excess female mortality (%), a = ",age), 
          main = name.c[c])
    abline(h=0)
  }#end of age loop
}#end of c loop
dev.off()

year = 2012.5
t = which(years==year)
select.c <- abs(exp(res$logP5.cqt[,2,t])-1)>0.05
name.c[select.c & relexcess.act[1,,t]< -5]
name.c[select.c & relexcess.act[1,,t]> 5]

select.c <- rep(T, C) & Q1.ct[,t]>0.005
name.c[select.c & relexcess.act[1,,t]< -5]
name.c[select.c & relexcess.act[1,,t]> 5]



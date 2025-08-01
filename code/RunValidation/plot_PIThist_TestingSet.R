## run construct_ErrorRelativeErrorCoverage_TestingSet.R first ##
## compute mean(ypredicts  <= yobs) ##
## read in data ##
load(file = paste0(output.dir,"sPredict_",runname,".rda")) #sPredict.el
load(file=paste0(output.dir,"Results_CoverageOneperCountry_TestingSet.rda")) #Coverage.OneperCountry.df

## PIT plot ##
pdf(paste0(fig.dir,"PIT_hist_",runname,".pdf"))
par(mfrow=c(2,2),las=1, mgp=c(2,0.5,0), tcl=-0.3, cex.lab=1.2, cex.main=1.5)
breakNO=10

for(n in 1:N){
  ## create vectors ##
  pick.c <- Coverage.OneperCountry.df[["pick.nc"]][n,]
  pick.ac <- Coverage.OneperCountry.df[["pick.anc"]][,n,]
  agecat.c <- agecat.e[pick.c]
  
  percentile.c <- rep(NA, length(pick.c))
  percentile.ac <- matrix(NA, nr=A, nc=C)
  for(c in pick.c){
    c.posi <- which(pick.c == c)
    s.pred <- sPredict.el[c,]
    percentile.c[c.posi] <- mean(s.pred <= s.e[c])
  }#end of c loop
  for(age in ages.a){
    a <- which(ages.a == age)
    pick.c <- pick.ac[a,]
    pick.c <- pick.c[!is.na(pick.c)]
    for(c in pick.c){
      c.posi <- which(pick.c == c)
      s.pred <- sPredict.el[c,]
      percentile.ac[a,c.posi] <- mean(s.pred <= s.e[c])
    }#end of c loop
  }#end of age loop
  
  hist(percentile.c, ylab="Density", freq=FALSE,col="darkseagreen2",border="darkslategray",
       main=c("all age groups",paste0("run ",n)), xlab="Pr(prediction <= leftout)", breaks=breakNO)
  abline(v=median(percentile.c), lwd=3, col="darkgreen")
  
  for(age in ages.a){
    a <- which(ages.a == age)
    percentile.c <- percentile.ac[a,!is.na(percentile.ac[a,])]
    hist(percentile.c, ylab="Density", freq=FALSE, col="darkseagreen2",border="darkslategray",
         main=paste0("age group ",a), xlab="Pr(prediction <= leftout)", breaks=breakNO)
    abline(v=median(percentile.c), lwd=3, col="darkgreen")  
  }#end of age loop  
}#end of n loop
dev.off()



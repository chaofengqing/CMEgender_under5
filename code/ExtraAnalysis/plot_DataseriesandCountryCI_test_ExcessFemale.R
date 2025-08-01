
countryCI.dir <- paste0(output.dir,"countryCI/")
countryCI2012.dir <- paste0(countryCI.dir,"/2012/")
countryCI2015.dir <-"C:/Users/FQ/Desktop/"  #paste0(countryCI.dir,"/2015/")

country <- c(1:6, 27) #(0:(C/13-1))*13+1
yearSex

pdf(paste0(fig.dir,"test_CIs",runname,"_ExcessFemale.pdf"), height = 18, width = 20)
par(mfrow = c(3,3), cex.lab = 2.5, cex.axis = 2, 
    mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))
for(c in country){
  load(file = paste0(countryCI2015.dir,"cis_",runname,"_",iso.c[c],"_full.rda"))#res.c.full
  res <- res.c.full
  
  for (age in ages.a){
    expQf.qt <- apply(res[[paste0("expQ",age,"f.jt")]],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(expQf.qt))==1,-1,min(expQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(expQf.qt))==1, 1,max(expQf.qt, na.rm=TRUE))),
                             CI1s=expQf.qt,x=yearSex,
                             ylab=paste0("expected Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
    
    expQf.qt <- apply(res[[paste0("expQ",age,"f.jt")]],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.xlimFix=TRUE, if.NewPlot=FALSE, 
                             datalim=c(ifelse(prod(is.na(expQf.qt))==1,-1,min(expQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(expQf.qt))==1, 1,max(expQf.qt, na.rm=TRUE))),
                             CI1s=expQf.qt,x=yearSex,colCI=c("indianred",NULL),cutoff=0)        
  }#end of age loop
  
  for (age in ages.a){
    excQf.qt <- apply(res[[paste0("excQ",age,"f.jt")]],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(excQf.qt))==1,-1,min(excQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excQf.qt))==1, 1,max(excQf.qt, na.rm=TRUE))),
                             CI1s=excQf.qt,x=yearSex,
                             ylab=paste0("excess Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
    
    excQf.qt <- apply(res[[paste0("excQ",age,"f.jt")]],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.xlimFix=TRUE, if.NewPlot=FALSE, 
                             datalim=c(ifelse(prod(is.na(excQf.qt))==1,-1,min(excQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excQf.qt))==1, 1,max(excQf.qt, na.rm=TRUE))),
                             CI1s=excQf.qt,x=yearSex,colCI=c("limegreen",NULL),cutoff=0)        
  }#end of age loop
  
  for (age in ages.a){    
    excDf.qt <- apply(res[[paste0("excD",age,"f.jt")]],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)  
    PlotCIbandwithDataseries(if.xlimFix=TRUE,
                             CI1s=excDf.qt,x=yearSex,
                             datalim=c(ifelse(prod(is.na(excDf.qt))==1,-1,min(excDf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excDf.qt))==1, 1,max(excDf.qt, na.rm=TRUE))),
                             ylab=paste0("excess Death",age,"female"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL))
    
    excDf.qt <- apply(res[[paste0("excD",age,"f.jt")]],2,quantile,probs=percentiles,na.rm=TRUE)  
    PlotCIbandwithDataseries(if.xlimFix=TRUE,if.NewPlot=FALSE,
                             CI1s=excDf.qt,x=yearSex,cutoff=0,
                             datalim=c(ifelse(prod(is.na(excDf.qt))==1,-1,min(excDf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excDf.qt))==1, 1,max(excDf.qt, na.rm=TRUE))),
                             colCI=c("royalblue",NULL))  
  }#end of age loop
  
  title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
    
}#end of c loop
dev.off()

########################
## check Burkina Faso ##
c <- which(name.c == "Burkina Faso")
load(file = paste0(countryCI2015.dir,"cis_",runname,"_",iso.c[c],"_2015.rda"))#res.c.full
res <- res.c.full

pdf(paste0(fig.dir,"test_CIs",runname,"_ExpectedMale_",name.c[c],".pdf"), height = 6, width = 20)
par(mfrow = c(1,3), cex.lab = 2.5, cex.axis = 2, 
    mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))
for (age in ages.a){
  Qm.qt <- apply(res[[paste0("Q",age,"m.jt")]],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
  PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                           datalim=c(ifelse(prod(is.na(Qm.qt))==1,-1,min(Qm.qt, na.rm=TRUE)),
                                     ifelse(prod(is.na(Qm.qt))==1, 1,max(Qm.qt, na.rm=TRUE))),
                           CI1s=Qm.qt,x=years,
                           ylab=paste0("estimated Q",age,"male *1000"),
                           xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
  Qm.qt <- apply(res[[paste0("Q",age,"m.jt")]],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
  PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,if.NewPlot=FALSE,
                           datalim=c(ifelse(prod(is.na(Qm.qt))==1,-1,min(Qm.qt, na.rm=TRUE)),
                                     ifelse(prod(is.na(Qm.qt))==1, 1,max(Qm.qt, na.rm=TRUE))),
                           CI1s=Qm.qt,x=years,cutoff=0,colCI=c("blue",NULL))   
}#end of age loop
title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
dev.off()

age=4
expQf.qt <- apply(res[[paste0("expQ",age,"f.jt")]],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000
expQf.qt
expQf.j <- res[[paste0("expQ",age,"f.jt")]][,Tend]
Qm.j <- res[[paste0("Q",age,"m.jt")]][,Tend]
Qm.j[which(expQf.j == 1)]
Qm.j[which(Qm.j < 0)]
expQf.j[which(Qm.j < 0)]
S.j <- res[["S4.lt"]][selectl.j,Tend]
S.j[which(expQf.j == 1)]
w4.j <- GetWeight4(w1 = w1, q1male = Qm.j, q1both = Q1.ctj[c,Tend,])
q4.j <- GetQ4fromQ15(q1 = Q1.ctj[c,Tend,], q5 = Q5.ctj[c,Tend,])
w4.j[which(expQf.j == 1)]
q4.j[which(expQf.j == 1)]
q4.j[which(Qm.j < 0)]
Q1.ctj[c,Tend,which(Qm.j < 0)]
Q5.ctj[c,Tend,which(Qm.j < 0)]


#####################
## check Australia ##
country <- which(name.c == "Australia")
plot.yr <- c(2009.5, 2010.5)
pdf(paste0(fig.dir,"test_CIs",runname,"_ExcessFemale_",name.c[country],".pdf"), height = 18, width = 20)
par(mfrow = c(3,3), cex.lab = 2.5, cex.axis = 2, 
    mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))

for(c in country){
  load(file = paste0(countryCI2015.dir,"cis_",runname,"_",iso.c[c],"_2015.rda"))#res.c.full
  res <- res.c.full
  
  ## estimated Qmale ##
  for (age in ages.a){
    Qm.qt <- apply(res[[paste0("Q",age,"m.jt")]][,is.element(years,yearSex)],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(Qm.qt))==1,-1,min(Qm.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(Qm.qt))==1, 1,max(Qm.qt, na.rm=TRUE))),
                             CI1s=Qm.qt,x=yearSex, seq.years=yearSex,
                             ylab=paste0("estimated Q",age,"male *1000"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
    Qm.qt <- apply(res[[paste0("Q",age,"m.jt")]][,is.element(years,yearSex)],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,if.NewPlot=FALSE,
                             datalim=c(ifelse(prod(is.na(Qm.qt))==1,-1,min(Qm.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(Qm.qt))==1, 1,max(Qm.qt, na.rm=TRUE))),
                             CI1s=Qm.qt,x=yearSex,seq.years=yearSex,cutoff=0,colCI=c("blue",NULL))   
    abline(v=plot.yr, col="gray")
  }#end of age loop
  
  ## expected Qfemale ##
  for (age in ages.a){
    expQf.qt <- apply(res[[paste0("expQ",age,"f.jt")]][,is.element(years,yearSex)],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(expQf.qt))==1,-1,min(expQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(expQf.qt))==1, 1,max(expQf.qt, na.rm=TRUE))),
                             CI1s=expQf.qt,x=yearSex,seq.years=yearSex,
                             ylab=paste0("expected Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
    
    expQf.qt <- apply(res[[paste0("expQ",age,"f.jt")]][,is.element(years,yearSex)],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.NewPlot=FALSE, 
                             datalim=c(ifelse(prod(is.na(expQf.qt))==1,-1,min(expQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(expQf.qt))==1, 1,max(expQf.qt, na.rm=TRUE))),
                             CI1s=expQf.qt,x=yearSex,seq.years=yearSex,colCI=c("indianred",NULL),cutoff=0)        
    abline(v=plot.yr, col="gray")
  }#end of age loop
  
  for (age in ages.a){
    excQf.qt <- apply(res[[paste0("excQ",age,"f.jt")]][,is.element(years,yearSex)],2,quantile,probs=c(0,0.50,1),na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(excQf.qt))==1,-1,min(excQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excQf.qt))==1, 1,max(excQf.qt, na.rm=TRUE))),
                             CI1s=excQf.qt,x=yearSex,seq.years=yearSex,
                             ylab=paste0("excess Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("gray",NULL)) 
    
    excQf.qt <- apply(res[[paste0("excQ",age,"f.jt")]][,is.element(years,yearSex)],2,quantile,probs=percentiles,na.rm=TRUE)*1000  
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.NewPlot=FALSE, 
                             datalim=c(ifelse(prod(is.na(excQf.qt))==1,-1,min(excQf.qt, na.rm=TRUE)),
                                       ifelse(prod(is.na(excQf.qt))==1, 1,max(excQf.qt, na.rm=TRUE))),
                             CI1s=excQf.qt,x=yearSex,seq.years=yearSex,colCI=c("limegreen",NULL),cutoff=0)        
    abline(v=plot.yr, col="gray")
  }#end of age loop
    
  title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
  
}#end of c loop
dev.off()

load(file=paste0(countryCI.dir,"cis_",runname,"_2012.rda")) #res2012
Q1.test     <- res2012[["Q1.cqt"]][ c,2,is.element(years, plot.yr)]*1000    #4.27 4.19
Q1m.test    <- res2012[["Q1m.cqt"]][c,2,is.element(years, plot.yr)]*1000    #4.76 4.69
Q4m.test    <- res2012[["Q4m.cqt"]][c,2,is.element(years, plot.yr)]*1000    #0.91 0.90
expQ4f.test <- res2012[["expQ4f.cqt"]][c,2,is.element(years, plot.yr)]*1000 #0.8  0.7
j = 1
qm <- Q4m.test[j]
wa <- GetWeight4(w1 = w1, q1male = Q1m.test, q1both = Q1.test)[j]          
W.k <- l4.lk[selectl.j[j],] 

## change ##
qf <- seq(0.0001, 1, 0.0001) #0.00001
pick <- rep(NA, length(qf))  

for(f in 1:length(qf)){
  pick[f] <- which.min(abs(gridQtot.k - GetQboth(qm, qf[f],wa)))
}#end of f loop

qexp4F.jt[j,t] <- qexp4F <- qf[which.min(abs(qm/qf - W.k[pick]))]


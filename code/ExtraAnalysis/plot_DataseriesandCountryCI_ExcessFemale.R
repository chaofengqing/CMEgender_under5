
## plot excess female mortality ##
pdf(paste0(fig.dir,"CIs",runname,"_ExcessFemale.pdf"), height = 18, width = 20)
par(mfrow = c(3,3), cex.lab = 2.5, cex.axis = 2, 
    mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))
for (c in 1:C){
  
  ## expected female mortality ##
  for (age in ages){
    expQf.qt <- res[[paste0("expQ",age,"f.cqt")]][c,,]*1000
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(expQf.qt))==1,-1,min(expQf.qt,0, na.rm=TRUE)),
                                       ifelse(prod(is.na(expQf.qt))==1, 1,max(expQf.qt, na.rm=TRUE))),
                             CI1s=expQf.qt,x=yearSex,
                             ylab=paste0("expected Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("indianred",NULL))      
  }#end of age loop
  
  ## excess female mortality ##
  for (age in ages){
    excQf.qt <- res[[paste0("excQ",age,"f.cqt")]][c,,]*1000    
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             datalim=c(ifelse(prod(is.na(excQf.qt))==1,-1,min(excQf.qt,0, na.rm=TRUE)),
                                       ifelse(prod(is.na(excQf.qt))==1, 1,max(excQf.qt, na.rm=TRUE))),
                             CI1s=excQf.qt,x=yearSex,
                             ylab=paste0("excess Q",age,"female *1000"),
                             xlab="Year",cutoff=0,colCI=c("limegreen",NULL))      
  }#end of age loop
  
  ## excess female deaths ##
  for (age in ages){ # remove Q4 here and in list
    excDf.qt <- res[[paste0("excD",age,"f.cqt")]][c,,]    
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             CI1s=excDf.qt,x=yearSex,
                             datalim=c(ifelse(prod(is.na(excDf.qt))==1,-1,min(excDf.qt,0, na.rm=TRUE)),
                                       ifelse(prod(is.na(excDf.qt))==1, 1,max(excDf.qt, na.rm=TRUE))),
                             ylab=paste0("excess Death",age,"female"),
                             xlab="Year",cutoff=0,colCI=c("royalblue",NULL))
  }#end of age loop
  
  title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
}#end of c loop
dev.off()


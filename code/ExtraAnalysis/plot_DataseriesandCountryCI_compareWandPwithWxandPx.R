
#########################################
## plot Wx and Px compare with W and P ##
pdf(paste0(fig.dir,"CIs",runname,"_compareWandPwithWxandPx.pdf"), height = 15, width = 18)
par(mfrow = c(3,3), cex.lab = 2.5, cex.axis = 2, 
    mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))

for (c in 1:C){

  ## Q total ##
  for (age in ages){    
    Q.qt <- res[[paste0("Q",age,".cqt")]][c,,is.element(years,yearSex)]*1000
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                             CI1s=Q.qt, x=yearSex,seq.years=yearSex,
                             ylab=paste0("Q",age,"total*1000"),datalim=range(Q.qt,na.rm=TRUE),
                             xlab="Year",cutoff=0,colCI=c("indianred2"))      
  }#end of age loop  
  
  ## Wx and W ##
  for (age in ages){
    Wx.qt <- res[[paste0("Wx",age,".cqt")]][c,,is.element(years,yearSex)]
    W.qt <- res[[paste0("W",age,".cqt")]][c,,is.element(years,yearSex)]    
    PlotCIbandwithDataseries(if.CI1s=TRUE,if.CI2s=TRUE,
                             if.xlimFix=TRUE,
                             datalim=c(1,max(W.qt,Wx.qt,na.rm=TRUE)),
                             CI1s=Wx.qt,CI2s=W.qt,colCI=c("darkgreen","orchid"),
                             x=yearSex,seq.years=yearSex,
                             if.CILegend=TRUE,nameCI1="Wx",nameCI2="W",
                             ylab=paste0("W",age),xlab="Year",cutoff=1)
  }#end of age loop
  
  ## Px and P ##
  for (age in ages){    
    Px.qt <- res[[paste0("Px",age,".cqt")]][c,,is.element(years,yearSex)]   
    P.qt <- res[[paste0("P",age,".cqt")]][c,,is.element(years,yearSex)]   
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.CI2s=TRUE, if.xlimFix=TRUE,
                             CI1s=Px.qt,CI2s=P.qt,x=yearSex,seq.years=yearSex,
                             ylab=paste0("P",age),datalim=c(min(P.qt,Px.qt,1,na.rm=TRUE),max(P.qt,Px.qt,1,na.rm=TRUE)),
                             if.CILegend=TRUE,nameCI1="Px",nameCI2="P",
                             xlab="Year",cutoff=c(0,1),colCI=c("darkgreen","orchid"))      
  }#end of age loop  
  
  title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
}#end of c loop
dev.off()


# ## plot Australia to explore the fluctuation at low Q ##
# pdf(paste0(fig.dir,"CIs",runname,"_compareWandPwithWxandPx_Australia.pdf"), height = 15, width = 18)
# par(mfrow = c(3,3), cex.lab = 2.5, cex.axis = 2, 
#     mar = c(4.5,6,3,1), oma=c(0.2,2.5,3,3), mgp=c(3.5,1,0))
# c <- which(name.c == "Australia")
# plot.yr <- c(2009.5, 2010.5)
# ## Q total ##
# for (age in ages){    
#   Q.qt <- res[[paste0("Q",age,".cqt")]][c,,is.element(years,yearSex)]*1000
#   PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
#                            CI1s=Q.qt, x=yearSex,seq.years=yearSex,
#                            ylab=paste0("Q",age,"total*1000"),datalim=range(Q.qt,na.rm=TRUE),
#                            xlab="Year",cutoff=0,colCI=c("indianred2")) 
#   abline(v=plot.yr, col="gray")
# }#end of age loop  
# ## Wx and W ##
# for (age in ages){
#   W.qt <- res[[paste0("W",age,".cqt")]][c,,is.element(years,yearSex)]
#   Wx.qt <- res[[paste0("Wx",age,".cqt")]][c,,is.element(years,yearSex)]  
#   PlotCIbandwithDataseries(if.CI1s=TRUE,if.CI2s=TRUE,
#                            if.xlimFix=TRUE,
#                            datalim=c(1,max(W.qt,Wx.qt,na.rm=TRUE)),
#                            CI1s=Wx.qt,CI2s=W.qt,colCI=c("darkgreen","orchid"),
#                            x=yearSex,seq.years=yearSex,
#                            if.CILegend=TRUE,nameCI1="Wx",nameCI2="W",
#                            ylab=paste0("W",age),xlab="Year",cutoff=1)
#   abline(v=plot.yr, col="gray")
# }#end of age loop
# ## Px and P ##
# for (age in ages){    
#   P.qt <- res[[paste0("P",age,".cqt")]][c,,is.element(years,yearSex)]   
#   Px.qt <- res[[paste0("Px",age,".cqt")]][c,,is.element(years,yearSex)]   
#   PlotCIbandwithDataseries(if.CI1s=TRUE, if.CI2s=TRUE, if.xlimFix=TRUE,
#                            CI1s=Px.qt,CI2s=P.qt,x=yearSex,seq.years=yearSex,
#                            ylab=paste0("P",age),datalim=c(min(P.qt,Px.qt,1,na.rm=TRUE),max(P.qt,Px.qt,1,na.rm=TRUE)),
#                            if.CILegend=TRUE,nameCI1="Px",nameCI2="P",
#                            xlab="Year",cutoff=c(0,1),colCI=c("darkgreen","orchid"))      
#   abline(v=plot.yr, col="gray")
# }#end of age loop  
# title(main=name.c[c],line=-1,cex.main=4.5,outer=TRUE)
# dev.off()


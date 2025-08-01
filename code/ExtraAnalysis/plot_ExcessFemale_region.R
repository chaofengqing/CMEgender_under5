
## total deaths in the world and by region against time ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
##################
## normal scale ##
## combined plot ##
pdf(paste0(fig.dir,"ExcessFemale_",runname,"_region_normalscale.pdf"), height = 16, width = 20)
par(mfrow = c(2,3), cex.lab = 3.5, cex.axis = 3, cex.main = 3.5, mgp=c(7,1.5,0), mar=c(2,11,5,2)) #for combined plot
for(age in ages){  
  ## global ##
  excDf.qt <- res[[paste0("excD",age,"f.qt")]]
  ## global ##
  excDf.qt <- res[[paste0("excD",age,"f.qt")]]
  PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.rescaleYaxis=TRUE,
                           CI1s=excDf.qt,x=yearSex, datalim=range(-5*10^4,5*10^5),
                           main=paste0("age group ",age),ylab="", xlab="",#ylab="Excess Female Deaths", xlab="Year", 
                           cutoff=0,colCI=c("black",NULL))    
  labelDeath <- c(-5*10^4,0,seq(1,5)*10^5)
  plotlabelDeath    <- c(expression(-5%*%10^4),0,expression(10^5),expression(2%*%10^5),
                         expression(3%*%10^5),expression(4%*%10^5),expression(5%*%10^5))
  axis(2,at = labelDeath, label = plotlabelDeath, las = 1)
  abline(h=labelDeath,col="lightgray",lwd=0.5)
  ## region ##
  collegend <- legendnames <- NULL
  for (r in 1:R){
    excDf.qt <- res[[paste0("excD",age,"f.rqt")]][r,,]
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,if.NewPlot=FALSE,
                             CI1s=excDf.qt,x=yearSex,
                             cutoff=0,colCI=c(colchart[r],NULL))          
    legendnames <- c(legendnames, regions[r])
    collegend <- c(collegend,colchart[r])
  }#end of r loop
  abline(h=0, lwd=3) 
}#end of a loop
par(mar=c(0,1,0,0))
plot(1,type="n", xlab=NA,ylab=NA, xaxt="n",yaxt="n",bty="n")
legend("left", legend = c("Global",legendnames), col = c("black",collegend), cex =3,lwd = 10, lty = 1)  
dev.off()

## seperate plot ##
pdf(paste0(fig.dir,"ExcessFemale_",runname,"_region_normalscale_full.pdf"), height = 25, width = 20)
par(mfrow = c(4,3), cex.lab = 3, cex.axis = 2.8, cex.main = 3, mgp=c(4.5,1.5,0), mar=c(6,7,5,2),oma=c(1,1,4,1)) #for full plot
for(age in ages){
  
  ## global ##
  excDf.qt <- res[[paste0("excD",age,"f.qt")]]
  PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE,
                           CI1s=excDf.qt,x=yearSex,
                           ylab="Excess Female Deaths", xlab="Year", main="Global",
                           cutoff=0,colCI=c("black",NULL))  
  ## region ##
  collegend <- legendnames <- NULL
  for (r in 1:R){
    excDf.qt <- res[[paste0("excD",age,"f.rqt")]][r,,]
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, 
                             CI1s=excDf.qt,x=yearSex,
                             ylab="", xlab="Year", main=regions[r],
                             cutoff=0,colCI=c(colchart[r],NULL))  
    
    legendnames <- c(legendnames, regions[r])
    collegend <- c(collegend,colchart[r])
  }#end of r loop
  abline(h=0, lwd=3) 
  plot(1,type="n", xlab=NA,ylab=NA, xaxt="n",yaxt="n",bty="n")
  title(main=paste0("age group ",age),outer=TRUE,liner=-0.5,cex.main=3.5)
}#end of a loop
dev.off()


###############################################################

## log scale ##

## seperate plot ##
pdf(paste0(fig.dir,"ExcessFemale_",runname,"_region_log10scale_full.pdf"), height = 25, width = 22)
par(mfrow = c(4,3), cex.lab = 3, cex.axis = 2.8, cex.main = 3, mgp=c(4.5,1.5,0), mar=c(6,10,5,2),oma=c(1,1,4,1)) #for full plot

for(age in ages){

  ## global ##
  excDf.qt <- res[[paste0("excD",age,"f.qt")]]
  ## log-scale ##
  sign.qt <- (!(excDf.qt>0 | is.na(excDf.qt)))*(-1) +  (excDf.qt>0 | is.na(excDf.qt))*1#identify neg and posi sign
  LOG10excDf.qt <- log10(abs(excDf.qt))*sign.qt
  
  PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.rescaleYaxis=TRUE,
                           CI1s=LOG10excDf.qt,x=yearSex, datalim=range(log10(10^4),log10(10^6)),
                           ylab="", xlab="Year", main="Global",
                           cutoff=0,colCI=c("black",NULL))  
  LOG10labelDeath <- c(4,log10(5)+4,5,log10(5)+5,6)
  labelDeath    <- 10^LOG10labelDeath
  axis(2,at = LOG10labelDeath, label = labelDeath, las = 1)
  abline(h=LOG10labelDeath,col="lightgrey",lty=1,lwd=0.5)  
  
  ## region ##
  collegend <- legendnames <- NULL
  for (r in 1:R){
    excDf.qt <- res[[paste0("excD",age,"f.rqt")]][r,,]
    ## log-scale ##
    sign.qt <- (!(excDf.qt>0 | is.na(excDf.qt)))*(-1) +  (excDf.qt>0 | is.na(excDf.qt))*1#identify neg and posi sign
    LOG10excDf.qt <- log10(abs(excDf.qt))*sign.qt
    
    PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.rescaleYaxis=TRUE,
                             CI1s=LOG10excDf.qt,x=yearSex, datalim=range(-1*log10(10^5),log10(10^6)),
                             ylab="", xlab="Year", main=regions[r],
                             cutoff=0,colCI=c(colchart[r],NULL))  
    labelDeath    <- c(-1*10^seq(5,1),0,10^seq(1,6))
    LOG10labelDeath <- c(log10(abs(labelDeath[1:5]))*(-1),0,log10(abs(labelDeath[7:12])))
    axis(2,at = LOG10labelDeath, label = labelDeath, las = 1)
    abline(h=LOG10labelDeath[-6],col="lightgrey",lty=1)  
    
    legendnames <- c(legendnames, regions[r])
    collegend <- c(collegend,colchart[r])
  }#end of r loop
  abline(h=0, lwd=3) 
  plot(1,type="n", xlab=NA,ylab=NA, xaxt="n",yaxt="n",bty="n")
  title(main=paste0("age group ",age),outer=TRUE,liner=-0.5,cex.main=3.5)
}#end of a loop

## global for age group 5 ##
par(mfrow=c(1,1),mar=c(6,10,10,2))
excDf.qt <- res[[paste0("excD5f.qt")]]
## log-scale ##
sign.qt <- (!(excDf.qt>0 | is.na(excDf.qt)))*(-1) +  (excDf.qt>0 | is.na(excDf.qt))*1#identify neg and posi sign
LOG10excDf.qt <- log10(abs(excDf.qt))*sign.qt

PlotCIbandwithDataseries(if.CI1s=TRUE, if.xlimFix=TRUE, if.rescaleYaxis=TRUE,
                         CI1s=LOG10excDf.qt,x=yearSex, datalim=range(log10(10^5),log10(10^6)),
                         ylab="", xlab="Year", main="Global age group 5",
                         cutoff=0,colCI=c("black",NULL))  
LOG10labelDeath <- c(5,log10(2)+5,log10(4)+5,log10(6)+5,
                     log10(8)+5,6)
labelDeath    <- c(expression(10^5),expression(2%*%10^5),expression(4%*%10^5),
                   expression(6%*%10^5),expression(8%*%10^5),expression(10^6))
axis(2,at = LOG10labelDeath, label = labelDeath, las = 1)
abline(h=LOG10labelDeath,col="lightgrey",lty=1,lwd=0.5)  

dev.off()


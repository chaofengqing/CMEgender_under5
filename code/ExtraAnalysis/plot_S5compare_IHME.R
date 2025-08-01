
## S5 or W5 (world & regional) comparison plot between IHME and IGME ##

tmp <- read.csv(paste0(output.dir, "Results_SexRatioIGMEandIHME.csv"))

###################
## S5 by regions ##
pdf(paste0(fig.dir, "S5", runname, "log_IHMEregion(90loess).pdf"), 
    height = 12, width = 18)
par(mfrow = c(1,2), cex.lab = 2.5, cex.axis = 2, cex.main=2.5, mar = c(5,5,4,1), oma=c(1,1,4,1),
    mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/1000*labelu)

#####################
## IHME S5 vs U5MR ##
## s5 and log(Q5) ##
plot(x=log(tmp[,"U5MR.IHME"]),y=tmp[,"SexRatio.IHME"],type="n",xlim = log(c(300/1000,0.003)), ylim = c(0.7,1.5),
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n",main="IHME")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)
points(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), pch=19, cex=0.5, col="lightgrey")

## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
regions <- sort(unique(reg.c))
col=0
for (reg in regions){
  col <- col+1
  select.r <- which(tmp[,"Region.Name"]==reg)
  xtemp <- log(c(tmp[select.r,"U5MR.IHME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(c(tmp[select.r,"SexRatio.IHME"])~ log(c(tmp[select.r,"U5MR.IHME"])), na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, reg)
  collegend <- c(collegend,colchart[col])
  if(reg=="Eastern Asia") points(c(tmp[select.r,"SexRatio.IHME"])~ log(c(tmp[select.r,"U5MR.IHME"])), pch=19, col=colchart[col])
}
legend("bottomright", legend = legendnames, col = collegend, cex =1.5,lwd = 7, lty = 1)

## global ##
xtemp<-log(c(tmp[,"U5MR.IHME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 9 ,from=Q[1],to=Q[2])
legend("bottomleft",c("Global"), col="purple",lty=1, lwd=9, cex=1.7)

#####################
## IGME S5 vs U5MR ##
plot(x=log(tmp[,"U5MR.IGME"]),y=tmp[,"SexRatio.IGME"],type="n",xlim = log(c(300/1000,0.003)), ylim = c(0.7,1.5),
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n",main="IGME")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)

points(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), pch=19, cex=0.5, col="lightgrey")
## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
regions <- sort(unique(reg.c))
col=0
for (reg in regions){
  col <- col+1
  select.r <- which(tmp[,"Region.Name"]==reg)
  xtemp <- log(c(tmp[select.r,"U5MR.IGME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(c(tmp[select.r,"SexRatio.IGME"])~ log(c(tmp[select.r,"U5MR.IGME"])), na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, reg)
  collegend <- c(collegend,colchart[col])
  if(reg=="Eastern Asia") {
    points(c(tmp[select.r,"SexRatio.IGME"])~ log(c(tmp[select.r,"U5MR.IGME"])), pch=19, col=colchart[col])
#     select.c <- which(!is.element(tmp[select.r,"Country.Name"],"Mongolia")) #"Korea DPR"
#     select.rc <- select.r[select.c]
#     
#     xtemp <- log(c(tmp[select.rc,"U5MR.IGME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
#     
#     curve(predict(loess(c(tmp[select.rc,"SexRatio.IGME"])~ log(c(tmp[select.rc,"U5MR.IGME"])), na.action = "na.omit",family = "symmetric")
#                   ,x), add= T, col = "hotpink3", lwd = 5, lty=1 ,from=Q[1],to=Q[2])
#     points(c(tmp[select.rc,"SexRatio.IGME"])~ log(c(tmp[select.rc,"U5MR.IGME"])), pch=19, col="hotpink3")
  } 
}
legend("bottomright", legend = legendnames, col = collegend, cex =1.5,lwd = 7, lty = 1)

## global ##
xtemp<-log(c(tmp[,"U5MR.IGME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 9 ,from=Q[1],to=Q[2])
legend("bottomleft",c("Global"), col="purple",lty=1, lwd=9, cex=1.7)

title(main="S5 by regions (90% loess)",line=1,cex.main=3, outer=TRUE)
dev.off()




####################################
## S5 global only: s5 and log(Q5) ##
pdf(paste0(fig.dir,"S5",runname,"log_IHMEglobal(90loess).pdf"), height = 12, width = 9)
par(cex.lab = 2.5, cex.axis = 2, cex.main=2.5, mar = c(5,5,4,1),
    mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/1000*labelu)
range(c(tmp[,c("SexRatio.IHME","SexRatio.IGME")]))
## IHME S5 vs U5MR ##
plot(x=log(tmp[,"U5MR.IHME"]),y=tmp[,"SexRatio.IHME"],type="n",xlim = log(c(300/1000,0.003)), ylim = c(0.9,1.5),
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n",main="S5 (90% loess)")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)
points(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), pch=19, cex=0.5, col="steelblue")
xtemp<-log(c(tmp[,"U5MR.IHME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "royalblue", lwd = 9 ,from=Q[1],to=Q[2])
## IGME S5 vs U5MR ##
points(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), pch=19, cex=0.5, col="thistle")
xtemp<-log(c(tmp[,"U5MR.IGME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 9 ,from=Q[1],to=Q[2])
legend("bottomright",c("IGME","IHME"), col=c("purple","royalblue"),lty=1, lwd=9, cex=1.7)

dev.off()

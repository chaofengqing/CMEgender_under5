
## plot s1/4/5 ~ log(Q1/4/5) and CI for W1/4/5 ##
xlim = c(300/1000,0.003) 
ylim = c(0.4, 2)

#######################
## normal scale of Q ##
pdf(paste0(fig.dir,"W1and4and5",runname,".pdf"), height = 9, width = 21)
par(mfrow = c(1,3), cex.lab = 3, cex.axis = 2.5, mar = c(7,7,1,1), cex.main = 3, mgp=c(4.3,1.5,0))
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- c(1/1000*labelu)
## s1 and Q1 ##
PlotCIandLoess (x=Q1.i[agecat.i==1],y=s.i[agecat.i==1],labelx=labelu,labelxlocation=labellogu,
                xlab="Q1*1000", ylab="S1", xlim = xlim, ylim = ylim, 
                if.redoXaix=TRUE,if.loess=FALSE,if.CI=TRUE,cutoff=1,CI=res$ci1.qk,CIx =exp(res$logQ1.k))
## s4 and Q4 ##
PlotCIandLoess (x=Q4.i[agecat.i==4],y=s.i[agecat.i==4],labelx=labelu,labelxlocation=labellogu,
                xlab="Q4*1000",ylab="S4", xlim = xlim, ylim = ylim, 
                if.redoXaix=TRUE,if.loess=FALSE,if.CI=TRUE,cutoff=1,CI=res$ci4.qk,CIx =exp(res$logQ4.k))
## s5 and Q5 ##
plot(x=Q5.i[agecat.i==5],y=s.i[agecat.i==5],type="n",xlim = xlim, ylim = ylim,
     xlab="Q5*1000",ylab="S5",xaxt="n")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1)
curve(predict(loess(c(res$W5.cqt[,2,])~ c(Q5.ct), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 7)

dev.off()


###################
## W5 by regions ##
pdf(paste0(fig.dir,"W5",runname,"log_region(90loess).pdf"), height = 9, width = 9)
par(mfrow = c(1,1), cex.lab = 2.5, cex.axis = 2, mar = c(5,5,4,1), cex.main = 3, mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/1000*labelu)
## s5 and log(Q5) ##
plot(x=log(Q5.i[agecat.i==5]),y=s.i[agecat.i==5],type="n",xlim = log(xlim), ylim = c(0.7,1.4),
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n",main="W5 by regions 90% loess")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)

## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
regions <- sort(unique(reg.c))
col=0
for (reg in regions){
  col <- col+1
  select.c <- which(reg.c==reg)
  xtemp<-log(c(Q5.ct[select.c,])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(c(res$W5.cqt[select.c,2,])~ log(c(Q5.ct[select.c,])), na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, reg)
  collegend <- c(collegend,colchart[col])
}
legend("bottomright", legend = legendnames, col = collegend, cex =1.2,lwd = 7, lty = 1)
abline(v=log(30/1000),col="grey", lty=2)
## global ##
xtemp<-log(c(Q5.ct[,])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(res$W5.cqt[,2,])~ log(c(Q5.ct)), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 9 ,from=Q[1],to=Q[2])
legend("bottomleft",c("Global"), col="purple",lty=1, lwd=9, cex=1.5)

dev.off()



###################
## S5 by regions ##
pdf(paste0(fig.dir,"S5",runname,"log_region(90loess).pdf"), height = 12, width = 18)
par(mfrow=c(1,2), cex.lab = 2.5, cex.axis = 2, cex.main=2.5, mar = c(5,5,4,1),
    mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/1000*labelu)

plot(x=log(c(Q5.ct)),y=c(res$S5.cqt[,2,]),type="n",
     xlim = log(c(300/1000,0.003)), ylim = range(c(res$S5.cqt[,2,]),na.rm=TRUE),
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n", main="S5 by regions (90% loess)")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)

points(c(res$S5.cqt[,2,]) ~ log(c(Q5.ct)), pch=19, cex=0.5, col="lightgrey")
## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
regions <- sort(unique(reg.c))
col=0
text.country <- c("India","Mongolia")

for (reg in regions){
  col <- col+1
  select.r <- which(reg.c==reg)
  xtemp <- log(c(Q5.ct[select.r,])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(c(res$S5.cqt[select.r,2,]) ~ log(c(Q5.ct[select.r,])), na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, reg)
  collegend <- c(collegend,colchart[col])
#   if(reg=="Eastern Asia" | reg=="Southern Asia") {
  for(text.c in text.country){
    select.rc <- select.r[name.c[select.r]==text.c]
      points(c(res$S5.cqt[select.rc,2,]) ~ log(c(Q5.ct[select.rc,])), 
             pch=19, cex=0.5, col=colchart[col])
  }#end of text.c loop
#   }#end of if 
}#end of reg loop

## global ##
xtemp<-log(c(Q5.ct)); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(res$S5.cqt[,2,]) ~ log(c(Q5.ct)), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = 12, from=Q[1],to=Q[2])
legend("bottomright",c("Global"), col="purple",lty=1, lwd=12, cex=2)

## identify outlying country ##
for(text.c in text.country){
  text(x=log(c(Q5.ct[is.element(name.c,text.c),which(years==2012.5)])), 
       y=c(res$S5.cqt[is.element(name.c,text.c),2,which(years==2012.5)]), 
       labels=text.c, pos=4,cex=1.5)
}#end of text.c loop

plot(1,type="n", xlab=NA,ylab=NA, xaxt="n",yaxt="n",bty="n")
legend("left", legend = legendnames, col = collegend, cex =1.7,lwd = 10, lty = 1)

dev.off()


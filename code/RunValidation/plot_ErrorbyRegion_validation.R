
## plot validation results based on source type ##
test.df <- read.csv(file=paste0(output.dir,"Results_ErrorRelativeError_TestingSet.csv"), stringsAsFactors=FALSE)
error.e  <- test.df$Error 
Q5.e     <- test.df$Q5.i
reg.e    <- test.df$reg.i
source.e <- test.df$typename.i

## plot non-extreme errors only!
NOextreme <- abs(error.e)<100
error.e  <- error.e[NOextreme]
Q5.e     <- Q5.e[NOextreme]
reg.e    <- reg.e[NOextreme]
source.e <- source.e[NOextreme]

######################################
## error by regions against log(Q5) ##
pdf(paste0(fig.dir,"Error",runname,"log_ValidationRegion(90loess).pdf"), height = 12, width = 9)
par(mfrow = c(1,1), cex.lab = 2.5, cex.axis = 2, cex.main=2.5, mar = c(5,5,4,1), oma=c(1,1,1,1),
    mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/Qunit*labelu)

plot(x=log(Q5.e),y=error.e,type="n", xlim = log(c(300/Qunit,3/Qunit)), ylim = range(error.e),
     xlab="Q5*1000 (log-scale)", ylab="Error" ,xaxt="n",main="Error by regions")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=0,lwd=3)
points(error.e ~ log(Q5.e), pch=19, cex=0.5, col="lightgrey")

## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
# regions <- sort(unique(reg.c))
col=0
for (reg in regions.r){
  col <- col+1
  select.r <- which(reg.e==reg)
  xtemp <- log(Q5.e[select.r]); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(error.e[select.r] ~ xtemp, na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, reg)
  collegend <- c(collegend,colchart[col])
 if(reg=="Caucasus and Central Asia" | reg=="South-eastern Asia") {
   points(error.e[select.r] ~ xtemp, pch=19, col=colchart[col], cex=0.7)
 }
}#end of reg loop
# plot(1, type="n",xlab=NA, ylab=NA, xaxt="n", yaxt="n", bty="n")
legend("topleft", legend = legendnames, col = collegend, cex =1.5,lwd = 7, lty = 1)

dev.off()

######################################
## error by sources against log(Q5) ##
pdf(paste0(fig.dir,"Error",runname,"log_ValidationSource(90loess).pdf"), height = 12, width = 9)
par(mfrow = c(1,1), cex.lab = 2.5, cex.axis = 2, cex.main=2.5, mar = c(5,5,4,1), oma=c(1,1,1,1),
    mgp=c(3.2,0.8,0),las=1,tcl=-0.2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/Qunit*labelu)

plot(x=log(Q5.e),y=error.e,type="n",xlim = log(c(300/Qunit,3/Qunit)), ylim = range(error.e),
     xlab="Q5*1000 (log-scale)",ylab="Error", xaxt="n",main="Error by sources")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=0,lwd=3)
points(error.e ~ log(Q5.e), pch=19, cex=0.5, col="lightgrey")

## region ##
colchart <- c("darkorange1","goldenrod3","hotpink2","seagreen3","royalblue",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
collegend <- legendnames <- NULL
sources <- sort(unique(source.e))
col=0
for (sou in sources){
  col <- col+1
  select.s <- which(source.e==sou)
  xtemp <- log(Q5.e[select.s]); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
  
  curve(predict(loess(error.e[select.s] ~ xtemp, na.action = "na.omit",family = "symmetric")
                ,x), add= T, col = colchart[col], lwd = 5, lty=1 ,from=Q[1],to=Q[2])
  legendnames <- c(legendnames, sou)
  collegend <- c(collegend,colchart[col])
  if(sou=="Others Indirect" | sou=="Census Indirect") {
    points(error.e[select.s]~ xtemp, pch=19, col=colchart[col], cex=0.7)
  }

}#end of sou loop
#plot(1, type="n",xlab=NA, ylab=NA, xaxt="n", yaxt="n", bty="n")
legend("topleft", legend = legendnames, col = collegend, cex =1.5,lwd = 7, lty = 1)

dev.off()

## the end ##

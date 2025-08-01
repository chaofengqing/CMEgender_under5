
## plot S5 and W5 combined across studies ##
xlim = c(300/1000,0.003) #c(0.2,0.003)
ylim = c(0.9, 1.3) #c(0.4, 2)
labelu <- c(1, 5, 10, 20,50,100, 150, 300)
labellogu <- log(1/1000*labelu)
colchart <- c("darkorange1","seagreen3","royalblue","indianred2",
              "yellowgreen","springgreen4","salmon","lightgoldenrod4","brown1")
lwd = 15:10

pdf(paste0(fig.dir,"S5W5",runname,"log_allResults.pdf"), height = 12, width = 9)
par(cex.lab = 3, cex.axis = 2.5, mar = c(7,7,1,1), cex.main = 3, mgp=c(4.3,1.5,0))
########
## W5 ##
plot(x=log(Q5.i[agecat.i==5]),y=s.i[agecat.i==5],type="n",xlim = log(xlim), ylim = ylim,
     xlab="Q5*1000 (log-scale)",ylab="S5",xaxt="n")
axis(1,at = labellogu, label = labelu, las = 1)
abline(h=1,lwd=3)
xtemp<-log(c(Q5.ct)); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(res$W5.cqt[,2,])~ log(c(Q5.ct)), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = "purple", lwd = lwd[1], from=Q[1],to=Q[2])
##########################
## S5 all country-years ##
xtemp<-log(c(Q5.ct)); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(res$S5.cqt[,2,]) ~ log(c(Q5.ct)), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col = colchart[1], lwd = lwd[2], from=Q[1],to=Q[2])

## read in data for comparable country-years for IGME and IHME S5 ##
tmp <- read.csv(paste0(output.dir,"Results_SexRatioIGMEandIHME.csv"))
#################################
## S5 comparable country-years ##
# points(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), pch=19, cex=0.5, col="darkolivegreen3")
xtemp<-log(c(tmp[,"U5MR.IGME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IGME"])~ log(c(tmp[,"U5MR.IGME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col=colchart[2], lwd = lwd[3], from=Q[1],to=Q[2])
######################################
## IHME S5 comparable country-years ##
# points(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), pch=19, cex=0.5, col="steelblue")
xtemp<-log(c(tmp[,"U5MR.IHME"])); Q<-quantile(xtemp, probs=c(0.05,0.95),na.rm=T)
curve(predict(loess(c(tmp[,"SexRatio.IHME"])~ log(c(tmp[,"U5MR.IHME"])), na.action = "na.omit",family = "symmetric")
              ,x), add= T, col =colchart[3], lwd =lwd[4], from=Q[1],to=Q[2])
############################
## Hill and Upchurch 1995 ##
hill <- read.csv("data/input/Hill1995_table1(SexRatio).csv")
## compute male-female sr (original is female-male)
sr5 <- 1/hill[,"SR.q5"]
## compute total q based on sr
q5m.hill <- hill[,"q5.male"]
q5.hill <- q5m.hill*((SRB+1/sr5)/(1+SRB))
lines(x=log(q5.hill), y=sr5, col =colchart[4], lwd = lwd[5], lty=1)
# points(sr5 ~ log(q5.hill), pch=19, cex=0.5, col="indianred4")

legend("bottomright", 
       c("W5", "S5 all country-years", 
         "S5 comparable country-years", 
         "IHME S5 comparable country-years", 
         "Hill and Upchurch (1995)"), 
       col=c("purple", colchart[1:4]), 
       lty=1, lwd=9,cex=1.5)
dev.off()

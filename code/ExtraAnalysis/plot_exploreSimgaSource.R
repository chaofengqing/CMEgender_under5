
## sigma parameters for source types ##
load(paste0(output.dir,runname,"_postinfo.rda")) #post.full

sigmapara <- paste0("sigma",rep(c(1,4,5),each=S-1),".s[",seq(1,S-1),"]")[-c(1,4,8,11,16,17,19,20)]
sigmaparaname <- paste0(rep(c(1,4,5),each=S-1)," [",sort(unique(typename.i))[-S],"]")[-c(1,4,8,11,16,17,19,20)]
post <- post.full[sigmapara,c("95lower","median","95upper")]

pdf(paste0(fig.dir,"post_",runname,"_sigmapara.pdf"),height=7,width=20)
par(mar=c(5,14,2,1),cex.lab=2,mgp=c(3,0.5,0),cex.axis=1.5)  

# PlotCIsegments(x=post,name.plot=sigmaparaname,cex=1.5,lwd.main =4.5,cutoff=0,
#                xlab="posterior samples of hyper parameters",colinfo=c("slategray1","navyblue"))  
# dev.off()  
x=post
name.plot=sigmaparaname
cex=1.5
lwd.main =4.5
cutoff=0
xlab="posterior samples of sigma1/4/5.s parameters"
colinfo=c("slategray1","navyblue")

in.l<-x[,1]
in.b<-x[,2]
in.u<-x[,3]    

N=dim(x)[1]
pick<-seq(1,N)

n<-length(pick)
labels <-name.plot[pick]
o<-rev(order(labels))
labels<-labels[o]
xmin <- min(in.l[pick],na.rm=TRUE);xmax <- max(in.u[pick],na.rm=TRUE)
if.plotall<-Xaxis<-Yaxis<-TRUE
nb<-trunc((xmax-xmin)/(0.1));  x2<-ifelse(if.plotall,cutoff+nb*0.1,cutoff);  x1<-(cutoff-nb*0.1);  by<-0.1

lower <- c(in.l[pick])[o]
upper <- c(in.u[pick])[o]
best  <- c(in.b[pick])[o]

temp <- rnorm(n,0,1)
temp.iq <- cbind(temp*runif(n, 0.7,0.9), temp, temp*runif(n,1.05,1.3))

plot(seq(1,n) ~ temp.iq[,2], type= "n", ylab = "", yaxt = "n", xaxt=ifelse(Xaxis==TRUE,"n","s"),
     xlim = c(xmin, xmax),xlab = xlab)
if(Xaxis==TRUE) {
  axis(1, las = 1, at = seq(x1,x2, by), label = seq(x1,x2, by),tick=FALSE)
  abline(v=seq(x1,x2,by),col="lightgrey",lty=2)
} 

if(Yaxis==TRUE) axis(2, las = 1, at = seq(1,n), label = labels,tick=FALSE)
for (i in seq(1,n,2)){
  polygon(-0.5+c(xmin,xmin,500,500,xmin),i + c(-0.5, 0.5, 0.5, -0.5, -0.5),col = colinfo[1], border = NA)
}
box()
segments(lower,seq(1,n),upper,seq(1,n),  lwd = lwd.main, col = colinfo[2])
points(seq(1,n) ~ best, col = colinfo[2], pch = 19, cex = cex)
abline(v=cutoff,col=colinfo[2],lwd=2)
dev.off()  


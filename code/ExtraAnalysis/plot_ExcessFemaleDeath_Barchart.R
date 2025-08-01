
## plot excess female deaths in 1990 and 2012 combined on log10 scale ##

n = R+1
## global ##
LOG10excDf.qt <- log10(res[["excD5f.qt"]][,is.element(years,c(1990.5,2012.5))])
## regional ##
excDf.rqt <- res[["excD5f.rqt"]][,,is.element(years,c(1990.5,2012.5))]
LOG10excDf.rqt <- array(NA, c(R,3,2))
for(t in 1:2) {
  sign.rq <- (!(excDf.rqt[,,t]>0 | is.na(excDf.rqt[,,t])))*(-1) +  (excDf.rqt[,,t]>0 | is.na(excDf.rqt[,,t]))*1#identify neg and posi sign
  LOG10excDf.rqt[,,t] <- log10(abs(excDf.rqt[,,t]))*sign.rq  
}#end of t loop

temp <- rnorm(n,0,1)
temp.iq <- cbind(temp*runif(n, 0.7,0.9), temp, temp*runif(n,1.05,1.3))

labels <- c(regions,"Global") # names of obs/outcomes

xmin <- min(LOG10excDf.rqt, LOG10excDf.qt)
xmax <- max(LOG10excDf.rqt, LOG10excDf.qt)
xlab = "Excess female deaths, age group 5 (log10-scale)"

cm=1/2.54
pdf(paste0(fig.dir,"ExcessFemale_",runname,"_Bar1_log10scale.pdf"),height=8,width=10)
par(mar = c(3,15,1,1),cex.lab=1.4, cex.axis=1.2,mgp=c(1.7,0.3,0))
plot(seq(1,n) ~ temp.iq[,2], type= "n", ylab = "", 
     yaxt = "n",xaxt = "n", xlim = c(round(xmin), round(xmax)),xlab = xlab,
     ylim=c(0,n+1))
labelDeath    <- c(-1*10^seq(5,1),0,10^seq(1,6))
LOG10labelDeath <- c(log10(abs(labelDeath[1:5]))*(-1),0,log10(abs(labelDeath[7:12])))
plotlabelDeath <- c(expression(-10^5),expression(-10^4),expression(-10^3),expression(-10^2),-10,0,
                    10, expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6))
axis(1,at = LOG10labelDeath, label = plotlabelDeath, las = 1,tcl=-0.3)
axis(2, las = 1, at = c(seq(1,R),n+0.5), label = labels, tick=FALSE)
abline(v=LOG10labelDeath,lty=2,col="lightgray",lwd=0.5)
for (i in seq(1,n-1,2)){
  polygon(-0.4+c(xmin,xmin,500,500,xmin)-10,
          i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
          col = adjustcolor("lightskyblue", alpha.f = 0.35), border = NA)
}
polygon(-0.5+c(xmin,xmin,500,500,xmin)-10,
        (n+0.5) + c(-0.9, 1, 1, -0.9, -0.9),
        col = adjustcolor("lightpink", alpha.f = 0.35), border = NA)

box()
abline(v=0,lty=1)

# use "add" when drawing more than 1 set of CIs close to i-th line
colglobal <- c("hotpink","brown3")
colregion <- c("turquoise4", "royalblue")
for(t in 1:2){
  add <- -0.3*(t-1)
  ## global ##
  segments(LOG10excDf.qt[1,t],add+n+0.5,LOG10excDf.qt[3,t],add+n+0.5,lwd = 6, col = colglobal[t])
  points(add+n+0.5 ~ LOG10excDf.qt[2,t],col = colglobal[t], pch = 19, cex = 1.5)
  
  ## regional ##
  segments(LOG10excDf.rqt[,1,t],add+seq(1,R),
           LOG10excDf.rqt[,3,t],add+seq(1,R), lwd = 4, col = colregion[t])
  points(add+seq(1,R) ~ LOG10excDf.rqt[,2,t], col = colregion[t], pch = 19, cex = 1.2)
}#end of t loop

legend("bottomleft",c("Global 1990","Global 2012","region 1990","region 2012"),col=c(colglobal,colregion),
       lty=1,lwd=5,pch=19,cex=1)
dev.off()


######################
## another bar plot ##
BarChart <- function(# Plot bar chart
  res.gq3t, # needs to be 3Dim array with G>1, e.g. g refers to a set of countries, and t to years, q3 denoted lower, median, upper
  order.g  = NULL, # can be subset, with G=1
  xlimmax = 100,
  xmin = 0,
  logscale = "", # any log-scales?
  xlab = "Percentage",
  xlabel = NULL,
  main = NULL, 
  add.legend = TRUE
){
  
  if (is.null(order.g)){
    G <- dim(res.gq3t)[[1]]
    order.g <- seq(1, G)
  } else {
    G <- length(order.g)
  }
  #if (is.null(xmin)) xmin <- 0
  name.g <- dimnames(res.gq3t)[[1]][order.g]
  nyears <- dim(res.gq3t)[3]
  name.t <- dimnames(res.gq3t)[[3]]
  # grey vertical bounds in background at vertbounds
  xmax <- min(xlimmax, max(res.gq3t[order.g,,])*1.05)
#   vertbounds <- GetNiceSequence(xmin = 0, xmax = xmax)
  # start plot
#   par( cex.main = 2, cex.axis = 2, cex.lab = 2)
  plot(seq(1, G)~rep(1,G), type = "n", 
       ylab = "",  yaxt = "n",
       xlim = c(xmin,xmax), xaxt = "n",
       bty = "n", log = logscale,
       ylim = c(G+0.5,0.5), # reverse ylim
       xlab = xlab, main = main)
  if (G>1) axis(2, at = seq(1, G), label = name.g, las = 1)#, cex.axis = 1)
  
#   if (length(vertbounds)>1){
#     for (s in seq(xmin,round(xmax)-1,2)){
#       polygon(vertbounds[c(s,s,s+1,s+1,s)],
#               c(0,G+1,G+1,0,0), border = "NA", 
#               col = adjustcolor("lightgrey", alpha.f = 0.4))
#     }
#   }
#   axis(1, at = vertbounds, label = vertbounds, las = 1)# cex.axis = 1)
  axis(1, at = seq(xmin,round(xmax)), label = xlabel, las = 1)# cex.axis = 1)
  #palette("default")
  #mycols <- adjustcolor(palette(), alpha.f = 0.4)
  col.t <- c(brewer.pal(nyears, ifelse(nyears<10, "Set1","Paired")))
  colfaded.t <- adjustcolor(col.t, alpha.f = 0.4)
  # depending on number of years, halfwidths and add need to adjusted
  # note that ylab is increasing as usual,
  # so if you want years in decreasing order, add needs to decrease
  startseq <- ifelse(nyears <=3, 0.22, 
                     ifelse(nyears <=7, 0.3,
                            0.36))
  add.t <- seq( -startseq, startseq, length.out = nyears)
  halfwidth <- (add.t[2] - add.t[1])/2.5
  if (G==1) axis(2, at = 1+add.t, label = name.t, las = 1)
  for (t in 1:nyears){
    add <- add.t[t]
    for (g in 1:G){
      res.q <- res.gq3t[order.g[g],,t]
      segments(res.q[1], add+g, res.q[3], lwd = 2, 
               add+g, col = col.t[t])
      polygon(c(0, res.q[2],res.q[2], 0,0), 
              add + g + c(-halfwidth,-halfwidth,+halfwidth, +halfwidth,-halfwidth), border = col.t[t],
              col = colfaded.t[t] )      
      points(add+g~res.q[2], pch = 20, lwd = 2, col = col.t[t])
    } # end g
    box()
  } # end t
  if (add.legend){ #!is.null(yearsnames)
    plot(1,type="n", xlab=NA,ylab=NA, xaxt="n",yaxt="n",bty="n")
    legend("left", legend = name.t, col = col.t[seq(1, nyears)], pch = -1, lty = 1, 
           cex = 1.2, lwd = 5)
  }
#   box()
  return(invisible())
}


# install.packages("RColorBrewer")
library(RColorBrewer)
res.gq3t <- array(NA,c(2,3,R))
for(t in 1:2){
  for(r in 1:R){
    res.gq3t[t,,r] <- LOG10excDf.rqt[r,,t]
  }#end of r loop
}#end of t loop
dimnames(res.gq3t)[[3]] <- regions
dimnames(res.gq3t)[[1]] <- c(1990, 2012)

pdf(paste0(fig.dir,"ExcessFemale_",runname,"_Bar2_log10scale.pdf"),height=8,width=14)
xlabel<- c(expression(-10^5),expression(-10^4),expression(-10^3),expression(-10^2),-10,0,
           10, expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6))
par(mfrow=c(1,2), cex.main = 2, cex.axis = 1.1, cex.lab = 1.3, mgp=c(1.8,0.4,0),tcl=-0.3,mar=c(3,3,1,0))
BarChart(res.gq3t,xmin=-5, xlabel=xlabel,xlab="Excess female deaths, age group 5 (log10-scale)",add.legend=TRUE)
dev.off()

# #########################################################################
# ## plot excess female deaths in 1990 and 2012 combined on normal scale ##
# n = R+1
# ## global ##
# excDf.qt <- res[["excD5f.qt"]][,is.element(years,c(1990.5,2012.5))]
# ## regional ##
# excDf.rqt <- res[["excD5f.rqt"]][,,is.element(years,c(1990.5,2012.5))]
# 
# temp <- rnorm(n,0,1)
# temp.iq <- cbind(temp*runif(n, 0.7,0.9), temp, temp*runif(n,1.05,1.3))
# 
# labels <- c(regions,"Global") # names of obs/outcomes
# 
# xmin <- min(excDf.rqt, excDf.qt)
# xmax <- max(excDf.rqt, excDf.qt)
# xlab = "Excess female deaths, age group 5"
# 
# cm=1/2.54
# pdf(paste0(fig.dir,"ExcessFemale_",runname,"_Bar1.pdf"),height=8,width=10)
# par(mar = c(3,15,1,1),cex.lab=1.4, cex.axis=1.2,mgp=c(1.7,0.3,0),tcl=-0.3)
# plot(1, type= "n", ylab = "", 
#      yaxt = "n",xlim = range(excDf.rqt, excDf.qt, na.rm=TRUE),xlab = xlab,
#      ylim=c(0,n+1))
# axis(2, las = 1, at = c(seq(1,R),n+0.5), label = labels, tick=FALSE)
# 
# for (i in seq(1,n-1,2)){
#   polygon(-0.4+c(xmin,xmin,500,500,xmin)+c(-1,-1,1,1,-1)*10^10,
#           i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
#           col = adjustcolor("lightskyblue", alpha.f = 0.35), border = NA)
# }
# polygon(-0.5+c(xmin,xmin,500,500,xmin)+c(-1,-1,1,1,-1)*10^10,
#         (n+0.5) + c(-0.9, 1, 1, -0.9, -0.9),
#         col = adjustcolor("lightpink", alpha.f = 0.35), border = NA)
# 
# box()
# abline(v=0,lty=1)
# 
# # use "add" when drawing more than 1 set of CIs close to i-th line
# colglobal <- c("hotpink","brown3")
# colregion <- c("turquoise4", "royalblue")
# for(t in 1:2){
#   add <- -0.3*(t-1)
#   ## global ##
#   segments(excDf.qt[1,t],add+n+0.5,excDf.qt[3,t],add+n+0.5,lwd = 6, col = colglobal[t])
#   points(add+n+0.5 ~ excDf.qt[2,t],col = colglobal[t], pch = 19, cex = 1.5)
#   
#   ## regional ##
#   segments(excDf.rqt[,1,t],add+seq(1,R),
#            excDf.rqt[,3,t],add+seq(1,R), lwd = 4, col = colregion[t])
#   points(add+seq(1,R) ~ excDf.rqt[,2,t], col = colregion[t], pch = 19, cex = 1.2)
# }#end of t loop
# 
# legend("bottomleft",c("Global 1990","Global 2012","region 1990","region 2012"),col=c(colglobal,colregion),
#        lty=1,lwd=5,pch=19,cex=1)
# dev.off()
# 
# 
# # install.packages("RColorBrewer")
# library(RColorBrewer)
# res.gq3t <- array(NA,c(2,3,R))
# for(t in 1:2){
#   for(r in 1:R){
#     res.gq3t[t,,r] <- excDf.rqt[r,,t]
#   }#end of r loop
# }#end of t loop
# dimnames(res.gq3t)[[3]] <- regions
# dimnames(res.gq3t)[[1]] <- c(1990, 2012)
# 
# pdf(paste0(fig.dir,"ExcessFemale_",runname,"_Bar2.pdf"),height=8,width=14)
# par(mfrow=c(1,2), cex.main = 2, cex.axis = 1.1, cex.lab = 1.3, mgp=c(1.8,0.4,0),tcl=-0.3,mar=c(3,3,1,0))
# BarChart(res.gq3t,xmin=min(c(res.gq3t),na.rm=TRUE),xlimmax=max(c(res.gq3t),na.rm=TRUE),
#          xlab="Excess female deaths, age group 5",add.legend=TRUE)
# dev.off()

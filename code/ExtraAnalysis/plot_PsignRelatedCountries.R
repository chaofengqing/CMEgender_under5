
colchart <- c("hotpink3", "lightslateblue", "royalblue")

for(yr in c(1990.5, 2000.5, 2010.5, 2012.5)){
  yr.posi <- which(yr == years)
  for(age in ages){
    ## outlying countries picked by P < 1 ##
    P.c <- res[[paste0("P",age,".cqt")]][,3,yr.posi]    
    pick.c <- which(P.c < 1)
    o <- rev(order(res[[paste0("P",age,".cqt")]][pick.c,2,yr.posi]))
    country.P <- name.c[pick.c[o]]
    
    n <- length(country.P)
    
    Q.scq <- array(NA,c(3,n,3))
    Q.scq[1,,] <- res[[paste0("expQ",age,"f.cqt")]][pick.c[o],,yr.posi]*1000 #expected Qfemale
    Q.scq[2,,] <- res[[paste0("Q",age,"f.cqt")]][pick.c[o],,yr.posi]*1000    #estimated Qfemale
    Q.scq[3,,] <- res[[paste0("Q",age,"f.cqt")]][pick.c[o],,yr.posi]*1000    #estimated Qmale
    
    temp <- rnorm(n,0,1)
    temp.iq <- cbind(temp*runif(n, 0.7,0.9), temp, temp*runif(n,1.05,1.3))
    
    pdf(paste0(PplotExplore.dir,"P",age,"signCountry_",runname,"_",round(yr),"_Mortality.pdf"),height=8,width=10)
    par(mar = c(3,10,1,1),cex.lab=1.4, cex.axis=1.2,mgp=c(1.7,0.3,0))
    plot(seq(1,n) ~ temp.iq[,2], type= "n", ylab = "", 
         yaxt = "n",xlab = "Q*1000",xlim = range(c(Q.scq),0,na.rm=TRUE),
         ylim=c(0,n+0.5))
    axis(2, las = 1, at = seq(1,n), label = country.P, tick=FALSE)
    for (i in seq(1,n,2)){
      polygon(-0.4+c(xmin,xmin,500,500,xmin)-10,
              i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
              col = adjustcolor("lightskyblue", alpha.f = 0.35), border = NA)
    }#end of i loop
    box()
    abline(v=0,lty=1)
    
    # use "add" when drawing more than 1 set of CIs close to i-th line
    for(s in 1:3){
      add <- -0.2*(s-1)+0.1
      ## regional ##
      segments(Q.scq[s,,1],add+seq(1,n),
               Q.scq[s,,3],add+seq(1,n), lwd = 3, col = colchart[s])
      points(add+seq(1,n) ~ Q.scq[s,,2], col = colchart[s], pch = 19, cex = 1)
    }#end of s loop
    
    legend("bottomright",c("expected Qfemale","estimated Qfemale","estimated Qmale"),col=colchart,
           lty=1,lwd=5,pch=19,cex=1)
    dev.off()
    
    
    
    #############################
    ## 
    q.limit <- c(10/1000,20/1000)
    country.pick <- list(Q1.ct[,yr.posi]>=q.limit[1],
                         Q1.ct[,yr.posi]>=q.limit[2],
                         Q4.ct[,yr.posi]>=q.limit[1],
                         Q4.ct[,yr.posi]>=q.limit[2],
                         Q5.ct[,yr.posi]>=q.limit[1],
                         Q5.ct[,yr.posi]>=q.limit[2])
    a <- which(ages == age)
    for(pick in 1:2){
      c.pick <- country.pick[[(a-1)*2+pick]]
      c.pick[is.na(c.pick)] <- FALSE
      
      P.c <- res[[paste0("P",age,".cqt")]][,3,yr.posi]    
      pick.c <- which(P.c < 1 & c.pick)
      if(length(pick.c)>0){
        o <- rev(order(res[[paste0("P",age,".cqt")]][pick.c,2,yr.posi]))
        country.P <- name.c[pick.c[o]]
        
        n <- length(country.P)
        
        Q.scq <- array(NA,c(3,n,3))
        Q.scq[1,,] <- res[[paste0("expQ",age,"f.cqt")]][pick.c[o],,yr.posi]*1000 #expected Qfemale
        Q.scq[2,,] <- res[[paste0("Q",age,"f.cqt")]][pick.c[o],,yr.posi]*1000    #estimated Qfemale
        Q.scq[3,,] <- res[[paste0("Q",age,"f.cqt")]][pick.c[o],,yr.posi]*1000    #estimated Qmale
        
        temp <- rnorm(n,0,1)
        temp.iq <- cbind(temp*runif(n, 0.7,0.9), temp, temp*runif(n,1.05,1.3))
        pdf(paste0(PplotExplore.dir,"P",age,"signCountry_",runname,"_",round(yr),"_Q",age,"above",q.limit[pick]*1000,"_Mortality.pdf"),height=8,width=10)
        par(mar = c(3,10,1,1),cex.lab=1.4, cex.axis=1.2,mgp=c(1.7,0.3,0))
        plot(seq(1,n) ~ temp.iq[,2], type= "n", ylab = "", 
             yaxt = "n",xlab = "Q*1000",xlim = range(c(Q.scq),0,na.rm=TRUE),
             ylim=c(0,n+0.5))
        axis(2, las = 1, at = seq(1,n), label = country.P, tick=FALSE)
        for (i in seq(1,n,2)){
          polygon(-0.4+c(xmin,xmin,500,500,xmin)-10,
                  i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
                  col = adjustcolor("lightskyblue", alpha.f = 0.35), border = NA)
        }#end of i loop
        box()
        abline(v=0,lty=1)
        
        # use "add" when drawing more than 1 set of CIs close to i-th line
        for(s in 1:3){
          add <- -0.2*(s-1)+0.1
          ## regional ##
          segments(Q.scq[s,,1],add+seq(1,n),
                   Q.scq[s,,3],add+seq(1,n), lwd = 3, col = colchart[s])
          points(add+seq(1,n) ~ Q.scq[s,,2], col = colchart[s], pch = 19, cex = 1)
        }#end of s loop
        
        legend("bottomright",c("expected Qfemale","estimated Qfemale","estimated Qmale"),col=colchart,
               lty=1,lwd=5,pch=19,cex=1)
        dev.off()
        
      }#end of if(length(pick.c)>0)
    }#end of pick loop
    
  }#end of age loop  
}#end of yr loop



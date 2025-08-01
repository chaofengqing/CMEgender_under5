
## bimodal function ##
ftmp <- function(x) ifelse(x < 3, x^2, (x-4)^2+8)

plot(ftmp, -2,5, lwd=3)
## optimize ##
abline(v=optimize(f=ftmp, interval=c(-2, 5))$minimum,col=2)
abline(v=optimize(f=ftmp, interval=c(-1,10),tol=10)$minimum,col=3)
abline(v=optimize(f=ftmp, interval=c(1,10))$minimum,col=4)
## which.min ##
x <- seq(-2,5,0.0001)
abline(v=x[which.min(ftmp(x))])

#default tol value
sqrt(.Machine$double.eps) #1.49e-08


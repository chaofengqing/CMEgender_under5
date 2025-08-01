

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_ConvergenceCheck.R
# 
# This script checks the convergence of JAGS model output, i.e. mcmc.array. It
# produces trace plots for (selected) parameters, and compute median, CI, R hat
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: data/output/runname/mcmc.array_runname.rda
#
# output data: data/output/runname/*_postinfo.csv - R hat; post sample median, CI
# output plot: fig/runname/convergence/*
#
###############################################################################


################
## trace plot ##

## hyper para ##
pdf(paste0(convergeplot.dir, "trace_", runname, ".pdf"))
for (parname in parnames.hyper) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}

for (parnames in c("sigma1.s", "sigma4.s", "sigma5.s")) {
  for (parname in paste0(parnames, "[", seq(1, S - 1), "]")) {
    PlotTrace(parname = parname, mcmc.array = mcmc.array)
  }
}    
dev.off()

## selected P ##
selectPpar <- c(paraP1[sample(length(paraP1), plotWhichP)], 
                paraP4[sample(length(paraP4), plotWhichP)])
pdf(paste0(convergeplot.dir, "trace_Pctl_", runname, ".pdf"))
for (parname in selectPpar) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}
dev.off()

## W1 and W4 ##
selectWpar <- c(paste0("logf1.k[", seq(1, k1, plotWhichW), "]"),
                paste0("logf4.k[", seq(1, k4, plotWhichW), "]"))

pdf(paste0(convergeplot.dir, "trace_W_", runname, ".pdf"))
for (parname in selectWpar) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}
dev.off()

## a.c and b.c ##  
acpar <- paste0("a.c[", seq(1, C), "]")
bcpar <- paste0("b.c[", seq(1, C), "]")

pdf(paste0(convergeplot.dir, "trace_acbc_", runname, ".pdf"))
for (parname in c(acpar, bcpar)) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}
dev.off()

## splines para ##
pdf(paste0(convergeplot.dir, "trace_", runname, "_splinespara.pdf"))
for (parnames in c("b1.k", "b4.k")) {
  for (parname in paste0(parnames, "[", seq(1, d), "]")) {
    PlotTrace(parname = parname, mcmc.array = mcmc.array)
  }
}
for (parname in paste0("u1.q", "[", seq(1, Q.1), "]")) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}
for (parname in paste0("u4.q","[", seq(1, Q.4), "]")) {
  PlotTrace(parname = parname, mcmc.array = mcmc.array)
}
dev.off()  


# ######################
# needs temp jags objects, not updated
# ## check DIC values ##
# if(First.run){
#   DIC.matrix<-matrix(NA,nr=mcmc.chains,nc=N.STEPS) #save DIC value
#   dimnames(DIC.matrix)[[1]]<-c(paste("chain",seq(1,mcmc.chains),sep=" "))
#   dimnames(DIC.matrix)[[2]]<-c(paste("step",seq(1,N.STEPS),sep=" "))
#   
#   for(chain in 1:mcmc.chains){
#     for(i in 1:N.STEPS){
#       load(paste0(output.dir,"temp.JAGSobjects/jags_mod",runname,chain,"update_",i,".Rdata"))
#       DIC.matrix[chain,i]<-mod.upd$BUGSoutput$DIC
#       remove(mod.upd)
#     }#end of N.STEPS loop
#   }#end of mcmc.chains loop
#   write.csv(DIC.matrix,paste0(output.dir,"DIC_",runname,"_",IntervalLength,"knots.csv"))
# }

###############################################
## compute R hat and post sample information ##
post.full <- getPostInfo(mcmc.array)
write.csv(post.full, paste0(output.dir, runname, "_postinfo.csv")) #checking only

## The End! ##

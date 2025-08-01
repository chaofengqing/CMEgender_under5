
pdf(file.path(figdir,"priorsandposts.pdf"), width = 12, height = 12)
par(mar = c(5,5,1,1), cex.lab = 1.5, cex.axis = 1.5)
parnames <- paste("b",rep(c(1,4),each=1),".k[",1,"]",sep="")
for (parname in parnames){
  PlotPostWithNormalPrior(c(mcmc.array[, ,parname]), 
                          priormean = mort.data$b0[1], 
                          priorsd = 1/sqrt(mort.data$Tau0[1,1]) ,
                          parname = parname)
}

parnames <- paste("b",rep(c(1,4),each=1),".k[",2,"]",sep="")
for (parname in parnames){
  PlotPostWithNormalPrior(c(mcmc.array[, ,parname]), 
                          priormean = mort.data$b0[2], 
                          priorsd = 1/sqrt(mort.data$Tau0[2,2]) ,
                          parname = parname)
}

dev.off()

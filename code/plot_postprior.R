

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_postprior.R
#
# This script plots prior distributions and posterior samples of selected
# parameters from JAGS model output, i.e. mcmc.array.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called:
# PlotPostWithUnifPrior(3)
# PlotPostSDWithGammaPrior(1)
# 
# input data: data/output/runname/mcmc.array_runname.rda
#
# output plot: fig/runname/convergence/priorpost_runname.pdf
#
###############################################################################

pdf(paste0(convergeplot.dir, "priorpost_", runname, ".pdf"))
par(mfrow = c(2, 2), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
for (par in c("nu5", "rho", "sigma.eps", "sigma.14")) {
  PlotPostWithUnifPrior(
    post.samp = c(mcmc.array[, , par]), parname = par,
    priorlow = eval(parse(text = paste0("pri.", par, ".lower"))),
    priorup = eval(parse(text = paste0("pri.", par, ".upper")))
  )
}#end of par loop

par(mfrow = c(2, 2), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
for (par in c("sigma.a", "sigma.b")) {
  PlotPostSDWithGammaPrior(
    post.samp = c(mcmc.array[, , par]), parname = par,
    priorshape = pri.sigma.a.shape, 
    priorrate = pri.sigma.a.shape * pri.sigma.ab.gamma.rate[par]
  )
}#end of par loop

par(mfrow = c(2, 2), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
# b1 and b4 have the same prior info
for (k in 1:d) { # splines computation: dim(G)[2] = d
  for (par in c(paste0("b1.k[", k, "]"), paste0("b4.k[", k, "]"))) {
    PlotPostWithUnifPrior(
      post.samp = c(mcmc.array[, , par]), parname = par,
      priorlow = eval(parse(text = paste0("pri.b1", k, ".lower"))),
      priorup = eval(parse(text = paste0("pri.b1", k, ".upper")))
    )
  }#end of par loop
}#end of k loop

# error variance
for (parname in c("sigma1.s", "sigma4.s", "sigma5.s")) {
  par(mfrow = c(2, 2), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
  for (par in paste0(parname, "[", seq(1, S - 1), "]")) {
    PlotPostWithUnifPrior(
      post.samp = c(mcmc.array[, , par]), parname = par,
      priorlow = pri.sigma1s.lower, priorup = pri.sigma1s.upper
    )
  }#end of par loop
}#end of parnames loop

dev.off()

## the end ##


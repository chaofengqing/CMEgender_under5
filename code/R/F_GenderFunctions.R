

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# F_GenderFunctions.R
# 
# This script contains all functions related to mortality computations.
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# GetQmale(..)
# GetQboth(..)
# GetQbothfromQmale(..)
# GetQfemale(..)
# GetQ4fromQ15(..)
# GetQ5fromQ14(..)
# GetQfromDeath(.., GetQ5fromQ14(1), ..)
# GetExpQf(.., GetQboth(1), ..)
# GetWeight4(..)
# GetS(..)
# GetS5(..)
# GetS5fromQmale(..)
# GetS5fromQboth(.., GetQmale(2), GetWeight4(1), GetS5fromQmale(1), ..)
# GetM1fromQ(..)
# GetM4fromQ(..)
# GetDeath(.., GetM1fromQ(1), GetM4fromQ(1), ..)
# 
###############################################################################

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetQmale <- function(qboth, w, s) {
  qboth / (w + (1 - w) / s)
}
#GetQmale(qboth = , w = , s = )
#------------------------------------------------------------------------------

GetQboth <- function(qmale, qfemale, w) {
  w * qmale + (1 - w) * qfemale
}
#GetQboth(qmale = , qfemale = , w = )
#------------------------------------------------------------------------------

GetQbothfromQmale <- function(
  qmale, s,
  k.SRB = SRB #SRB is a constant here: 1.05
){
  qmale * ((k.SRB + 1 / s) / (1 + k.SRB))
}
#GetQboth(qmale = , s = , w = , k.SRB = SRB)
#------------------------------------------------------------------------------

GetQfemale <- function(qmale, s) {
  qmale / s
}
#GetQfemale(qmale = , s = )
#------------------------------------------------------------------------------

GetQ4fromQ15 <- function(q1, q5) {
  (q5 - q1) / (1 - q1)
}
# GetQ4fromQ15(q1 = , q5 = )
#------------------------------------------------------------------------------

GetQ5fromQ14 <- function(q1, q4) {
  q1 + (1 - q1) * q4
}
# GetQ5fromQ14(q1 = , q4 = )
#------------------------------------------------------------------------------

GetQfromDeath <- function(death1, death4, pop1, pop4, a1, a4) {
  M1 <- death1 / pop1
  M4 <- death4 / pop4
  #FQ: use functions for the next two lines;
  #FQ to check: what's 0.2, 0.1, 0.3, 4?
  q1 <- M1 / (1 + (1 - ifelse(mean(a1, na.rm = TRUE) < 0.2, 0.1, 0.3)) * M1)
  q4 <- 4 * M4 / (1 + (4 - 4 * mean(a4, na.rm = TRUE)) * M4)
  q5 <- GetQ5fromQ14(q1, q4)
  
  tmp <- list(q1 = q1, q4 = q4, q5 = q5)
}
#GetQfromDeath(death1=, death4=, pop1=, pop4=, a1=, a4=)
#------------------------------------------------------------------------------

GetExpQf <- function(qm, wa, W.k, gridQtot.k,
                     max.W = maxW, min.W = minW) {
  ## get expected Qfemale ##
  from.f <- qm / max.W
  to.f   <- min(qm / min.W, 1) #make sure max(qf.f) <= 1
  # Instead of fixing the gap of the sequence, we assign the number of
  # the sequence based on the range of the sequence. When qm is high,
  # the length of the sequence need not be very high in order to find
  # the best result.
  length.f  <- ifelse(qm * Qunit <= 10, 400,
                      ifelse(qm * Qunit <= 50, 500,
                             ifelse(qm * Qunit <= 200, 1000, 2000)))
  
  qf.f <- seq(from.f, to.f, length.out = length.f)
  getk.f <- rep(NA, length.f)  
  
  for (f in 1:length.f) {
    getk.f[f] <- which.min(abs(gridQtot.k - GetQboth(qm, qf.f[f], wa)))
  }#end of f loop
  
  expQf <- qf.f[which.min(abs(qm / qf.f - W.k[getk.f]))]
  
  return(expQf)
  
}
#GetExpQf(qm = , wa =, W.k = , gridQtot.k = , max.W = maxW, min.W = minW)
#------------------------------------------------------------------------------

GetWeight4 <- function(w1, q1male, q1both) {
  w1 * (1 - q1male) / (1 - q1both)
}
# GetWeight4(w1 = ,q1male = ,q1both = )
#------------------------------------------------------------------------------

GetS <- function (qmale, qfemale) {
  qmale / qfemale
}
#GetS(qmale = , qfemale = )
#------------------------------------------------------------------------------

GetS5 <- function(q1male, q4male, q1female, q4female) {
  (1 - (1 - q1male) * (1 - q4male)) /
    (1 - (1 - q1female) * (1 - q4female)) 
}
#GetS5(q1male = , q4male = , q1female = , q4female = )
#------------------------------------------------------------------------------

GetS5fromQmale <- function(q1male, q4male, s1, s4) {
  (1 - (1 - q1male) * (1 - q4male)) /
    (1 - (1 - q1male / s1) * (1 - q4male / s4))
}
#GetS5fromS(q1male = , q4male = , s1 = , s4 = )
#------------------------------------------------------------------------------

GetS5fromQboth <- function(q1both, q4both, s1, s4) {
  q1male <-  GetQmale(qboth = q1both, w = w1, s = s1) 
  w4 <- GetWeight4(w1 = w1, q1male = q1male, q1both = q1both) 
  q4male <-  GetQmale(qboth = q4both, w = w4, s = s4) 
  GetS5fromQmale(q1male = q1male, q4male = q4male, s1 = s1, s4 = s4)
}
#GetS5fromQboth(q1both = , q4both = , s1 = , s4 = )
#------------------------------------------------------------------------------

GetM1fromQ <- function(q1, a1) {
  q1 / (1 - q1 * (1 - a1))
}
#GetM1fromQ(q1 = , a1 = )
#------------------------------------------------------------------------------

GetM4fromQ <- function(q4, a4) {
  q4 / (4*(1 - q4 * (1 - a4)))
}
#GetM4fromQ(q4 = , a4 = )
#------------------------------------------------------------------------------

GetDeath <- function(q1, q4, q5,
                     a1, a4,
                     pop1, pop4) {
  
  NAposi <- is.na(q1) | is.na(q5)
  # set population to 0 if rate data not available
  pop1 <- rep(pop1, length(q1))
  pop4 <- rep(pop4, length(q1))
  pop1[NAposi] <- 0
  pop4[NAposi] <- 0
  
  M1 <- GetM1fromQ(q1, a1) #q1 / (1 - q1 * (1 - a1))  
  M4 <- GetM4fromQ(q4, a4) #q4 / (4*(1 - q4 * (1 - a4)))
  M1[NAposi] <- 0
  M4[NAposi] <- 0
  
  death1 <- M1 * pop1
  death4 <- M4 * pop4
  death5 <- death1 + death4
  
  # set deaths to NA if rate data is not available
  death1[NAposi] <- NA
  death4[NAposi] <- NA
  death5[NAposi] <- NA
  
  tmp <- list(death1 = death1, death4 = death4, death5 = death5)
  return(tmp)
}
#GetDeath(q1 = , q4 = , q5 = , a1 = , a4 = , pop1 = , pop4 = )
#------------------------------------------------------------------------------



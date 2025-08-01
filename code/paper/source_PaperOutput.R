
## to get paper output ##

A <- 150
B <- 20
C <- 30
D <- 5

pred <- predict(loess(c(res$W5.cqt[, 2, ]) ~ log(c(Q5.ct[,])), na.action = "na.omit",family = "symmetric"), 
                log(seq(1, 1000) / Qunit), se = FALSE)
Q5 <- seq(1, 1000)[!is.na(pred)]
pred <- pred[!is.na(pred)]
E <- range(pred[Q5 >= 10])
G <- Q5[which.min(pred)]
H <- Q5[which.max(pred)]
AA <- pred[Q5 == D]#min(pred[Q5 <= H])

paste0("For age group 0, the estimated expected sex ratio increases from ", 
      round(res$W1.qk[, A], 2), " to ", round(res$W1.qk[, B],2),
      " as mortality decreases from around ",A," deaths per 1,000 births to around ",B,
      " deaths per 1,000 births. This increase is followed by a decrease in the estimated sex ratio from ",
      round(res$W1.qk[, B],2)," to ", round(res$W1.qk[,D], 2),
      " as total IMR decreases from ",B," to ",D," deaths per 1,000 births")
paste0("For ages 1--4, estimated sex ratios are close to 1", round(res$W4.qk[, C], 2),
       "for total mortality above", C, "deaths per 1000 survivors up to age 1.",
      " The ratio increases as mortality decreases, the maximum of ", round(res$W4.qk[, D], 2),
      " is reached for mortality of", D, "per 1,000.")

paste0("The estimated sex ratio for ages 0-4 is driven by the estimates for the sex ratios for age 0 and ages 1--4 
and increases from ",round(E[1],2)," to ",round(E[2],2), 
      " as U5MR decreases from ",G," to ",H," deaths per 1,000 live births."
      "This increase is followed by a decrease in the estimated sex ratio from ",
      round(E[2],2), " to ", round(AA,2),
      " as U5MR decreases from 19 deaths per 1,000 live births to zero.")

## data availability ##
print(paste(sum(yearto.i - yearfrom.i, na.rm = TRUE), "country-years for all observations."))
for (age in ages.a) {
  obs.countryyr <- sum((yearto.i - yearfrom.i)[agecat.i == age & !is.na(yearto.i)], na.rm = TRUE)
  print(paste(obs.countryyr, "country-years for age group", age))
}

## Abstract - Findings ##
# number of countries with outlying sex ratios and
# significant excess female U5MR in 2012
t <- which(years.t == 2012.5)
select1.c <-   (res$S5.cqt[, 1, t] > cutoffS | res$S5.cqt[, 3, t] < cutoffS) &
  (res$excQ5f.cqt[, 1, t] > cutoffQ | res$excQ5f.cqt[, 3, t] < cutoffQ) &
  (abs(res$excQ5f.cqt[, 2, t]) > 1 / Qunit)
sum(select1.c)
name.c[select1.c]
outlying.excQ5f.cqt <- res$excQ5f.cqt[select1.c, , t]
dimnames(outlying.excQ5f.cqt)[[1]] <- name.c[select1.c]
round(outlying.excQ5f.cqt * Qunit, 2)
# among above countries, number of countries where female mortality
# was lower than expected given total mortality
select2.c <- select1.c & (res$Q5f.cqt[, 2, t] < res$expQ5f.cqt[, 2, t])
sum(select2.c)
name.c[select2.c]
# among above countries, number of countries where female mortality
# was higher than expected given total mortality
select3.c <- select1.c & (res$Q5f.cqt[, 2, t] > res$expQ5f.cqt[, 2, t])
sum(select3.c)
name.c[select3.c]

## results - Table 1 ##
load(file = paste0(output.dir, "indicesforexcess.1.all.rda")) #indicesforexcess.1.all
load(file = paste0(output.dir, "indicesforexcess.4.all.rda")) #indicesforexcess.4.all
load(file = paste0(output.dir, "indicesforexcess.5.all.rda")) #indicesforexcess.5.all
# outlying world and regions in 1990 and 2012
# IMR
c((indicesforexcess.1.all$f1990.all | indicesforexcess.1.all$m1990.all)[["World"]], # in 1990
  (indicesforexcess.1.all$f2012.all | indicesforexcess.1.all$m2012.all)[["World"]]) # in 2012
cbind((indicesforexcess.1.all$f1990.all | indicesforexcess.1.all$m1990.all)[regions.r], # in 1990
      (indicesforexcess.1.all$f2012.all | indicesforexcess.1.all$m2012.all)[regions.r]) # in 2012
# CMR
c((indicesforexcess.4.all$f1990.all | indicesforexcess.4.all$m1990.all)[["World"]], # in 1990
  (indicesforexcess.4.all$f2012.all | indicesforexcess.4.all$m2012.all)[["World"]]) # in 2012
cbind((indicesforexcess.4.all$f1990.all | indicesforexcess.4.all$m1990.all)[regions.r], # in 1990
      (indicesforexcess.4.all$f2012.all | indicesforexcess.4.all$m2012.all)[regions.r]) # in 2012
# U5MR
c((indicesforexcess.5.all$f1990.all | indicesforexcess.5.all$m1990.all)[["World"]], # in 1990
  (indicesforexcess.5.all$f2012.all | indicesforexcess.5.all$m2012.all)[["World"]]) # in 2012
cbind((indicesforexcess.5.all$f1990.all | indicesforexcess.5.all$m1990.all)[regions.r], # in 1990
      (indicesforexcess.5.all$f2012.all | indicesforexcess.5.all$m2012.all)[regions.r]) # in 2012

# lowest sex ratios in regions
which.min(res[["S1.rqt"]][, 2, "1990"]) #Southern Asia
which.min(res[["S1.rqt"]][, 2, "2012"]) #Southern Asia
which.min(res[["S4.rqt"]][, 2, "1990"]) #Southern Asia
which.min(res[["S4.rqt"]][, 2, "2012"]) #Southern Asia
which.min(res[["S5.rqt"]][, 2, "1990"]) #Southern Asia
which.min(res[["S5.rqt"]][, 2, "2012"]) #Southern Asia

# regions where CMR and U5MR sr < 1 in 1990 and 2012: only Southern Asia
cbind(apply(res[["S4.rqt"]][, 2, c("1990", "2012")] < 1, 1, prod),
      apply(res[["S5.rqt"]][, 2, c("1990", "2012")] < 1, 1, prod))

# region with highest sex ratio for IMR, CMR, and U5MR in 2012
which.max(res[["S1.rqt"]][, 2, "2012"]) #Caucasus and Central Asia
which.max(res[["S4.rqt"]][, 2, "2012"]) #Developed regions
which.max(res[["S5.rqt"]][, 2, "2012"]) #Caucasus and Central Asia

# region with lowest ratio of estimated/expected Qfemale
which.min(res[["Rx1.rqt"]][, 2, "1990"]) #Caucasus and Central Asia
which.min(res[["Rx1.rqt"]][, 2, "2012"]) #Caucasus and Central Asia
which.min(res[["Rx4.rqt"]][, 2, "1990"]) #Caucasus and Central Asia
which.min(res[["Rx4.rqt"]][, 2, "2012"]) #Caucasus and Central Asia
which.min(res[["Rx5.rqt"]][, 2, "1990"]) #Caucasus and Central Asia
which.min(res[["Rx5.rqt"]][, 2, "2012"]) #Caucasus and Central Asia

# region with highest ratio of estimated/expected Qfemale
which.max(res[["Rx1.rqt"]][, 2, "1990"]) #Eastern Asia
which.max(res[["Rx1.rqt"]][, 2, "2012"]) #Southern Asia
which.max(res[["Rx4.rqt"]][, 2, "1990"]) #Southern Asia
which.max(res[["Rx4.rqt"]][, 2, "2012"]) #Southern Asia
which.max(res[["Rx5.rqt"]][, 2, "1990"]) #Southern Asia
which.max(res[["Rx5.rqt"]][, 2, "2012"]) #Southern Asia


## results - outlying countries ##

# number of countries with positive female excess IMR
sum(indicesforexcess.1.all$f1990.all[name.c]) # in 1990
sum(indicesforexcess.1.all$f2012.all[name.c]) # in 2012
sum(indicesforexcess.1.all$f2012.all[name.c]) # in 2012

t <- which(years.t == 2012.5)
o.2012 <- rev(order(res$excQ1f.cqt[indicesforexcess.1.all$f2012.all[name.c], 2, t]))
c.select <- which(indicesforexcess.1.all$f2012.all[name.c])[o.2012]
round(res$excQ1f.cqt[c.select, , t] * Qunit, 1)

# country with highest excess female IMR in 2012
which.max(res[["excQ1f.cqt"]][, 2, "2012"]) #India

# number of countries with positive female excess CMR
sum(indicesforexcess.4.all$f1990.all[name.c]) # in 1990
sum(indicesforexcess.4.all$f2012.all[name.c]) # in 2012

# outlying CMR in 1990 and/or 2012 also have outlying IMR:
name.c[((indicesforexcess.4.all$f1990.all | indicesforexcess.4.all$f2012.all) &
         (indicesforexcess.1.all$f1990.all | indicesforexcess.1.all$f2012.all))[name.c]]

# country with highest excess female CMR in 2012
which.max(res[["excQ4f.cqt"]][, 2, "2012"]) #India

res[["S5.cqt"]]["India", 2, "2010"]

t <- which(years.t == 2012.5)
o.2012 <- rev(order(res$excQ4f.cqt[indicesforexcess.4.all$f2012.all[name.c], 2, t]))
c.select <- which(indicesforexcess.4.all$f2012.all[name.c])[o.2012]
round(res$excQ4f.cqt[c.select, , t] * Qunit, 1)


# number of countries with positive female excess U5MR
sum(indicesforexcess.5.all$f1990.all[name.c]) # in 1990
sum(indicesforexcess.5.all$f2012.all[name.c]) # in 2012

# outlying CMR in 1990 and/or 2012 also have outlying IMR:
name.c[which(indicesforexcess.5.all$f1990.all | indicesforexcess.5.all$f2012.all)[!is.element(which(indicesforexcess.5.c$f1990.c | indicesforexcess.5.c$f2012.c),
                   which(indicesforexcess.4.c$f1990.c | indicesforexcess.4.c$f2012.c |
                           indicesforexcess.1.c$f1990.c | indicesforexcess.1.c$f2012.c))]]
t <- which(years.t == 2012.5)
o.2012 <- rev(order(res$excQ5f.cqt[indicesforexcess.5.all$f2012.all[name.c], 2, t]))
c.select <- which(indicesforexcess.5.all$f2012.all[name.c])[o.2012]
round(res$excQ5f.cqt[c.select, , t] * Qunit, 1)

# check Table 3 - outlying countries
# outlying IMR
outlyingIMR.c <- (indicesforexcess.1.all$f1990.all | indicesforexcess.1.all$f2012.all)[name.c]
cbind((indicesforexcess.1.all$f1990.all | indicesforexcess.1.all$m1990.all)[name.c[outlyingIMR.c]],
      (indicesforexcess.1.all$f2012.all | indicesforexcess.1.all$m2012.all)[name.c[outlyingIMR.c]])
# outlying CMR
outlyingCMR.c <- (indicesforexcess.4.all$f1990.all | indicesforexcess.4.all$f2012.all)[name.c]
cbind((indicesforexcess.4.all$f1990.all | indicesforexcess.4.all$m1990.all)[name.c[outlyingCMR.c]],
      (indicesforexcess.4.all$f2012.all | indicesforexcess.4.all$m2012.all)[name.c[outlyingCMR.c]])

# number of countries with negetive female excess IMR
sum(indicesforexcess.1.all$m1990.all[name.c]) # in 1990
sum(indicesforexcess.1.all$m2012.all[name.c]) # in 2012
# number of countries with negetive female excess CMR
sum(indicesforexcess.4.all$m1990.all[name.c]) # in 1990
sum(indicesforexcess.4.all$m2012.all[name.c]) # in 2012
# number of countries with negetive female excess U5MR
sum(indicesforexcess.5.all$m1990.all[name.c]) # in 1990
sum(indicesforexcess.5.all$m2012.all[name.c]) # in 2012
which(indicesforexcess.5.all$m2012.all[name.c])

res$excQ4f.cqt["India", , t] * Qunit
res$excQ4f.cqt["Pakistan", , t] * Qunit
# res$excQ4f.cqt["Niger", , t] * Qunit
res$excQ4f.cqt["Nepal", , t] * Qunit

res$excQ5f.cqt["India", , t] * Qunit
res$excQ5f.cqt["Afghanistan", , t] * Qunit
res$excQ5f.cqt["Pakistan", , t] * Qunit

# proportion of India excess female deaths under-5 in 2012
res$excD5f.cqt["India", 2, t] / res$excD5f.qt[2, t]
res$excD5f.cqt["India", 2, t] / (res$D5f.cqt["India", 2, t] + res$D5m.cqt["India", 2, t])


## countries with outlying sex ratio in 1990 or 2012? ##
name.c[indicesforexcess.1.c$f1990.c]

# for each age group select countries by running the following (uncomment to show result):
# ## select countries with outlying sex ratio ##
# t.select <- which(is.element(floor(years.t), c(1990, 2012)))
# ## select countries with outlying sex ratio ##
# eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", age, ".c")))
# include.c <- indicesforexcess[[paste0("f", floor(years.t[t.select[1]]), ".c")]] |
#   indicesforexcess[[paste0("m", floor(years.t[t.select[1]]), ".c")]] |
#   indicesforexcess[[paste0("f", floor(years.t[t.select[2]]), ".c")]] |
#   indicesforexcess[[paste0("m", floor(years.t[t.select[2]]), ".c")]]

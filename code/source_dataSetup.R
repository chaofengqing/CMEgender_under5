

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# source_dataSetup.R
# 
# This script setup data and indices for modelling and output.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_*.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Functions called in the scripts listed above are not listed.
# GetQ4fromQ15(1) - get CMR by using the IGME estimates of IMR and U5MR
# GetQmale(2)     - get sex-specific data
# GetQfemale(3)
# GetWeight4(1)
# GetQ5fromQ14(1)
# PlotCIbandwithDataseries(1) - data series plots for checking purpose.
# GetSplinesResults(2)        - JAGS model setup; plot splines
# ReformatPopulationData(2)   - to get male and female population data
# 
# input data in folder data/:
# note 1: data files 1 and 2 are read in main*_*.R before calling this script.
# note 2: data file 3 is loaded in this script only for Excl.run.
# 1. interim/dataset_formodeling.csv        - SR data base
# 2. input/Results.Table_Final_20130812.csv - IGME 2013 estimates file
# 3. output/M49/rmvCountry.rda              - info of countries to be excluded
#                                             in Excl.run
#
# output data:
# 1. data/interim/dataset_Validation.csv - if Validation.run & First.run;
# 2. all the variables and indices for JAGS model are constructed here.
#
# output plot in folder fig/M49/: if Main.run & First.run
# 1. B-splines_logscale.pdf - plot to illustrate B-splines used in this model
#
# Data setup summary in eight parts:
# part 1: setup country and region names for all countries
# use name.c as the default order of countries.
#
# part 2a: assign IGME 2013 estimates of IMR and U5MR as model input
# by matching to country-year of SR data base
# part 2b: remove entries in SR data base if there is no IGME estimate for
# its corresponding mortality
# part 2c: get sex-specific IMR, CMR, and U5MR
#
# part 3: setup training and testing set for validation run
#
# part 4a: get source types and make sure VR is the last type.
# note: the source/typename/age group 2-by-2 tables are in the txt file:
# 1. code/info/sourceType.txt
# These numbers will change if input data files are updated in the future. 
# part 4b: get SR and its standard error on log-scale.
# note: the details of getting logSEImpute = 0.15 are in the txt file:
# 1. code/info/getSE.txt
#
# part 5: setup B-splines on log-scale of IMR, CMR, and U5MR.
# round the country Q's and match with the sequence of Q's in splines
#
# part 6: get indices for included observations of age group 1,4 and 5
#
# part 7: get indices for non-missing P.ct's to avoid sampling for all P.ct's
# instead, only sample the t.i's that have an observation
#
# part 8: setup population-related data
# 
###############################################################################


if (Excl.run) {
  load(file = "data/output/M49/rmvCountry.rda") #rmvCountry
  rmest <- !is.element(est[, "ISO3Code"], rmvCountry[["rmvISO.c"]])
  print(rmvCountry[["rmvISO.c"]])
  
  est <- est[rmest, ]
  
}#end of if (Excl.run)

##########
## part 1: setup country and region names for all countries
# use name.c as the default order of countries.
iso.c  <- est[, "ISO3Code"   ]
name.c <- est[, "CountryName"]
reg.c  <- est[, "MDGRegion1" ] # MDG regionS
C      <- length(iso.c)

# IGME 2013 estimates: we work with Q's on original scale, not *Qunit!
Q5.ct <- as.matrix(est[, paste(ageGroupName.a[A], floor(years.t), sep = ".")] / Qunit)
Q1.ct <- as.matrix(est[, paste(ageGroupName.a[1], floor(years.t), sep = ".")] / Qunit)
Q4.ct <- GetQ4fromQ15(q1 = Q1.ct, q5 = Q5.ct)


#---------- updated later when NAs are removed ----------#
iso.i    <- dataset[, "Country.Code"  ]
s.i      <- dataset[, "Sex.Ratio"     ]
year.i   <- dataset[, "Reference.Date"]
agecat.i <- dataset[, "agecat.i"      ]
I        <- length(iso.i)
#---------- updated later when NAs are removed ----------#

###########
## part 2a: assign IGME 2013 estimates of IMR and U5MR as model input
# by matching to country-year of SR data base
exclude.i <- rep(TRUE, length(iso.i))
estyear.i <- floor(year.i) + 0.5
reg.i <- Q1.i <- Q5.i <- Q4.i <- rep(NA, I)
for (i in 1:I) { # to find one-to-one match
  if (sum(iso.c == iso.i[i]) == 1 & 
        sum(years.t == estyear.i[i]) == 1) { 
    Q1.i[i] <-  Q1.ct[iso.c == iso.i[i], years.t == estyear.i[i]]
    Q4.i[i] <-  Q4.ct[iso.c == iso.i[i], years.t == estyear.i[i]]
    Q5.i[i] <-  Q5.ct[iso.c == iso.i[i], years.t == estyear.i[i]]
    reg.i[i] <- paste(reg.c[iso.c == iso.i[i]])
  }
  
  if (agecat.i[i] == ages.a[1] & !is.na(Q1.i[i])) {
    exclude.i[i] <- FALSE
  }
  if (agecat.i[i] == ages.a[2] & !is.na(Q4.i[i])) {
    exclude.i[i] <- FALSE
  }
  if (agecat.i[i] == ages.a[A] & !is.na(Q4.i[i]) & !is.na(Q1.i[i])) {
    exclude.i[i] <- FALSE
  }
}#end of i loop

##########
## part 2b: remove entries in SR data base if there is no IGME estimate for
# its corresponding mortality
sum(exclude.i) #63
data.frame(iso.i, year.i)[exclude.i, ]
s.i[exclude.i] <- NA # set obs without country-years.t Q's to NA

# REMOVE the rows with s.i=NA
Q1.i     <- Q1.i   [!is.na(s.i)]
Q4.i     <- Q4.i   [!is.na(s.i)] #; Q4.i.oringin <- Q4.i
Q5.i     <- Q5.i   [!is.na(s.i)]
reg.i    <- reg.i  [!is.na(s.i)]
data.all <- dataset[!is.na(s.i), ]
dim(data.all) #10084    48
# data.final <- data.frame(data.all,
#                          Q1.i = Q1.i, Q4.i = Q4.i, Q5.i = Q5.i)
# write.csv(data.final, "data/interim/dataset2013_fullyclean_2014-05-23.csv",
#           row.names = FALSE, na = "")

###########
## part 2c: get sex-specific IMR, CMR, and U5MR
s.i   <- data.all[, "Sex.Ratio"]
Q1m.i <- GetQmale    (qboth = Q1.i, w = w1, s = s.i)
Q1f.i <- GetQfemale  (qmale = Q1m.i, s = s.i)
w4.i  <- GetWeight4  (w1 = w1, q1male = Q1m.i, q1both = Q1.i)
Q4m.i <- GetQmale    (qboth = Q4.i, w = w4.i, s = s.i)
Q4f.i <- GetQfemale  (qmale = Q4m.i, s = s.i)
Q5m.i <- GetQ5fromQ14(q1 = Q1m.i, q4 = Q4m.i)
Q5f.i <- GetQfemale  (qmale = Q5m.i, s = s.i)

agecat.i       <- data.all[, "agecat.i"      ]
name.i         <- data.all[, "Country.Name"  ]
year.i         <- data.all[, "Reference.Date"]
yearfrom.i     <- data.all[, "Year.From"     ]
yearto.i       <- data.all[, "Year.To"       ]
iso.i          <- data.all[, "Country.Code"  ]
method.i       <- data.all[, "method.i"      ]
typename.i     <- data.all[, "typename.i"    ]
typenameplot.i <- data.all[, "typenameforplot.i"]
surveyplot.i   <- data.all[, "surveyplot.i"  ]
surveyyear.i   <- data.all[, "Series.Year"   ]
SEnoimpute.i   <- data.all[, "Sex.Ratio.SE"  ] #before imputing for missing SEs
I              <- length(iso.i)

##########
## part 3: set up training and testing set for validation run
if (Validation.run) {
  if (First.run) {
    
    ## step 1: leave out data after a survey yr so that about 20% data is left out
    survey.end <- ifelse(nchar(surveyyear.i) == 4, 
                         surveyyear.i, 
                         substr(surveyyear.i,nchar(surveyyear.i) - 4 + 1, nchar(surveyyear.i)))
    survey.end <- as.numeric(survey.end)
    survey.end[typename.i=="VR"] <- year.i[typename.i=="VR"] 
    #note: typename.i is for modeling only, typename.i for "VR" is actually VR and SRS!!
    
    mean(survey.end >= yearLeftout) #18.37%
    
    indexte.i <- seq(1, I)[survey.end >= yearLeftout] #testing set indices
    indextr.i <- setdiff(seq(1, I), indexte.i) #training set indices
    length(indextr.i) # 8231
    Indicator.i <- rep("TestingSet", I)
    Indicator.i[indextr.i] <- "TrainingSet"
    
    ## training & testing sets ##
    data.vali <- cbind(data.frame(
      Q1.i = Q1.i, Q4.i = Q4.i, Q5.i = Q5.i,
      s.i = s.i, agecat.i = agecat.i, name.i = name.i,
      year.i = year.i, iso.i = iso.i, reg.i = reg.i,
      method.i = method.i, typename.i = typename.i,
      surveyplot.i = surveyplot.i, surveyyear.i = surveyyear.i,
      SEnoimpute.i = SEnoimpute.i, Indicator.i = Indicator.i
    ))
    write.csv(data.vali, "data/interim/dataset_Validation.csv",
              row.names = FALSE, na = "")
  } else {
    data.vali <- read.csv("data/interim/dataset_Validation.csv",
                          header = TRUE, stringsAsFactors = FALSE)
  }#end of if (First.run)
  
  source.i <- as.numeric(as.factor(data.vali[, "typename.i"]))
  S        <- max(source.i)
  
  Indicator.i <- data.vali[, "Indicator.i"]
  data.vali   <- data.vali[, names(data.vali) != "Indicator.i"]  
  I           <- sum(Indicator.i == "TrainingSet") # 8231
  E           <- sum(Indicator.i == "TestingSet")  # 1853
  variables.v <- names(data.vali)  
  
  for (vari in variables.v) {
    # assign variables for Training set
    assign(vari, data.vali[Indicator.i == "TrainingSet", vari])
    print(vari)
    # assign variables for Testing set: use *.e to indicate left-out
    assign(gsub(".i", ".e", vari, fixed = TRUE), 
           data.vali[Indicator.i == "TestingSet", vari])
    print(gsub(".i", ".e", vari, fixed = TRUE))
  }#end of vari loop
  # note: use "vari" instead of "var" here because var itself is a function!
  
  source.i <- source.i[Indicator.i == "TrainingSet"]
  
  # NOTE for later: for 1-country run, you'll have subsets of these 
  # need to keep track of what parameters to use for sigma of source type
  source.e     <- as.numeric(as.factor(typename.e))
  S.e          <- max(source.e) # same as Main.run (i.e. full set): 8
  
  ## log-scale of s.i and SE.i ##
  logs.e  <- log(s.e)
  logSEnoimpute.e <- 1 / s.e * SEnoimpute.e
  logSE.e         <- ifelse(is.na(logSEnoimpute.e), logSEImpute, logSEnoimpute.e)
  
}#end of if (Validation.run)

###########
## part 4a: get source types and make sure VR is the last type
# NOTE for later: for 1-country run, you'll have subsets of these 
# need to keep track of what parameters to use for sigma of source type
if (!Validation.run) {
  source.i <- as.numeric(as.factor(typename.i))
  S        <- max(source.i)
}

###----------------------------------------------------------------
# VR needs to be last one
# print warning here and at the end
if (prod((source.i == S)[typename.i == "VR"]) != 1) {
  print("STOP: VR is not sourcetype S!")
}
if (Validation.run) {
  if (prod((source.e == S.e)[typename.e == "VR"]) != 1) {
    print("STOP: VR is not sourcetype S for left-out observations!")
  }  
}

# for Main.run, the following shows the full set info
# for Validation.run, the following shows the training set info
table(typename.i, source.i)
table(typename.i, agecat.i)
if (Validation.run) { # testing set info
  # order of soruces are the same for training and testing sets
  table(typename.e, source.e) 
  table(typename.e, agecat.e)  
}
# check if the two tables above are the same as the standard results
if (First.run) file.show("code/info/sourceType.txt") # opens in a new window in R
###----------------------------------------------------------------
###########
## part 4b: get SR and its standard error on log-scale
logs.i  <- log(s.i)
# with delta method: variance(logs.i) = 1/s.i^2*SE.i^2
logSEnoimpute.i <- 1 / s.i * SEnoimpute.i
logSE.i <- ifelse(is.na(logSEnoimpute.i), logSEImpute, logSEnoimpute.i)
# see the details of getting the value 0.15 for logSEImpute
if (First.run) file.show("code/info/getSE.txt") # opens in a new window in R

##############################################
## plot with data serise and non-imputed SE ##
if (First.run) {
  ## plot all countries ##
  pdf(paste0(fig.dir, Sys.Date(), "_dataSeries_FullyCleaned(noimputeSE)",
             ifelse(Validation.run, "_TrainingSet", ""), ".pdf"), 
      height = 12, width = 10)
  par(mfrow = c(3, 2), cex.lab = 2, cex.axis = 2, mar = c(2, 5, 3, 1), 
      oma = c(3, 3, 3, 3))
  for (c in 1:C) {  
    selectCountry.i <- (iso.i == paste(iso.c[c]))
    unique.sources <- 
      unique(surveyplot.i[
        which(selectCountry.i)[order(surveyyear.i[selectCountry.i])]
        ])
    
    for (age in ages.a) {
      selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == age]
      # sort by survey date to get nicer legend
      selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]
      
      PlotCIbandwithDataseries(
        if.xlimFix = TRUE, if.SurveyLegend = TRUE, if.sepLegendPage = TRUE, 
        dataseries = s.i, dataseriesSE = SEnoimpute.i,
        Source = surveyplot.i, baseSeries = "VR",
        x = year.i, select.x = selectCountryAge.i, unique.sources = unique.sources,
        ylab = paste0("S", age), xlab = "Year", cutoff = cutoffS
      )    
    }#end of age loop
    title(main = name.c[c], line = 0, cex.main = 3, outer = TRUE)
  }#end of c loop
  dev.off()  
}#end of if(First.run)

###########
## part 5: setup B-splines on log-scale of IMR, CMR, and U5MR
# round the country Q's and match with the sequence of Q's in splines
# change to using min-max of estimates too!
for (age in ages.a[-A]) {
  res.sp <- GetSplinesResults(
    Qsplines.input = eval(parse(text = paste0("Q", age, ".ct"))),
    Qobserve.input = eval(parse(text = paste0("Q", age, ".i")))
  )
  eval(parse(text = paste0("logQ", age, ".k <- res.sp$logQ.k")))
  eval(parse(text = paste0("k", age, "<- res.sp$k")))
  eval(parse(text = paste0("BG.tm.", age, "<- res.sp$BG.tm")))
  eval(parse(text = paste0("Z.tk.", age, "<- res.sp$Z.tk")))
  eval(parse(text = paste0("Q.", age, "<- res.sp$Q")))
}#end of age loop

if (First.run & Main.run) {
  # plot splines on log-scale with finer grid
  pdf(paste0(fig.dir, "B-splines_logscale.pdf"), height = 10, width = 20)
  par(mfrow = c(2, 1), cex.lab = 2.7, cex.axis = 2.2, cex.main = 4, las = 1,
      mar = c(6, 7.5, 2, 2), mgp = c(4.5, 1, 0))
  for (age in ages.a[-A]) {
    GetSplinesResults(
      Qsplines.input = eval(parse(text = paste0("Q", age, ".ct"))),
      Qobserve.input = eval(parse(text = paste0("Q", age, ".i"))),
      cutoff = 30, plot = TRUE
    )
  }#end of age loop
  dev.off()  
}#end of if

##########
## part 6: get indices for included observations of age group 1,4 and 5
geti.h1 <- seq(1, I)[agecat.i == ages.a[1]] 
geti.h4 <- seq(1, I)[agecat.i == ages.a[2]] 
geti.h5 <- seq(1, I)[agecat.i == ages.a[A]] 
n1 <- length(geti.h1)
n4 <- length(geti.h4)
n5 <- length(geti.h5)

Q4.i[Q4.i < Q4LowerBound] <- Q4LowerBound
t.i <- c.i  <-getk4.i <- getk1.i <- rep(NA, I)

for (i in 1:I) {
  # find country-year for each index
  c.i[i] <- which(iso.c == iso.i[i])
  t.i[i] <- which(years.t == floor(year.i[i]) + 0.5)
}#end of i loop

# implementation below is convoluted, 
# but one Q4.i is missing for a q1 or q5 entry.
for (i in geti.h5) {
  getk4.i[i] <- which((logQ4.k) == log(1 / Qunit * round(Qunit * Q4.i[i])))
  getk1.i[i] <- which((logQ1.k) == log(1 / Qunit * round(Qunit * Q1.i[i])))
}
for (i in geti.h4) {
  getk4.i[i] <- which((logQ4.k) == log(1 / Qunit * round(Qunit * Q4.i[i])))
}
for (i in geti.h1) {
  getk1.i[i] <- which((logQ1.k) == log(1 / Qunit * round(Qunit * Q1.i[i])))
}


##########
## part 7: get indices for non-missing P.ct's to avoid sampling for all P.ct's
# instead, only sample the t.i's that have an observation
gett4.cz <- gett1.cz <- matrix(NA, C, I) # reduce dimension later
nt1.c <- nt4.c <- rep(NA, C)

for (c in 1:C) {
  age <- ages.a[1]
  gett <- sort(unique(t.i[is.element(agecat.i, c(age, ages.a[A])) & iso.i == iso.c[c]]))
  # make sure all countries have at least two t's 
  # (easier in loop in JAGS, and coding later when reconstructing P)
  if (length(gett) == 0) {
    gett <- c(floor((Tend + 1) / 2), floor((Tend + 1) / 2 + 1))
  }
  if (length(gett) == 1 & max(gett) < Tend) {
    gett <- c(gett, gett + 1)
  }
  if (length(gett) == 1 & max(gett) == Tend) {
    gett <- c(gett - 1, gett)
  }
  nt1.c[c] <- length(gett)
  gett1.cz[c, 1:nt1.c[c]] <- gett
  
  age <- ages.a[2]
  gett <- sort(unique(t.i[is.element(agecat.i, c(age, ages.a[A])) & iso.i == iso.c[c]]))
  # make sure all countries have at least two t's 
  # (easier in loop in JAGS, and coding later when reconstructing P)
  if (length(gett) == 0) {
    gett <- c(floor((Tend + 1) / 2), floor((Tend + 1) / 2 + 1))
  }
  if (length(gett) == 1 & max(gett) < Tend) {
    gett <- c(gett, gett + 1)
  }
  if (length(gett) == 1 & max(gett) == Tend) {
    gett <- c(gett - 1, gett)
  }
  nt4.c[c] <- length(gett)
  gett4.cz[c, 1:nt4.c[c]] = gett
}
gett1.cz <- gett1.cz[, 1:max(nt1.c)]
gett4.cz <- gett4.cz[, 1:max(nt4.c)]

# warning again:
if (prod((source.i == S)[typename.i == "VR"]) != 1) {
  print("STOP: VR is not sourcetype S!")
}

##########
## part 8: setup population-related data
if (getQbysex) {
  pop.female <- ReformatPopulationData(
    a1.filename = "a0.csv", 
    pop.filename = "data_female_CMEpopulation_JRformat.csv"
  )
  pop.male   <- ReformatPopulationData(
    a1.filename = "a0.csv", 
    pop.filename = "data_male_CMEpopulation_JRformat.csv"
  )
  
  a1.c     <- pop.female[["a1.c"   ]]
  a4.c     <- pop.female[["a4.c"   ]]
  pop1F.ct <- pop.female[["pop1.ct"]]
  pop4F.ct <- pop.female[["pop4.ct"]]
  pop1M.ct <- pop.male  [["pop1.ct"]]
  pop4M.ct <- pop.male  [["pop4.ct"]]
  
}#end of if (getQbysex)

## The END! ##


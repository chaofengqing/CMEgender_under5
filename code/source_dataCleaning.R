

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# source_dataCleaning.R
# 
# This script cleans the input data in order to get the SR data base.
#
# used for which run: its output data file is used in Main.run, Validation.run,
# and Excl.run. However, it is only called once when doing Main.run.
#
# this script is called by any other scripts: main48_1.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Functions called in the scripts listed above are not listed.
# PlotCIbandwithDataseries(2) - data series plots for checking purpose.
# 
# input data in folder data/input/
# note 1: data files are read in main48_1.R before calling this script.
# note 2: data files 2 - 4 are from country consultation in 2013 July.
# 1. datasetfinal20120624.csv         - inupt data from all countries
# 2. Data Entry-Post-CC_20130806.xlsx - updated data from several countries
# 3. Haiti Gabon All_20130807.xlsx    - updated data from Haiti and Gabon
# 4. Sudan_20130807.xlsx              - updated data from Sudan
# 5. Results.Table_Final_20130812.csv - IGME 2013 estimates file
#
# output data in folder data/: is saved in main48_1.R
# 1. interim/dataset_formodeling.csv - the SR database for modelling;
#
# Data cleaning summary in five steps:
# step 1a: set sex ratios outside [extremeSR, 1/extremeSR] to NA;
# step 1b: set any SR with SEs that are Inf to NA;
# step 1c: set any SE that are 0 or Inf to NA;
# 
# step 2a: remove data from countries not in UNICEF file.
# step 2b: remove data from Andorra and Monaco.
# 
# step 3: For indirect series, use s5 only (and exclude s1 and s4).
# Unless s5 is missing, then include s1.
# 
# step 4: use s5 only if s1 is missing.
# 
# step 5: re-categorize Series.Category and type:
# typename is combi of method (seriestype) and sourcetype (category)
# construct typenameforplot.i  and typename.i 
# model categorization differs from plot wrt:
# 1. SRS: in plot, it is labeled as SRS; in model, it is treated as VR data
# because no sampling error variance.
# 2. merge some categories because of too few observations
# 
# note: Commented numbers below are updated on Dec 29, 2013 (verified by CFQ).
# These numbers will change if input data files are updated in the future. 
# 
###############################################################################


############################################
## part 1: plot all data without cleaning ##
method.i <- dataraw[, "Series.Type"]
method.i <- ifelse(is.element(method.i, c("VR", "VR (Single year)")),
                   "", paste(method.i))
typename.i <- ifelse(method.i == "", dataraw[, "Series.Category"],
                     paste(dataraw[, "Series.Category"], method.i))
surveyyear.i <- dataraw[, "Series.Year"]
surveyplot.i <- paste0(typename.i, " (", surveyyear.i, ")")

# need country info for plot
iso.c  <- est[, "ISO3Code"   ]
reg.c  <- est[, "MDGRegion1" ]
name.c <- est[, "CountryName"]
C      <- length(iso.c)

## plot for the raw data ##
pdf(paste0(fig.dir, Sys.Date(), "_dataSeries_AllRaw.pdf"), 
    height = 12, width = 10)
par(mfrow = c(3, 2), cex.lab = 2, cex.axis = 2, mar = c(2, 5, 3, 1), 
    oma = c(3, 3, 3, 3))
for (c in 1:C) {
  selectnotyetageTandF <- (!is.na(dataraw[, "Sex.Ratio"]) 
                           & dataraw[, "Inclusion"   ] == 1
                           & dataraw[, "Country.Code"] == paste(iso.c[c]))
  unique.sources <- 
    unique(surveyplot.i[
      which(selectnotyetageTandF)[order(surveyyear.i[selectnotyetageTandF])]
      ])
  
  for (age in ages.a) {
    a <- which(ages.a == age)
    select <- seq(1, 
                  length(dataraw[, "Country.Code"]))[selectnotyetageTandF  
                                                     & dataraw[, "Indicator"] == 
                                                       c("Infant Mortality Rate", 
                                                         "Child Mortality Rate", 
                                                         "Under-five Mortality Rate")[a]]
    # sort by survey date to get nicer legend
    select <- select[order(surveyyear.i[select])]
    if (length(select) == 0) {
      print(paste0(name.c[c], " has no raw data for age group ", age))
    }
    PlotCIbandwithDataseries(
      if.SurveyLegend = TRUE, if.sepLegendPage = TRUE, if.xlimFix = TRUE,
      dataseries = dataraw[, "Sex.Ratio"], unique.sources = unique.sources,
      dataseriesSE = dataraw[, "Sex.Ratio.SE"],
      Source = surveyplot.i, #baseSeries = "VR",
      x = dataraw[, "Reference.Date"], select.x = select,
      ylab = paste0("S", age), xlab = "Year", cutoff = cutoffS
    )    
  }#end of age loop
  title(main = name.c[c], line = 0, cex.main = 3, outer = TRUE)
}#end of c loop
dev.off()


######################
## part 2: cleaning ##
######################

data.all <- dataraw

##########
## step 1a: Remove SR outside [extremeSR, 1/extremeSR]
data.all[which(data.all[, "Sex.Ratio"] > 1 / extremeSR), "Sex.Ratio"   ] <- NA
data.all[which(data.all[, "Sex.Ratio"] < extremeSR    ), "Sex.Ratio"   ] <- NA

## step 1b: set any SR with SEs that are Inf to NA
data.all[which(data.all[, "Sex.Ratio.SE"] == Inf      ), "Sex.Ratio"   ] <- NA

# step 1c: set any SE that are 0 or Inf to NA
data.all[which(data.all[, "Sex.Ratio.SE"] == Inf      ), "Sex.Ratio.SE"] <- NA
data.all[which(data.all[, "Sex.Ratio.SE"] == 0        ), "Sex.Ratio.SE"] <- NA
sum(is.na(data.all[, "Sex.Ratio"]) | (data.all[, "Inclusion"] == 0)) #6886-1

##########
## step 2a: remove data from countries not in UNICEF file.
unique(data.all[!is.element(data.all[, "Country.Code"], iso.c), "Country.Name"])
# Montserrat, Puerto Rico, Turks and Caicos Islands
isoexclude <- unique(
  data.all[!is.element(data.all[, "Country.Code"], iso.c), "Country.Code"]
  )
data.all[is.element(data.all[, "Country.Code"], isoexclude), "Inclusion"] <- 0
sum(is.na(data.all[, "Sex.Ratio"]) | (data.all[, "Inclusion"] == 0)) #7018-1

## step 2b: remove data from Andorra and Monaco.
sum(is.element(data.all[, "Country.Name"], c("Andorra", "Monaco"))) #132
#all selected data are already with Inclusion == 0
table(data.all[is.element(data.all[, "Country.Name"], 
                          c("Andorra", "Monaco")), "Inclusion"])
data.all[is.element(data.all[, "Country.Name"], 
                    c("Andorra","Monaco")), "Inclusion"] <- 0
sum(is.na(data.all[, "Sex.Ratio"]) | (data.all[, "Inclusion"] == 0)) #7018-1


data.all <- data.all[data.all[, "Inclusion"] == 1, ]
sum(is.na(data.all[, "Sex.Ratio"]) | (data.all[, "Inclusion"] == 0)) == 
  sum(is.na(data.all[, "Sex.Ratio"])) #TRUE

##########
## step 3: For indirect series, use s5 only (and exclude s1 and s4).
## Unless s5 is missing, then include s1.
iso.i  <- data.all[, "Country.Code"  ]
s.i    <- data.all[, "Sex.Ratio"     ]
year.i <- data.all[, "Reference.Date"]

unique(data.all[, "Indicator"]) #stardardize Indicator for age groups 1 and 4
IndicatorClean <- rep(NA, dim(data.all)[1])
IndicatorClean[grep("Infant", data.all[, "Indicator"])] <- "Infant Mortality Rate"
IndicatorClean[grep("Child" , data.all[, "Indicator"])] <- "Child Mortality Rate"
IndicatorClean[grep("Under" , data.all[, "Indicator"])] <- "Under-five Mortality Rate"
unique(IndicatorClean) #"Under-five Mortality Rate","Infant Mortality Rate","Child Mortality Rate"

agecat.i <- ifelse(IndicatorClean == "Infant Mortality Rate", ages.a[1], 
                   ifelse(IndicatorClean == "Child Mortality Rate", ages.a[2], 
                          ages.a[A]))
table(IndicatorClean, agecat.i)
#                                 agecat.i
# IndicatorClean               1    4    5
# Child Mortality Rate         0 5957-1  0
# Infant Mortality Rate     5813    0    0
# Under-five Mortality Rate    0    0 5758

SourceID <- apply(data.all[, c("Country.Name", "Series.Year",
                             "Series.Name", "Series.Type", 
                               "Series.Category")], 1, paste, collapse=" ")

for (sid in unique(SourceID)) {
  if (sum(SourceID == sid & 
            data.all[, "Series.Type"] == "Indirect" & 
            agecat.i == ages.a[A] & !is.na(s.i)
          ) > 0) {
    s.i[SourceID == sid & agecat.i != ages.a[A]] <- NA
  }#end of if
}#end of sid loop
sum(is.na(s.i)) #2679-1

##########
## step 4: use s5 only if s1 is missing
for (sid in unique(SourceID)) {
  if (sum(SourceID == sid & agecat.i == ages.a[1] & !is.na(s.i)) > 0){
    s.i[SourceID == sid & agecat.i == ages.a[A]] <- NA
  }#end of if
}#end of sid loop
sum(is.na(s.i)) #7381-1

data.all <- cbind(data.all, agecat.i, IndicatorClean, SourceID)
dim(data.all) #17528-1    43
data.all <- data.all[data.all[,"Inclusion"] == 1 & !is.na(s.i), ]
dim(data.all) #10147    43

##########
## step 5: re-categorize Series.Category and type ##
# typename is combi of method (seriestype) and sourcetype (category)
# construct typenameforplot.i  and typename.i 
# model categorization differs from plot wrt:
# 1. SRS: in plot, noted as SRS, in model, treated as VR because no sampling error variance
# 2. merge some cats because too few observations

unique(data.all[, "Series.Type"])
# "Direct"               
# "Indirect"             
# "VR"                   
# "Direct (Single year)" 
# "Direct (5 year)"      
# "Household Deaths"
method.i <- data.all[, "Series.Type"]
method.i <- ifelse(is.element(method.i, c("VR", "VR (Single year)")), "", 
                   ifelse(method.i == "Household Deaths", "Direct",
                          ifelse(is.element(method.i, 
                                            c("Direct (Single year)",
                                              "Direct (Various periods)",
                                              "Direct (5 year)")),
                                 "Direct", paste(method.i))))
unique(method.i) 
# "Direct"
# "Indirect"
# ""

unique(data.all[, "Series.Category"]) #16 types
# "DHS"         
# "Others"      
# "MICS"        
# "RHS"         
# "PAP"         
# "VR"          
# "MIS"         
# "NDHS"        
# "Special DHS" 
# "NMICS"       
# "SVR"         
# "WFS"
# "Census"      
# "AIS"         
# "Panel"       
# "Interim DHS"
sourcetype.i <- data.all[, "Series.Category"]
sourcetype.i <- ifelse(sourcetype.i == "SVR", "SVR",
                       ifelse(data.all[,"Series.Type"] == "VR", "VR",
                              ifelse(is.element(sourcetype.i, 
                                                c("DHS")), "DHS",
                                     ifelse(is.element(sourcetype.i, 
                                                       c("Interim DHS", 
                                                         "Special DHS", 
                                                         "AIS", "MIS", 
                                                         "NDHS", "WFS")), 
                                            "Other DHS",
                                            ifelse(sourcetype.i == "MICS", 
                                                   "MICS",
                                                   ifelse(sourcetype.i == "Census", 
                                                          "Census",
                                                          "Others"))))))# RHS, PAP, NMICS, Panel in Others!
sourcetype.i <- ifelse(is.element(data.all[, "Series.Type"], 
                                  c("Household Deaths", "Life Table")),
                       "Others", sourcetype.i)
unique(sourcetype.i) #7 types
# "DHS"
# "Others"
# "MICS"
# "VR"
# "Other DHS"
# "SVR"
# "Census"
typename.i <- paste(sourcetype.i, method.i)
# remove the spaces introduced for VR and SVR
typename.i[typename.i == "VR " ] <- "VR"
typename.i[typename.i == "SVR "] <- "SRS" 

## typename for plot only ##
typenameforplot.i <- typename.i
unique(typenameforplot.i)
surveyplot.i <- paste0(typenameforplot.i, 
                       paste0(ifelse(!is.element(typenameforplot.i, 
                                                 c("VR", "SRS")),                                      
                                     paste0(" (", data.all[, "Series.Year"],
                                            ")"), "")))
#note: SRS is collected in the same way as VR (on a continuous basis) 
#so should also use reference date instead of series.year in the input file.
unique(surveyplot.i)
# for model

# set SRS to VR
typename.i[typename.i == "SRS"] <- "VR" 
# merge few types since some categories have too few number of obs
table(typename.i, data.all[, "agecat.i"])
# typename.i            1    4    5
# Census Direct         6    4    0
# Census Indirect       0    0   90
# DHS Direct         1301 1024    0
# DHS Indirect          0    0   10
# MICS Direct          79   56    0
# MICS Indirect         0    0  335
# Other DHS Direct    319  267    0
# Other DHS Indirect    1    1   25
# Others Direct       140  143    0
# Others Indirect       4    4  166
# VR                 3141 3019   12

typename.i[typename.i == "DHS Indirect" | 
             typename.i == "Other DHS Indirect"] <- "Others Indirect"
typename.i[typename.i == "Census Direct"] <- "Others Direct" 
typename.i[typename.i == "Others Indirect" & 
             data.all[, "agecat.i"] != ages.a[A]] <- "Others Direct" 
table(typename.i, data.all[, "agecat.i"])
# typename.i          1    4    5
# Census Indirect     0    0   90
# DHS Direct       1301 1024    0
# MICS Direct        79   56    0
# MICS Indirect       0    0  335
# Other DHS Direct  319  267    0
# Others Direct     151  152    0
# Others Indirect     0    0  201
# VR               3141 3019   12

data.all <- cbind(data.all, sourcetype.i, method.i, typename.i,
                  typenameforplot.i, surveyplot.i)
dim(data.all) #10147    48

# plot half-cleaned data
pdf(paste0(fig.dir, Sys.Date(), "_dataSeries_HalfCleaned.pdf"), 
    height = 12, width = 10)
par(mfrow = c(3, 2), cex.lab = 2, cex.axis = 2, mar = c(2, 5, 3, 1), 
    oma = c(3, 3, 3, 3))
for (c in 1:C) {

  selectnotyetageTandF <- (!is.na(data.all[, "Sex.Ratio"]) & 
                             data.all[, "Inclusion"] == 1 & 
                             data.all[, "Country.Code"] == paste(iso.c[c]))
  unique.sources <- 
    unique(surveyplot.i[
      which(selectnotyetageTandF)[order(surveyyear.i[selectnotyetageTandF])]
      ])
  
  for (age in ages.a) {
    a <- which(ages.a == age)
    select <- seq(1, dim(data.all)[1])[selectnotyetageTandF & 
                                         data.all[, "agecat.i"] == age]
    # sort by survey date to get nicer legend
    select <- select[order(surveyyear.i[select])]
    if (length(select) == 0) {
      print(paste0(name.c[c], " has no cleaned data for age group ", age))
    }
    PlotCIbandwithDataseries(
      if.SurveyLegend = TRUE, if.sepLegendPage = TRUE, if.xlimFix = TRUE,
      dataseries = data.all[, "Sex.Ratio"],
      dataseriesSE = data.all[, "Sex.Ratio.SE"],
      Source = surveyplot.i, #data.all[, "surveyplot.i"],
      baseSeries = "VR", unique.sources = unique.sources,
      x = data.all[, "Reference.Date"], select.x = select,
      ylab = paste0("S", age), xlab = "Year", cutoff = cutoffS
    )    
  }#end of age loop
  title(main = name.c[c], line = 0, cex.main = 3, outer = TRUE)
}#end of c loop
dev.off()

##########
## step 9: save cleaned data without NA for s.i
sum(data.all[, "Inclusion"] == 1 &
      !is.na(data.all[, "Sex.Ratio"])) #10147
sum(data.all[, "Inclusion"] == 1 &
      is.na(data.all[, "Exclusion.External.Info"]) &
      !is.na(data.all[,"Sex.Ratio"])) #10147
sum(data.all[, "Inclusion"] == 0 |
      is.na(data.all[, "Sex.Ratio"])) #0

dataset <- data.all # saved outside this script; see main48_1.R

## the end ##



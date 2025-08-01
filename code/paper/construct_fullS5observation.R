

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_fullS5observation.R
# 
# This script create full dataset for observed S5 with normal data cleaning
# procedures,but keep those excluded becuase of the existence of s1 or s4. Not
# used for modelling, only for paper plot.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main48_output.R
#
# this script calls other scripts: null
#
# functions called: null
# 
# input data in folder data/input/
# note: data files 2 - 4 are from country consultation in 2013 July.
# 1. datasetfinal20120624.csv         - inupt data from all countries
# 2. Data Entry-Post-CC_20130806.xlsx - updated data from several countries
# 3. Haiti Gabon All_20130807.xlsx    - updated data from Haiti and Gabon
# 4. Sudan_20130807.xlsx              - updated data from Sudan
#
# output data in folder data/:
# 1. interim/dataset_S5all.csv
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
##--------------------------------------------THIS PART IS OMITTED
# step 4: use s5 only if s1 is missing.
##--------------------------------------------THIS PART IS OMITTED
# 
# step 5: re-categorize Series.Category and type:
# typename is combi of method (seriestype) and sourcetype (category)
# construct typenameforplot.i  and typename.i 
# model categorization differs from plot wrt:
# 1. SRS: in plot, it is labeled as SRS; in model, it is treated as VR data
# because no sampling error variance.
# 2. merge some categories because of too few observations
# 
# note: Commented numbers and steps below may not updated to most recent file.
# These numbers will change if input data files are updated in the future. Most
# parts are the same as source_dataCleaning.R, should use some function to 
# reduce the repeated part for next round of poject!
# 
###############################################################################



######################
## part 2: cleaning ##
######################

# combine and reformat input data files in folder: data/input/
data.all <- GetInputData(
  inputBaseData.name = "datasetfinal20120624.csv",
  inputData.name1 = "Data Entry-Post-CC_20130806.xlsx",
  inputData.name2 = "Haiti Gabon All_20130807.xlsx",
  inputData.name3 = "Sudan_20130807.xlsx"
)  


method.i <- data.all$Series.Type
method.i <- ifelse(is.element(method.i, c("VR", "VR (Single year)")),"", paste(method.i))
unique(method.i)
typename.i <- paste(data.all$Series.Category, method.i)
unique(typename.i)
surveyyear.i <- data.all$Series.Year
surveyplot.i <- paste0(typename.i," (",surveyyear.i,")")

Sex.Ratio <- as.numeric(as.character(data.all$Sex.Ratio))
data.all$Sex.Ratio[setdiff(which(is.na(Sex.Ratio)),which(is.na(data.all$Sex.Ratio)))]
data.all$Sex.Ratio <- Sex.Ratio

# need country info for plot
iso.c <- as.character(est$ISO3Code)
name.c <- est$CountryName
C <- length(iso.c)

##########
## step 1: Remove SR outside [extremeSR, 1/extremeSR] 
data.all$Sex.Ratio[data.all$Sex.Ratio > 1/extremeSR] <- NA
data.all$Sex.Ratio[data.all$Sex.Ratio < extremeSR] <- NA
data.all$Sex.Ratio[data.all$Sex.Ratio.SE==Inf] <- NA
data.all$Sex.Ratio.SE[data.all$Sex.Ratio.SE==Inf] <- NA
data.all$Sex.Ratio.SE[data.all$Sex.Ratio.SE==0] <- NA
sum(is.na(data.all$Sex.Ratio) | data.all$Inclusion==0) #6886

##########
## step 2: remove data from countries not in UNICEF file ##
unique(data.all$Country.Name)[!is.element(unique(data.all$Country.Code), iso.c)]
#Montserrat, Puerto Rico, Turks and Caicos Islands
isoexclude <- unique(data.all$Country.Code)[!is.element(unique(data.all$Country.Code), iso.c)]
data.all$Inclusion[is.element(data.all$Country.Code, isoexclude)] <- 0
sum(is.na(data.all$Sex.Ratio) | data.all$Inclusion==0) #7018


## step 2a: remove data from Andorra and Monaco ##
unique(data.all$Country.Name)[is.element(unique(data.all$Country.Name), c("Andorra", "Monaco"))]
data.all$Inclusion[is.element(data.all$Country.Name, c("Andorra", "Monaco"))] <- 0
sum(is.na(data.all$Sex.Ratio) | data.all$Inclusion==0) #7018



data.all <- data.all[data.all$Inclusion==1,]
sum(is.na(data.all$Sex.Ratio) | data.all$Inclusion==0)==sum(is.na(data.all$Sex.Ratio))

##########
## step 6: For indirect series, use s5 only (and exclude s1 and s4) 
## UNLESS s5 is missing, then include s1
iso.i <- data.all$Country.Code
s.i <- data.all$Sex.Ratio
year.i <- as.numeric(data.all$Reference.Date)

unique(as.character(data.all$Indicator)) #stardardize Indicator for age groups 1 and 4
IndicatorClean <- rep(NA,length(data.all$Indicator))
IndicatorClean[grep("Infant",data.all$Indicator)] <- "Infant Mortality Rate"
IndicatorClean[grep("Child",data.all$Indicator)] <- "Child Mortality Rate"
IndicatorClean[grep("Under",data.all$Indicator)] <- "Under-five Mortality Rate"
unique(IndicatorClean) #"Under-five Mortality Rate","Infant Mortality Rate","Child Mortality Rate"

agecat.i <- ifelse(IndicatorClean=="Infant Mortality Rate",1, 
                   ifelse(IndicatorClean=="Child Mortality Rate", 4, 5))
table(IndicatorClean, agecat.i)

SourceID <- apply(data.all[,c("Country.Name","Series.Year",
                              "Series.Name","Series.Type","Series.Category")],1,paste,collapse=" ")

for (sourceid in unique(SourceID)){
  if (sum(SourceID==sourceid & data.all$Series.Type=="Indirect" & agecat.i==5 & !is.na(s.i))>0){
    s.i[SourceID==sourceid & agecat.i!=5] = NA
  }
}
sum(is.na(s.i)) 

##---------------------------THIS PART IS OMITTED ---------------------------##
# ##########
# ## step 7: use s5 only if s1 is missing
# for (sourceid in unique(SourceID)){
#   if (sum(SourceID==sourceid & agecat.i==1 & !is.na(s.i))>0){
#     s.i[SourceID==sourceid & agecat.i==5] = NA
#   }
# }
# sum(is.na(s.i)) 
##---------------------------THIS PART IS OMITTED ---------------------------##

data.all <- cbind(data.all, agecat.i, IndicatorClean, SourceID)
dim(data.all)
data.all <- data.all[data.all$Inclusion==1 & !is.na(s.i),]
dim(data.all) #14849 43

##########
## step 8: re-categorize Series.Category and type ##
# typename is combi of method (seriestype) and sourcetype (category)
# construct typenameforplot.i  and typename.i 
# model categorization differs from plot wrt:
# 1. SRS: in plot, noted as SRS, in model, treated as VR because no sampling error variance
# 2. merge some cats because too few observations

as.character(unique(data.all$Series.Type)) #"Direct","Indirect","VR","Household Deaths"
method.i <- data.all$Series.Type
method.i <- ifelse(is.element(method.i, c("VR", "VR (Single year)")),"", 
                   ifelse(data.all$Series.Type == "Household Deaths", "Direct",
                          ifelse(is.element(method.i, c("Direct (Single year)","Direct (Various periods)","Direct (5 year)")),"Direct",paste(method.i))))
unique(method.i) #"Direct","Indirect",""

as.character(unique(data.all$Series.Category)) #16 types
sourcetype.i <- data.all$Series.Category
sourcetype.i <- ifelse(data.all$Series.Category == "SVR", "SVR",
                       ifelse(data.all$Series.Type == "VR", "VR",
                              ifelse(is.element(data.all$Series.Category, c("DHS")), "DHS",
                                     ifelse(is.element(data.all$Series.Category, c("Interim DHS", "Special DHS", "AIS", "MIS", "NDHS", "WFS")), "Other DHS",
                                            ifelse(data.all$Series.Category == "MICS", "MICS",
                                                   ifelse(data.all$Series.Category == "Census", "Census",
                                                          "Others"))))))# RHS, PAP, NMICS, Panel in Others!
sourcetype.i <- ifelse(is.element(data.all$Series.Type, c("Household Deaths", "Life Table")),
                       "Others", sourcetype.i)
unique(sourcetype.i) #7 types:"DHS","Others","MICS","VR","Other DHS","SVR","Census"
typename.i <- paste(sourcetype.i, method.i)
# remove the spaces introduced for VR and SVR
typename.i[typename.i=="VR "] <- "VR"
typename.i[typename.i=="SVR "] <- "SRS" 


## typename for plot only ##
typenameforplot.i <- typename.i
unique(typenameforplot.i)
surveyplot.i <- paste(typenameforplot.i, 
                      paste(ifelse(!is.element(typenameforplot.i, c("VR", "SRS")),                                      
                                   paste0(" (",data.all$Series.Year,")"), ""), sep = ""))
#note: SRS is collected in the same way as VR (on a continuous basis) 
#so should also use reference date instead of series.year in the input file.
unique(surveyplot.i)
# for model

# set SRS to VR
typename.i[typename.i=="SRS"] <- "VR" 
# merge few types since some categories have too few number of obs
table(typename.i,data.all$agecat.i)
typename.i[typename.i=="DHS Indirect"|typename.i=="Other DHS Indirect"] <- "Others Indirect"
typename.i[typename.i=="Census Direct"] <- "Others Direct" 
typename.i[typename.i=="Others Indirect" & data.all$agecat.i!=5] <- "Others Direct" 
table(typename.i,data.all$agecat.i)

####################################
# typename.i          1    4    5
# Census Indirect     0    0   90
# DHS Direct       1301 1024 1030
# MICS Direct        79   56  112
# MICS Indirect       0    0  335
# Other DHS Direct  319  267  291
# Others Direct     151  152  135
# Others Indirect     0    0  201
# VR               3141 3019 3146
####################################

data.all <- cbind(data.all,sourcetype.i, method.i, typename.i,surveyplot.i)
dim(data.all) 

##########
## step 9: save cleaned data without NA for s.i
sum(data.all$Inclusion==1 & !is.na(data.all$Sex.Ratio))
sum(data.all$Inclusion==1 & is.na(data.all$Exclusion.External.Info) & !is.na(data.all$Sex.Ratio))
sum(data.all$Inclusion==0 | is.na(data.all$Sex.Ratio))
dataset <- data.all

est <- read.csv("data/interim/IGME.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

# some settings
# set years max based on Q5 estimates
iso.c  <- est$ISO3Code
reg.c  <- est[,"MDGRegion1"]
name.c <- est$CountryName
C      <- length(iso.c)
# 'truth'
# note: we work with Q's on original scale, not *1000!
Q5.ct <- as.matrix(est[, paste("U5MR", floor(years.t), sep = ".")]/Qunit)
Q1.ct <- as.matrix(est[, paste("IMR", floor(years.t), sep = ".")]/Qunit)
Q4.ct <- GetQ4fromQ15(q1 = Q1.ct, q5 = Q5.ct)


#---------- updated later when NAs are removed ----------#
iso.i <- as.character(dataset$Country.Code)
I <- length(iso.i)
s.i <- dataset$Sex.Ratio
year.i <- dataset$Reference.Date
agecat.i <- dataset$agecat.i
#---------- updated later when NAs are removed ----------#

##################################
## get Q1/4/5.i match to Q.ct's ##
exclude.i <- rep(TRUE, length(iso.i))
estyear.i = (floor(year.i)+0.5)
reg.i <- Q1.i <- Q5.i <- Q4.i <- rep(NA, I)
for (i in 1:I){
  #FQ20130616 in order to find one-to-one match
  if (sum(iso.c==iso.i[i])==1 & sum(years.t == estyear.i[i])==1){
    Q1.i[i] <-  Q1.ct[iso.c==iso.i[i], years.t == estyear.i[i]]
    Q4.i[i] <-  Q4.ct[iso.c==iso.i[i], years.t == estyear.i[i]]
    Q5.i[i] <-  Q5.ct[iso.c==iso.i[i], years.t == estyear.i[i]]
    reg.i[i] <- paste(reg.c[iso.c==iso.i[i]])
  }
  
  if (agecat.i[i]==1 & !is.na(Q1.i[i])) exclude.i[i] <- FALSE
  if (agecat.i[i]==4 & !is.na(Q4.i[i])) exclude.i[i] <- FALSE
  if (agecat.i[i]==5 & !is.na(Q4.i[i])& !is.na(Q1.i[i])) exclude.i[i] <- FALSE
}
sum(exclude.i)#60 #77
data.frame(iso.i, year.i)[exclude.i,]
s.i[exclude.i] <- NA # set obs without country-years Q's to NA

# REMOVE the rows with s.i=NA
Q1.i     <- Q1.i[!is.na(s.i)]
Q4.i     <- Q4.i[!is.na(s.i)]
Q5.i     <- Q5.i[!is.na(s.i)]
reg.i    <- reg.i[!is.na(s.i)]
data.all <- cbind(data.frame(dataset[!is.na(s.i),], 
                       Q1.i, Q4.i, Q5.i, reg.i))
dim(data.all) 
write.csv(data.all, "data/interim/dataset_S5all.csv", row.names = FALSE, na = "")

## the end ##



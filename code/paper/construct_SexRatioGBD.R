

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_SexRatioGBD.R
# 
# This script creates csv file for sex ratio estimates from GBD.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main48_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetS(1)
# GetQbothfromQmale(1)
# 
# input data from folder data/
# 1. output/M49/countryCI/cis_M49_2012.rda
# 2. inputforpaper/IHME_GBD_2010_MORTALITY_AGE_BY_COUNTRY_1970_2010_0.csv
# note: IHME_GBD_2010_MORTALITY_AGE_BY_COUNTRY_1970_2010_0.csv is not used for
# modeling and is only used to construct plot for paper, hence it is from the
# folder inputforpaper/.
#
# output data: data/output/M49/Results_SexRatioIGMEandGBD.csv
# 
###############################################################################

## input file ##
ihme.df <- read.csv(
  "data/inputforpaper/IHME_GBD_2010_MORTALITY_AGE_BY_COUNTRY_1970_2010_0.csv", 
  header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

colname.ResultMatrix <- c(
  "ISO.Code", "Country.Name", "Region.Name", "Year",
  "SexRatio.IGME", "U5MR.IGME", "U5MR.IGME.male", "U5MR.IGME.female", 
  "SexRatio.IHME", "U5MR.IHME", "U5MR.IHME.male", "U5MR.IHME.female"
  )
ResultMatrix <- matrix(NA, nr = C * Tend, nc = length(colname.ResultMatrix))
colnames(ResultMatrix) <- colname.ResultMatrix

ResultMatrix[, "ISO.Code"    ] <- rep(iso.c,  each = Tend)
ResultMatrix[, "Country.Name"] <- rep(name.c, each = Tend)
ResultMatrix[, "Region.Name" ] <- rep(reg.c,  each = Tend)
ResultMatrix[, "Year"        ] <- rep(years.t,  time = C   )

ResultMatrix[, "SexRatio.IGME"   ] <- c(t(res[["S5.cqt" ]][, 2, ]))
ResultMatrix[, "U5MR.IGME"       ] <- c(t(res[["Q5.cqt" ]][, 2, ]))
ResultMatrix[, "U5MR.IGME.male"  ] <- c(t(res[["Q5m.cqt"]][, 2, ]))
ResultMatrix[, "U5MR.IGME.female"] <- c(t(res[["Q5f.cqt"]][, 2, ]))

C.ihme <- dim(ihme.df)[1]
for (c in 1:C.ihme) {
  ## which row to fall in ResultMatrix
  rowResultMatrix.c <- which(
    ihme.df[c, "iso3"] == ResultMatrix[, "ISO.Code"] &
    ihme.df[c, "year"] == floor(as.numeric(ResultMatrix[, "Year"]))
    )
  
  if (ihme.df[c, "sex_name"] == "Male") {
    ResultMatrix[rowResultMatrix.c, "U5MR.IHME.male"  ] <- 
      ihme.df[c, "mort_5q0"]
  } else {
    ResultMatrix[rowResultMatrix.c, "U5MR.IHME.female"  ] <- 
      ihme.df[c, "mort_5q0"]    
  }  
}#end of c loop

ResultMatrix[, "SexRatio.IHME"] <- GetS(
  as.numeric(ResultMatrix[, "U5MR.IHME.male"]), 
  as.numeric(ResultMatrix[, "U5MR.IHME.female"]))
ResultMatrix[, "U5MR.IHME"] <- GetQbothfromQmale(
  as.numeric(ResultMatrix[, "U5MR.IHME.male"]),
  as.numeric(ResultMatrix[, "SexRatio.IHME"]))

write.csv(ResultMatrix, 
          file = paste0(output.dir, "Results_SexRatioIGMEandGBD.csv"), 
          row.names = FALSE, na = "")

## the end ##


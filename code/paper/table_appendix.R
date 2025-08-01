

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# table_appendix.R
# 
# This script is to print out latex script of Table 1, 3, 4 in appendix.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: null
# 
# input data: data/output/:
# 1. /M49_vali/Results_CoverageOneperCountrySummary_TestingSet.xlsx;
# 2. /M49_vali/Results_CoveragePickYearSummary_TrainingSet.csv;
#
# output data: latex script of tables in Appendix Table 1, 3, 4
#
###############################################################################


#########################
## table 1 in Appendix ##
## table for data in source type-age groups ##
#run after run source_callforoutput.R
data.df <- table(typenameplot.i, agecat.i) #FQ20140617: change typename to typenameplot
colnames(data.df) <- c("age group [0,1)","age group [1,5)", "age group [0,5)")
data.table <- xtable(data.df,
                     caption = paste("Observations are grouped by source type.",
                                     "The data is broken down by age groups."),
                     label = "tb-data")
align(data.table) <- "|r|r|r|r|"
print(data.table, hline.after = c(0:(nrow(data.df) - 1)))

#########################
## table 3 in Appendix ##
row.order <- c(5:7, 3, 4)
file <- "Results_CoverageOneperCountrySummary_TestingSet.xlsx"
age1.df <- read.xlsx("data/output/M49_vali/", file, sheetName="age group 1")
age4.df <- read.xlsx("data/output/M49_vali/", file, sheetName="age group 4")

age1 <- age1.df[row.order,"all.regions"]
age4 <- age4.df[row.order,"all.regions"]

table2.df <- cbind(age1, age4)
dimnames(table2.df)[[2]] <- c("age group [0,1)","age group [1,5)")
dimnames(table2.df)[[1]] <- age4.df[row.order,1]

table2 <- xtable(table2.df, 
                 caption = paste("comparison between estimates obtained based on the full,",
                                 "and estimates based on the training set."),
                 label = "tb-val-res-ppd")
digits(table2) <- rbind(matrix(2, 3, 3), matrix(1, 2, 3))
align(table2) <- "|l|l|l|"

print(table2)

#########################
## table 4 in Appendix ##
table3.df <- read.csv("data/output/M49_vali/Results_CoveragePickYearSummary_TrainingSet.csv")

table3 <- xtable(table3.df, 
                 caption = "changes in estimates for selected years.",
                 label = "tb-val-res-est")
digits(table3) <- rbind(matrix(2, 3, 8), matrix(1, 2, 8))
align(table3) <- "|l|p{3cm}|l|l|l|l|l|l|"
print(table3, include.colnames = FALSE, include.rownames = FALSE)

table3.colnames <- rbind(rep(
  c("age group [0,1)","age group [1,5)", "age group [0,5)"), each = 2),
                         paste0(" (",rep(c(2000, 2005), times = A), ")"))

print(xtable(table3.colnames), include.rownames = FALSE, include.colnames = FALSE)

## the end ##



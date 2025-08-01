

## Lancet Global Health appendix table ##
# a table with all countries that were selected as outlying for IMR and/or CMR
# and/or U5MR (separately for lower and higher than expected) for 1990 and/or
# 2012 (so any countries that is included in any of the tables) and indicate
# what age group was outlying in what year
years <- c(1990, 2012)
table.df <- matrix("", nr = C, nc = length(years) * A)
dimnames(table.df)[[1]] <- name.c
dimnames(table.df)[[2]] <- paste(rep(ageGroupName.a, length(years)), rep(years, each = A))

for (a in 1:A) {
  # load indicies for outlying countries
  load(file = paste0(output.dir, "indicesforexcess.", ages.a[a], ".all.rda"))
  eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", ages.a[a], ".all")))
  
  for (year in years) {
    table.df[, paste(ageGroupName.a[a], year)] <-
      ifelse(indicesforexcess[[paste0("f", year, ".all")]][name.c],
             paste0("", "$\\times$"), table.df[, paste(ageGroupName.a[a], year)])
  }#end of year loop
}#end of a loop

# only select oulying countries
select.c <- (apply(table.df == "", 1, prod) == 0)
table.female.df <- cbind(ExternalMakeCountryNamesFull(name.c)[select.c],
                         table.df[select.c, ])

##
years <- c(1990, 2012)
table.df <- matrix("", nr = C, nc = A)
dimnames(table.df)[[1]] <- name.c
dimnames(table.df)[[2]] <- ageGroupName.a

for (a in 1:A) {
  # load indicies for outlying countries
  load(file = paste0(output.dir, "indicesforexcess.", ages.a[a], ".all.rda"))
  eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", ages.a[a], ".all")))
  
  for (year in years) {
    eval(parse(text = paste0("syb <- sig.", year, ".syb")))
    table.df[, ageGroupName.a[a]] <-
      ifelse(indicesforexcess[[paste0("m", year, ".all")]][name.c],
             paste0(table.df[, ageGroupName.a[a]], syb),
             table.df[, ageGroupName.a[a]])
  }#end of year loop
}#end of a loop

# only select oulying countries
select.c <- (apply(table.df == "", 1, prod) == 0)
table.male.df <- cbind(ExternalMakeCountryNamesFull(name.c)[select.c],
                       table.df[select.c, ])

# table.df <- matrix("", nr = C, nc = length(years) * A)
# dimnames(table.df)[[1]] <- name.c
# dimnames(table.df)[[2]] <- paste(rep(ageGroupName.a, length(years)), rep(years, each = A))
# 
# for (a in 1:A) {
#   # load indicies for outlying countries
#   load(file = paste0(output.dir, "indicesforexcess.", ages.a[a], ".all.rda"))
#   eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", ages.a[a], ".all")))
#   
#   for (year in years) {
#     table.df[, paste(ageGroupName.a[a], year)] <-
#       ifelse(indicesforexcess[[paste0("m", year, ".all")]][name.c],
#              paste0("", "$\\times$"), table.df[, paste(ageGroupName.a[a], year)])
#   }#end of year loop
# }#end of a loop
# 
# # only select oulying countries
# select.c <- (apply(table.df == "", 1, prod) == 0)
# table.male.df <- cbind(ExternalMakeCountryNamesFull(name.c)[select.c],
#                          table.df[select.c, ])


table <- xtable(table.female.df, 
                caption = paste(
                  "\\bf{Countries with outlying sex ratios and",
                  "higher-than-expected female mortality in 1990 and/or 2012",
                  "for IMR, CMR, and U5MR respectively.}",
                  "\\textmd{Countries are ordered alphabetically.}"),
                label = "tb-sup-OutlyingCountry-female",
                align = "|l|p{4cm}||c|c|c||c|c|c|" # the 1st entry is for rownames
)
print(table, tabular.environment = 'longtable', floating = FALSE,
      include.colnames = FALSE, include.rownames = FALSE,
      hline.after = c(1:(nrow(table) - 1)), size = "small",
      sanitize.text.function=function(x){x} #display formula!!
)

table <- xtable(table.male.df, 
                caption = paste(
                  "\\bf{Overview of countries with lower-than-expected",
                  "female IMR and/or female CMR and/or female U5MR",
                  "for 1990 and/or 2012.}",
                  "\\textmd{",
                  sig.1990.syb, ": Sex ratio is outlying in 1990.",
                  sig.2012.syb, ": Sex ratio is outlying in 2012.",
                  "Countries are ordered alphabetically.}"),
                label = "tb-sup-OutlyingCountry-male",
                align = "|l|p{3.5cm}||c|c|c|" # the 1st entry is for rownames
)
# REPLACE align in the print-out
# align = "|l|p{3.5cm}||>{\centering\arraybackslash}m{1.5cm}|>{\centering\arraybackslash}m{1.5cm}|>{\centering\arraybackslash}m{1.5cm}|" # the 1st entry is for rownames

print(table, tabular.environment = 'longtable', floating = FALSE,
      include.colnames = FALSE, include.rownames = FALSE,
      hline.after = c(1:(nrow(table) - 1)), #size = "small",
      sanitize.text.function=function(x){x} #display formula!!
)

# paste column names and info after \begin{longtable}
# \hline
# \bf{Country} & \bf{IMR} &  \bf{CMR} &  \bf{U5MR} \\
# \hline\hline
# \endfirsthead
# 
# \multicolumn{4}{c}
# {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\
# 
# \hline
# \bf{Country} & \bf{IMR} &  \bf{CMR} &  \bf{U5MR} \\
# \hline\hline
# \endhead
# 
# \multicolumn{4}{r}{{Continued on next page}} \\
# \endfoot
# 
# \endlastfoot

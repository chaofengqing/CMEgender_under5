

## Lancet Global Health appendix table ##
# Data source list of sex-specific child mortality data sources used in the model
# import fully cleaned data
database <- read.csv("data/interim/dataset2013_fullyclean_2014-05-23.csv",
                    header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

s.i <- database[, "Sex.Ratio"]
iso.i <- database[, "Country.Code"]
year.i <- database[, "Reference.Date"]
surveyID.i <- database[, "SourceID"]
surveyname.i <- database[, "Series.Name"]
typenameforplot.i <- database[, "typenameforplot.i"]

typename.s <- sort(unique(typenameforplot.i))

# clean series name
surveyname.i <- ifelse(surveyname.i == "GFH", surveyname.i, surveyname.i)

table.df <- NULL
colname.table <- c("Country", "Number of obs",
                   "Most recent obs year", "Source")
for (c in 1:C) {
  surveyID.c <- unique(surveyID.i)[grep(name.c[c], unique(surveyID.i))]
  
  if (length(surveyID.c) > 0) {
    table.c <- matrix("", nr = length(surveyID.c) + 1, nc = length(colname.table))
    dimnames(table.c)[[2]] <- colname.table
    
    if (length(surveyID.c) > 1) {
      dimnames(table.c)[[1]] <- c(name.c[c], surveyID.c)
      for (sur in surveyID.c) {
        select.i <- (surveyID.i == sur)
        table.c[sur, "Source"] <- paste(unique(database[select.i,
                                                        c("Series.Year", "Series.Name")]),
                                        collapse = " ")
        # the number of data within a coutnry and the most recent observation year
        sum.s <- sum(select.i)
        maxobsyear.s <- round(max(year.i[select.i]), 1)
        table.c[sur, "Number of obs"] <- sum.s
        table.c[sur, "Most recent obs year"] <- maxobsyear.s
        table.c[sur, "Source"] <- paste0(table.c[sur, "Source"], " (",
                                         unique(typenameforplot.i[select.i]),
                                         ")")
        #table.c[sur, unique(typenameforplot.i[select.i])] <- "$\\times$"
      }#end of sur loop
      order.c <- c(1,
                   c(order(substr(table.c[2:nrow(table.c), "Source"], 1, 4)) + 1))
      table.c <- table.c[order.c, ]
      
    } else {
      sur <- surveyID.c
      select.i <- (surveyID.i == sur)
      row.names(table.c) <- c(name.c[c], sur)
      table.c[sur, "Source"] <- paste(unique(database[select.i,
                                                             c("Series.Year", "Series.Name")]),
                                             collapse = " ")
      # the number of data within a coutnry and the most recent observation year
      sum.s <- sum(select.i)
      maxobsyear.s <- round(max(year.i[select.i]), 1)
      table.c[sur, "Number of obs"] <- sum.s
      table.c[sur, "Most recent obs year"] <- maxobsyear.s
      table.c[sur, "Source"] <- paste0(table.c[surveyID.c, "Source"], " (",
                                       unique(typenameforplot.i[select.i]),
                                       ")")
      #table.c[sur, unique(typenameforplot.i[select.i])] <- "$\\times$"
    }#end of ifelse(length(surveyID.c) > 1)
    
    # the number of data within a coutnry and the most recent observation year
    sum.c <- sum(iso.i == iso.c[c])
    maxobsyear.c <- round(max(year.i[iso.i == iso.c[c]]), 1)
    table.c[1, "Number of obs"] <- sum.c
    table.c[1, "Most recent obs year"] <- maxobsyear.c
    
    #table.c[1, "Source"] <- paste0("(", sum.c, "; ", maxobsyear.c, ")")
    
  } else {
    table.c <- matrix("", nr = 1, nc = length(colname.table))
    dimnames(table.c)[[2]] <- colname.table
  }#end of ifelse(length(surveyID.c) > 0)
  
  table.c[1, "Country"] <- ExternalMakeCountryNamesFull(name.c[c])
  table.c[, "Source"] <- gsub("%", " percent", table.c[, "Source"])
  
  table.df <- rbind(table.df, table.c)
  
}#end of c loop
row.names(table.df) <- gsub("%", "percent", row.names(table.df))


table <- xtable(table.df, 
                caption = paste(
                  "\\bf{Overview of data sources used by country}",
                  "\\textmd{For each country, the total number of observation",
                  "and the most recent observation year are shown after the",
                  "country name.",
                  "For each country-specific data series, the number of",
                  "observations and the most recent observation year within",
                  "that series are shown before each data series name.",
                  "The source type that each data series falls in is shown in",
                  "parentheses after each data series name.",
                  "}"),
                label = "tb-sup-datasource",
                align = "|l|p{3cm}||p{1cm}|p{1.5cm}|p{8cm}|" # the 1st entry is for rownames
)
# row.names(table.df)[c(124,125,126,150)]
print(table, tabular.environment = 'longtable', floating = FALSE,
      include.colnames = FALSE, include.rownames = FALSE,
      hline.after = c(0:(nrow(table) - 1)), size = "tiny",
      sanitize.text.function=function(x){x} #display formula!!
      )

# paste column names and info after \begin{longtable}
# \hline
#  Country &  Number of obs &  Most recent obs year &  Source (source type) \\
# \hline\hline
# \endfirsthead
# 
# \multicolumn{4}{c}
# {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\
# 
# \hline
#  Country &  Number of obs &  Most recent obs year &  Source (source type) \\
# \hline\hline
# \endhead
# 
# \multicolumn{4}{r}{{Continued on next page}} \\
# \endfoot
# 
# \endlastfoot

# paste this for allignment
# {|p{3cm}||>{\centering\arraybackslash}m{0.5cm}|>{\centering\arraybackslash}m{1cm}|p{7cm}|}


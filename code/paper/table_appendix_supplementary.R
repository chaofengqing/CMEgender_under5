

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# table_appendix_supplementary.R
# 
# This script is to print out latex script of Appendix supplementary Table 5-10
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script.
# ExternalMakeCountryNamesFull(4)
# StandardizeDecimal(27)
# StandardizeSignifDigit(1)
# 
# input data: data/output/M49/:
# 1. trajectory_M49_Rx.rda;
# 2. trajectory_M49_S.rda;
#
# output data in /M49/: latex script of tables in Appendix Table 5-10
# 1. appendix_sup_table1_IMR.csv       - Appendix Table 5;
# 2. appendix_sup_table2_CMR.csv       - Appendix Table 6;
# 3. appendix_sup_table3_U5MR.csv      - Appendix Table 7;
# 4. appendix_sup_table4_excDeath1.csv - Appendix Table 8;
# 5. appendix_sup_table5_excDeath4.csv - Appendix Table 9;
# 6. appendix_sup_table6_excDeath5.csv - Appendix Table 10
#
###############################################################################

## supplementary tables in Appendix ##
t.select <- which(is.element(floor(years.t), c(1990, 2012)))

load(file = paste0(output.dir, "trajectory_", runname, "_Rx.rda")) #res.Rxtrajectory
load(file = paste0(output.dir, "trajectory_", runname, "_S.rda")) #res.Strajectory


######################################
## Appendix supplementary table 5-7 ##
for (age in ages.a) {
  
  a <- which(ages.a == age)
  
  table.colnames <- c(
    paste("Sex ratio", ageGroupName.a[a], "(1990)"), 
    paste("Sex ratio", ageGroupName.a[a], "(2012)"), 
    paste("Sex ratio", ageGroupName.a[a], "change (1990-2012)"),
    paste("Sex-specific", ageGroupName.a[a], "male per 1,000 (2012)"), 
    paste("Sex-specific", ageGroupName.a[a], "female per 1,000 (2012)"),
    paste("Estimated/Expected female", ageGroupName.a[a], "(1990)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "(2012)"), 
    paste("Estimated/Expected female", ageGroupName.a[a], "change (1990-2012)")
  )
  table.rownames <- c("World", regions.r, ExternalMakeCountryNamesFull(name.c))
  table.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
  rownames(table.df) <- table.rownames
  colnames(table.df) <- table.colnames
  
  for (t in t.select) {
    S.median <- c(res[[paste0("S", age, ".qt" )]][  2, t],
                  res[[paste0("S", age, ".rqt")]][, 2, t],
                  res[[paste0("S", age, ".cqt")]][, 2, t])
    S.lower <- c(res[[paste0("S", age, ".qt" )]][  1, t],
                 res[[paste0("S", age, ".rqt")]][, 1, t],
                 res[[paste0("S", age, ".cqt")]][, 1, t])
    S.upper <- c(res[[paste0("S", age, ".qt" )]][  3, t],
                 res[[paste0("S", age, ".rqt")]][, 3, t],
                 res[[paste0("S", age, ".cqt")]][, 3, t])
    
    table.df[, paste0("Sex ratio ", ageGroupName.a[a],
                      " (", floor(years.t[t]), ")")] <-
      paste0(StandardizeDecimal(S.median, preciSR), " [", 
             StandardizeDecimal(S.lower, preciSR), "; ",
             StandardizeDecimal(S.upper, preciSR), "]")
    
  }#end of t loop
  
  S.jt  <- res.Strajectory[[paste0("S", age, ".jt")]]
  S.rjt <- res.Strajectory[[paste0("S", age, ".rjt")]]
  S.clt <- res.Strajectory[[paste0("S", age, ".clt")]]
  
  changeS.df <- cbind(
    quantile(S.jt[, t.select[2]] - S.jt[,    t.select[1]],
             percentiles, na.rm = TRUE),
    apply(S.rjt[, , t.select[2]] - S.rjt[, , t.select[1]],
          1, quantile, percentiles, na.rm = TRUE),
    apply(S.clt[, , t.select[2]] - S.clt[, , t.select[1]],
          1, quantile, percentiles, na.rm = TRUE)
  )
  
  table.df[, paste("Sex ratio", ageGroupName.a[a], 
                   "change (1990-2012)")] <- 
    paste0(StandardizeDecimal(changeS.df[2, ], preciSR), " [", 
           StandardizeDecimal(changeS.df[1, ], preciSR), "; ",
           StandardizeDecimal(changeS.df[3, ], preciSR), "]",
           ifelse(changeS.df[1, ] > 0 | changeS.df[3, ] < 0,
                  sig.change.syb, ""))
  
  Qm.median <- c(res[[paste0("Q", age, "m.qt" )]][  2, t.select[2]], 
                 res[[paste0("Q", age, "m.rqt")]][, 2, t.select[2]], 
                 res[[paste0("Q", age, "m.cqt")]][, 2, t.select[2]])
  Qm.lower  <- c(res[[paste0("Q", age, "m.qt" )]][  1, t.select[2]], 
                 res[[paste0("Q", age, "m.rqt")]][, 1, t.select[2]], 
                 res[[paste0("Q", age, "m.cqt")]][, 1, t.select[2]])
  Qm.upper  <- c(res[[paste0("Q", age, "m.qt" )]][  3, t.select[2]], 
                 res[[paste0("Q", age, "m.rqt")]][, 3, t.select[2]], 
                 res[[paste0("Q", age, "m.cqt")]][, 3, t.select[2]])
  
  table.df[, paste("Sex-specific", ageGroupName.a[a], 
                   "male per 1,000 (2012)")] <- 
    paste0(StandardizeDecimal(Qm.median * Qunit, preciQ), " [", 
           StandardizeDecimal(Qm.lower * Qunit, preciQ), "; ", 
           StandardizeDecimal(Qm.upper * Qunit, preciQ), "]")
  
  Qf.median <- c(res[[paste0("Q", age, "f.qt" )]][  2, t.select[2]], 
                 res[[paste0("Q", age, "f.rqt")]][, 2, t.select[2]], 
                 res[[paste0("Q", age, "f.cqt")]][, 2, t.select[2]])
  Qf.lower  <- c(res[[paste0("Q", age, "f.qt" )]][  1, t.select[2]], 
                 res[[paste0("Q", age, "f.rqt")]][, 1, t.select[2]], 
                 res[[paste0("Q", age, "f.cqt")]][, 1, t.select[2]])
  Qf.upper  <- c(res[[paste0("Q", age, "f.qt" )]][  3, t.select[2]], 
                 res[[paste0("Q", age, "f.rqt")]][, 3, t.select[2]], 
                 res[[paste0("Q", age, "f.cqt")]][, 3, t.select[2]])
  
  table.df[, paste("Sex-specific", ageGroupName.a[a], 
                   "female per 1,000 (2012)")] <- 
    paste0(StandardizeDecimal(Qf.median * Qunit, preciQ), " [", 
           StandardizeDecimal(Qf.lower * Qunit, preciQ), "; ", 
           StandardizeDecimal(Qf.upper * Qunit, preciQ), "]")
  
  for (t in t.select) {
    Rx.median <- c(res[[paste0("Rx", age, ".qt" )]][  2, t],
                   res[[paste0("Rx", age, ".rqt")]][, 2, t],
                   res[[paste0("Rx", age, ".cqt")]][, 2, t])
    Rx.lower <- c(res[[paste0("Rx", age, ".qt" )]][  1, t],
                  res[[paste0("Rx", age, ".rqt")]][, 1, t],
                  res[[paste0("Rx", age, ".cqt")]][, 1, t])
    Rx.upper <- c(res[[paste0("Rx", age, ".qt" )]][  3, t],
                  res[[paste0("Rx", age, ".rqt")]][, 3, t],
                  res[[paste0("Rx", age, ".cqt")]][, 3, t])
    
    table.df[, paste0("Estimated/Expected female ", ageGroupName.a[a],
                     " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(Rx.median, preciSR), " [",
             StandardizeDecimal(Rx.lower, preciSR), "; ",
             StandardizeDecimal(Rx.upper, preciSR), "]",
             ifelse(Rx.lower > cutoffS | Rx.upper < cutoffS,
                    sig.ratio.syb, ""))
    
    Rx.jt  <- res.Rxtrajectory[[paste0("Rx", age, ".jt")]]
    Rx.rjt <- res.Rxtrajectory[[paste0("Rx", age, ".rjt")]]
    Rx.cjt <- res.Rxtrajectory[[paste0("Rx", age, ".cjt")]]
    
    changeRx.df <- cbind(
      quantile(Rx.jt[, t.select[2]] - Rx.jt[,    t.select[1]],
               percentiles, na.rm = TRUE),
      apply(Rx.rjt[, , t.select[2]] - Rx.rjt[, , t.select[1]],
            1, quantile, percentiles, na.rm = TRUE),
      apply(Rx.cjt[, , t.select[2]] - Rx.cjt[, , t.select[1]],
            1, quantile, percentiles, na.rm = TRUE)
    )
    
    table.df[, paste("Estimated/Expected female", ageGroupName.a[a], 
                     "change (1990-2012)")] <- 
      paste0(StandardizeDecimal(changeRx.df[2, ], preciSR), " [", 
             StandardizeDecimal(changeRx.df[1, ], preciSR), "; ", 
             StandardizeDecimal(changeRx.df[3, ], preciSR), "]",
             ifelse(changeRx.df[1, ] > 0 | changeRx.df[3, ] < 0,
                    sig.change.syb, ""))
    
  }#end of t loop
  
  write.csv(table.df, paste0(output.dir, "appendix_sup_table", a, "_", 
                             ageGroupName.a[a], ".csv"), row.names = TRUE)
}#end of age loop


#######################################
## Appendix supplementary table 8-10 ##
for (age in ages.a) {
  
  a <- which(ages.a == age)
  
  table.colnames <- c(
    paste("Excess female death", ageGroupName.a[a], "(1990)"),
    paste("Excess female death", ageGroupName.a[a], "(2012)"),
    paste("% of death", ageGroupName.a[a], "(1990)"),
    paste("% of death", ageGroupName.a[a], "(2012)"),
    paste("Excess female", ageGroupName.a[a], "(1990)"), 
    paste("Excess female", ageGroupName.a[a], "(2012)")
  )
  table.rownames <- c("World", regions.r, ExternalMakeCountryNamesFull(name.c))
  
  table.excDf.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
  rownames(table.excDf.df) <- table.rownames
  colnames(table.excDf.df) <- table.colnames
  
  excDf.ALLqt <- array(NA, c(length(table.rownames), Per, Tend))
  excDf.ALLqt[1, , ] <- res[[paste0("excD", age, "f.qt")]]
  excDf.ALLqt[(1 + 1):(1 + R), , ] <- res[[paste0("excD", age, "f.rqt")]]
  excDf.ALLqt[(1 + R + 1):length(table.rownames), , ] <-
    res[[paste0("excD", age, "f.cqt")]]
  
  excDf.ALLqt <- signif(round(excDf.ALLqt[, , t.select]), 3)
  
  # for the UI bounds, we should end up with the same order of magnitude,
  # e.g. if point estimate is 123, we want to avoid reporting 0.456 for a bound
  # because we do not have that level of precision. So the order of magnitude
  # should follow from the median.
  for (all in 1:dim(excDf.ALLqt)[1]) {
    for (t in 1:dim(excDf.ALLqt)[3]) {
      for (q in c(1, 3)) {
        excDf.ALLqt[all, q, t] <- StandardizeSignifDigit(
          number.in = excDf.ALLqt[all, q, t],
          number.ref = excDf.ALLqt[all, 2, t])
      }#end of q loop
    }#end of t loop
  }#end of all loop
  
  for (all in 1:dim(excDf.ALLqt)[1]) {
    for (q in 1:Per) {
      excDf.ALLqt[all, q, ] <- prettyNum(excDf.ALLqt[all, q, ],
                                         width = 3, big.mark = ",")
    }#end of q loop
  }#end of all loop
  
  
  table.excDf.df[, paste("Excess female death", ageGroupName.a[a], "(1990)")] <-
    paste0(excDf.ALLqt[, 2, 1],
           " [", excDf.ALLqt[, 1, 1], "; ",
           excDf.ALLqt[, 3, 1], "]")
  
  table.excDf.df[, paste("Excess female death", ageGroupName.a[a], "(2012)")] <-
    paste0(excDf.ALLqt[, 2, 2],
           " [", excDf.ALLqt[, 1, 2], "; ",
           excDf.ALLqt[, 3, 2], "]") 
  
  ## 1990 ##
  table.excDf.df["World", paste("% of death", ageGroupName.a[a], "(1990)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.qt")]][2, t.select[1]] /
            (res[[paste0("D", age, "f.qt")]][2, t.select[1]] +
               res[[paste0("D", age, "m.qt")]][2, t.select[1]]) * 100,
          preciDchange)
  table.excDf.df[regions.r, paste("% of death", ageGroupName.a[a], "(1990)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.rqt")]][, 2, t.select[1]] /
            (res[[paste0("D", age, "f.rqt")]][, 2, t.select[1]] +
               res[[paste0("D", age, "m.rqt")]][, 2, t.select[1]]) * 100,
          preciDchange)
  table.excDf.df[ExternalMakeCountryNamesFull(name.c),
                 paste("% of death", ageGroupName.a[a], "(1990)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.cqt")]][, 2, t.select[1]] /
            (res[[paste0("D", age, "f.cqt")]][, 2, t.select[1]] +
               res[[paste0("D", age, "m.cqt")]][, 2, t.select[1]]) * 100,
          preciDchange)
  
  ## 2012 ##
  table.excDf.df["World", paste("% of death", ageGroupName.a[a], "(2012)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.qt")]][2, t.select[2]] /
            (res[[paste0("D", age, "f.qt")]][2, t.select[2]] +
               res[[paste0("D", age, "m.qt")]][2, t.select[2]]) * 100,
          preciDchange)
  table.excDf.df[regions.r, paste("% of death", ageGroupName.a[a], "(2012)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.rqt")]][, 2, t.select[2]] /
            (res[[paste0("D", age, "f.rqt")]][, 2, t.select[2]] +
               res[[paste0("D", age, "m.rqt")]][, 2, t.select[2]]) * 100,
          preciDchange)
  table.excDf.df[ExternalMakeCountryNamesFull(name.c),
                 paste("% of death", ageGroupName.a[a], "(2012)")] <-
    StandardizeDecimal(res[[paste0("excD", age, "f.cqt")]][, 2, t.select[2]] /
            (res[[paste0("D", age, "f.cqt")]][, 2, t.select[2]] +
               res[[paste0("D", age, "m.cqt")]][, 2, t.select[2]]) * 100,
          preciDchange)
  
  for (t in t.select) {
    excQf.median <- c(res[[paste0("excQ", age, "f.qt" )]][  2, t], 
                      res[[paste0("excQ", age, "f.rqt")]][, 2, t], 
                      res[[paste0("excQ", age, "f.cqt")]][, 2, t])    
    excQf.lower  <- c(res[[paste0("excQ", age, "f.qt" )]][  1, t], 
                      res[[paste0("excQ", age, "f.rqt")]][, 1, t], 
                      res[[paste0("excQ", age, "f.cqt")]][, 1, t])
    excQf.upper  <- c(res[[paste0("excQ", age, "f.qt" )]][  3, t], 
                      res[[paste0("excQ", age, "f.rqt")]][, 3, t], 
                      res[[paste0("excQ", age, "f.cqt")]][, 3, t])
    
    table.excDf.df[, paste0("Excess female ", ageGroupName.a[a],
                            " (", floor(years.t[t]), ")")] <- 
      paste0(StandardizeDecimal(excQf.median * Qunit, preciQ), " [", 
             StandardizeDecimal(excQf.lower * Qunit, preciQ), "; ", 
             StandardizeDecimal(excQf.upper * Qunit, preciQ), "]")
  }#end of t loop
  
  write.csv(table.excDf.df, 
            paste0(output.dir, "appendix_sup_table",
                   3 + a,"_excDeath", age, ".csv"), 
            row.names = TRUE)
  
}#end of age loop


##########################
## print out tex tables ##
a <- 3 #1, 2, 3
table.df <- read.csv(paste0("data/output/M49/appendix_sup_table", a, "_",
                            ageGroupName.a[a], ".csv"))
# load indicies for outlying countries
load(file = paste0(output.dir, "indicesforexcess.", ages.a[a], ".all.rda"))
eval(parse(text = paste0("indicesforexcess <- indicesforexcess.", ages.a[a], ".all")))
# outlying countries in 1990 only
only1990.all <- (indicesforexcess$f1990.all | indicesforexcess$m1990.all) &
  !(indicesforexcess$f2012.all | indicesforexcess$m2012.all)
# outlying countries in 2012 only
only2012.all <- !(indicesforexcess$f1990.all | indicesforexcess$m1990.all) &
  (indicesforexcess$f2012.all | indicesforexcess$m2012.all)
# outlying countries in 1990 and 2012
both.all <- (indicesforexcess$f1990.all | indicesforexcess$m1990.all) &
  (indicesforexcess$f2012.all | indicesforexcess$m2012.all)

rowname.latex <- as.character(table.df[, 1])
rowname.latex[only1990.all] <- paste0(rowname.latex[only1990.all], sig.1990.syb)
rowname.latex[only2012.all] <- paste0(rowname.latex[only2012.all], sig.2012.syb)
rowname.latex[both.all]     <- paste0(rowname.latex[both.all], sig.1990.syb, sig.2012.syb)
table.df[, 1] <- rowname.latex

table <- xtable(table.df, 
                caption = paste(
                  "\\bf{Estimates and 90\\% uncertainty intervals for",
                  "sex ratios for", ageGroupName.a[a],
                  "in 1990 and 2012, the change in",
                  "sex ratios from 1990 to 2012,",
                  "sex-specific", ageGroupName.a[a], "in 2012,",
                  "and ratios of estimated to expected female",
                  ageGroupName.a[a]," and their change from 1990 to 2012",
                  "for the world, MDG regions, and all countries.}",
                  "\\textmd{",
                  sig.1990.syb, ": Sex ratio is outlying in 1990.",
                  sig.2012.syb, ": Sex ratio is outlying in 2012.",
                  sig.ratio.syb, ": Ratio of estimated to expected female mortality",
                  "is significantly different from one.",
                  sig.change.syb, ": Change is significantly different from zero.",
                  "}"),
                label = paste0("tb-sup-SR", ages.a[a]),
                align = "|l|p{2cm}||c|c|c||c|c||c|c|c|" # the 1st entry is for rownames
)
print(table, tabular.environment = 'longtable', floating = FALSE,
      include.colnames = FALSE, include.rownames = FALSE,
      hline.after = c(1:(nrow(table) - 1), 1, 1 + R), size = "tiny",
      sanitize.text.function=function(x){x} #display formula!!
      )
# paste the following right BEFORE \begin{longtable}
# \captionsetup{width=1.08\textwidth} %a=1
# \captionsetup{width=1.065\textwidth} %a=2
# \captionsetup{width=1.085\textwidth} %a=3

# paste column names and info right AFTER \begin{longtable}
# \hline
# & \multicolumn{3}{c||}{\bf{Sex ratio IMR}} & \multicolumn{2}{c||}{\bf{Sex-specific IMR in 2012 (per 1,000)}} & \multicolumn{3}{c|}{\bf{Estimated/Expected female IMR}} \\ \hline
# & \bf{1990} &  \bf{2012} &  \bf{Change}     &  \bf{Male} &  \bf{Female} & \bf{1990} &  \bf{2012} &  \bf{Change} \\
# &      &       &  \bf{1990--2012} &       &         &      &       &\bf{1990--2012}\\
# \hline\hline
# \endfirsthead
# 
# \multicolumn{9}{c}
# {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\
# 
# \hline
# & \multicolumn{3}{c||}{\bf{Sex ratio IMR}} & \multicolumn{2}{c||}{\bf{Sex-specific IMR in 2012 (per 1,000)}} & \multicolumn{3}{c|}{\bf{Estimated/Expected female IMR}} \\ \hline
# & \bf{1990} &  \bf{2012} &  \bf{Change}     &  \bf{Male} &  \bf{Female} & \bf{1990} &  \bf{2012} &  \bf{Change} \\
# &      &       &  \bf{1990--2012} &       &         &      &       &\bf{1990--2012}\\
# \hline\hline
# \endhead
# 
# \multicolumn{9}{l}{\textmd{$\star$ : Sex ratio is outlying in 1990. \dag : Sex ratio is outlying in 2012. \ddag : Ratio of estimated to expected female mortality is significantly different from one. \S : Change is significantly different from zero.}} \\
# \multicolumn{9}{r}{{Continued on next page}} \\
# \endfoot
# 
# \endlastfoot

# paste the following right BEFORE \end{longtable}
# \multicolumn{9}{l}{\textmd{$\star$ : Sex ratio is outlying in 1990. \dag : Sex ratio is outlying in 2012. \ddag : Ratio of estimated to expected female mortality is significantly different from one. \S : Change is significantly different from zero.}} \\


age <- 5 #1, 4, 5
a <- which(ages.a == age)
table.df <- read.csv(paste0(output.dir, "appendix_sup_table", a + 3, "_excDeath", age, ".csv"))
table <- xtable(table.df, 
                caption = paste("\\bf{Estimates and uncertainty intervals for",
                                "excess female deaths and excess female", ageGroupName.a[a],
                                "and the percentage of excess female deaths among all deaths",
                                "of", ageGroupName.a[a],
                                "for 1990 and 2012 for the world and MDG regions, and all countries.}"),
                label = paste0("tb-sup-excF", age),
                align = "|l|p{3cm}||c|c||c|c||c|c|" # the 1st entry is for rownames
)
print(table, tabular.environment = 'longtable', floating = FALSE,
      include.colnames = FALSE, include.rownames = FALSE,
      hline.after = c(0:(nrow(table) - 1), 1, 1 + R), size = "tiny")
# paste column names and info after \begin{longtable}
# \hline
# & \multicolumn{2}{c||}{Excess female deaths IMR} & \multicolumn{2}{c||}{\% of deaths IMR}& \multicolumn{2}{c|}{Excess female IMR (per 1,000)} \\ \hline
# & 1990 &  2012 & 1990 &  2012 &  1990 &  2012 \\
# \hline
# \endfirsthead
# 
# \multicolumn{7}{c}
# {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\
# 
# \hline
# & \multicolumn{2}{c||}{Excess female deaths IMR} & \multicolumn{2}{c||}{\% of deaths IMR}& \multicolumn{2}{c|}{Excess female IMR (per 1,000)} \\ \hline
# & 1990 &  2012 & 1990 &  2012 &  1990 &  2012 \\
# \hline
# \endhead
# 
# \multicolumn{7}{r}{{Continued on next page}} \\
# \endfoot
# 
# \endlastfoot

## the end ##



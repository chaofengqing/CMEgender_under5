

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# plot_DataseriesandCountryCI_GBDcompare.R
# 
# This script is to get comparison plots for GBD and this study.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main49_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script.
# ExternalMakeCountryNamesFull(2)
# PlotCIbandwithDataseries(2)
# 
# input data: data/output/M49/:
# 1. output/M49/cis_M49_full.rda
# 2. output/M49/Results_SexRatioIGMEandGBD.csv
# 3. interim/dataset_S5all.csv
#
# output plots in fig/paper/:
# 1. paper_GBDvsIGMEM49_SRcompare.pdf                - Appendix Figure 2;
# 2. paper_CIsM49_GBDcompare_OutlyingCountry2010.pdf - Appendix Figure 3;
# 3. paper_CIsM49_GBDcompare_OppositeCountry2010.pdf - Appendix Figure 4;
#
###############################################################################

## read in GBD data ##
ihme.df <- read.csv(paste0(output.dir, "Results_SexRatioIGMEandGBD.csv"),
                    header = TRUE, stringsAsFactors = FALSE, 
                    strip.white = TRUE)
ihme.sr   <- ihme.df[, "SexRatio.IHME"]
ihme.yr   <- ihme.df[, "Year"         ]
ihme.iso  <- ihme.df[, "ISO.Code"     ]
ihme.name <- ihme.df[,"Country.Name"  ]

dataS5all <- as.data.frame(read.csv("data/interim/dataset_S5all.csv",
                                    header = TRUE, stringsAsFactors = FALSE, 
                                    strip.white = TRUE))
I            <- dim(dataS5all)[1]
s.i          <- dataS5all[, "Sex.Ratio"     ]
agecat.i     <- dataS5all[, "agecat.i"      ]
name.i       <- dataS5all[, "Country.Name"  ]
year.i       <- dataS5all[, "Reference.Date"]
iso.i        <- dataS5all[, "Country.Code"  ]
method.i     <- dataS5all[, "method.i"      ]
typename.i   <- dataS5all[, "typename.i"    ]
surveyplot.i <- dataS5all[, "surveyplot.i"  ]
surveyyear.i <- dataS5all[, "Series.Year"   ]
SEnoimpute.i <- dataS5all[, "Sex.Ratio.SE"  ]


############################
## plot IGME against GBD ##
YEAR.PLOT <- sort(unique(ihme.yr[!is.na(ihme.sr)]))
igme.sr <- ihme.df[,"SexRatio.IGME"]

# absolute difference
base <- seq(0.7, 1.7, 0.1)
diff <- c(0.05, 0.10, 0.15, 0.2)

pdf(paste0(paperplot.dir,"paper_GBDvsIGME",runname,"_SRcompare.pdf"), 
    height = 29 * cm, width = 21 * cm)
par(mfrow = c(3, 2), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.5, 
    mar = c(3, 3.5, 2.5, 0.5), las = 1, mgp = c(1.8, 0.5, 0))
for (yr in YEAR.PLOT) {
  igme.s5 <- igme.sr[ihme.yr == yr]
  ihme.s5 <- ihme.sr[ihme.yr == yr]
  name.plot <- InternalMakeCountryNamesShort(ihme.name[ihme.yr == yr])
  year.plot <- YEAR.PLOT[ihme.yr == yr]
  
  plot(igme.s5 ~ ihme.s5, type = "n", xlab = "GBD", ylab = "UN IGME",
       xlim = c(0.9, 1.5), ylim = c(0.9, 1.5),
       main = paste("sex ratio U5MR in", floor(yr)))
  
  abline(a = 0, b = 1, lwd = 2) #IGME agrees with IHME
  
  for (d in 1:length(diff)) {
    low1 <- base - diff[d]
    up1  <- base + diff[d]
    polygon(x = c(base, rev(base)), y = c(up1, rev(low1)), 
            col = adjustcolor("lightgreen", alpha.f = 1 - 0.2 * d), border = NA)  
  }#end of d loop
  
  points(igme.s5 ~ ihme.s5, pch = 19, cex = 0.7)
  
  ## highlight points with absolute difference >= 0.15 ##
  outlying.c <- abs(igme.s5 - ihme.s5)  >= diff[3] | 
    (igme.s5 - 1) * (ihme.s5 - 1) < 0
  points(igme.s5[outlying.c] ~ ihme.s5[outlying.c], 
         pch = 19, cex = 1.7, col = "mediumpurple1")
  
  mark1 <- which((igme.s5 - ihme.s5) >= diff[3])
  mark2 <- which((igme.s5 - ihme.s5) <= 0 - diff[3])
  mark3 <- which((igme.s5 - 1) * (ihme.s5 - 1) < 0)
  mark <- sort(union(mark1, union(mark2, mark3)))
  
  if(length(mark) > 0) {
    text(x = ihme.s5[mark], y = igme.s5[mark],
         labels = 1:length(mark), pos = NULL, cex = 0.8)
    legend("bottomright", 
           c(paste(1:length(mark), name.plot[mark], 
                   sep = ". ")), cex = 0.8, x.intersp = -0.5)
  }#end of if(length(mark) > 0)
  
}#end of year loop

# add in the full country name
plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
c.short <- c("CAR", "DRC", "Gambia", "Iran", "Laos", "South Korea")
c.full  <- c("Central African Republic",
             "Democratic Republic of the Congo",
             "The Gambia",
             "Iran (Islamic Republic of)",
             "Lao People's Democratic Republic",
             "Republic of Korea")
legend("left", c(paste(c.short, c.full, sep = " = ")),
       cex = 1.3, x.intersp = -0.5)

dev.off()


############################################################
## plot with data serise and IGME & IHME estimates over time 
# for outlying countries ##
# no need to use unique.sources since we only compare sex ratio U5MR
pdf(paste0(paperplot.dir, "paper_CIs", runname, "_GBDcompare_OutlyingCountry2010.pdf"), 
    height = 5.5, width = 9)
par(mfrow = c(2, 3), cex.lab = 1.2, cex.axis = 1, las = 1, tcl = -0.3,
    mar = c(2.5, 3, 1.8, 0.5), mgp = c(1.5, 0.3, 0)) #oma = c(0.2, 2.5, 2, 3), 
#based on plot at the end
name.c.plot <- c("Congo DR", "Haiti", "Lesotho", "Somalia", "Mongolia")
for (name in name.c.plot) {
  c <- which(name.c == name)
  
  S.qt <- res[["S5.cqt"]][c, , ]
  
  selectCountry.i <- (iso.i == paste(iso.c[c]))     
  selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == ages.a[A]]
  # sort by survey date to get nicer legend
  selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    
  
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE,
    dataseries = s.i, baseSeries = c("VR", "VR "), datalim = c(0.7, 2.5), 
    CI1s = S.qt, Source = surveyplot.i, x = year.i, alpha.dataseries = 0.85,
    select.x = selectCountryAge.i, ylab = "sex ratio U5MR", xlab = "Year",
    cutoff = cutoffS, lwd.CI1 = 4, cex.dataseries = 0.8, lwd.dataseries = 1.0
  )
  #FQ: shld construct within function---------
  select.ihme <- ihme.iso==iso.c[c] & !is.na(ihme.sr)
  lines(floor(ihme.yr[select.ihme]), ihme.sr[select.ihme], 
        col = "royalblue", lwd = 4)
  legend("bottomleft", c("UN IGME", "GBD"), lty = 1, lwd = 5, 
         col = c("red", "royalblue"), cex = 1)
  #FQ: shld construct within function---------
  title(main = ExternalMakeCountryNamesFull(name.c)[c], line = 0.5, cex.main = 1.45)
  
}#end of name loop
dev.off()


pdf(paste0(paperplot.dir, "paper_CIs", runname, "_GBDcompare_OppositeCountry2010.pdf"), 
    height = 5, width = 9)
par(mfrow = c(1, 2), cex.lab = 1.1, cex.axis = 1, las = 1, tcl = -0.25,
    mar = c(2.75, 3, 2, 0.5), mgp = c(1.7, 0.2, 0))
#based on plot at the end
name.c.plot <- c("India", "Jordan")
for (name in name.c.plot) {
  c <- which(name.c == name)
  
  S.qt <- res[["S5.cqt"]][c, , ]
  
  selectCountry.i <- (iso.i == paste(iso.c[c]))     
  selectCountryAge.i <- seq(1, I)[selectCountry.i & agecat.i == ages.a[A]]
  # sort by survey date to get nicer legend
  selectCountryAge.i <- selectCountryAge.i[order(surveyyear.i[selectCountryAge.i])]    
  
  PlotCIbandwithDataseries(
    if.xlimFix = TRUE,
    dataseries = s.i, baseSeries = c("VR", "VR "), datalim = c(0.6, 2), 
    CI1s = S.qt, Source = surveyplot.i, x = year.i, alpha.dataseries = 0.85,
    select.x = selectCountryAge.i, ylab = "sex ratio U5MR", xlab = "Year",
    cutoff = cutoffS, lwd.CI1 = 4.5, cex.dataseries = 0.8, lwd.dataseries = 1.3
  )
  
  select.ihme <- ihme.iso==iso.c[c] & !is.na(ihme.sr)
  lines(floor(ihme.yr[select.ihme]), ihme.sr[select.ihme], 
        col = "royalblue", lwd = 4.5)
  legend("bottomleft", c("UN IGME", "GBD"), lty = 1, lwd = 5, 
         col = c("red", "royalblue"), cex = 1)
  
  title(main = ExternalMakeCountryNamesFull(name.c)[c], line = 0.5, cex.main = 1.5)
  
}#end of name loop
dev.off()


## the end ##


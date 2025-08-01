

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# check_reproducibility.R
# 
# This script checks reproducibility for the model.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# GetS5fromQboth(2)
# SamplesToUI(3)
# 
# input data folder /data/output/runname/:
# 1. cis_*.rda     - old result;
# 2. cis_*_new.rda - new result;
#
# output data in folder data/output/runname/:
# 1. reproducibility_*.csv
# 
###############################################################################


if (Main.run) {
  load(file = paste0(output.dir, "cis_", runname, "_full.rda")) #res.full
  res.old <- res.full
  load(file = paste0(output.dir, "cis_", runname, "_full_new.rda")) #res.full
  res.new <- res.full
}
if (!Main.run) {
  load(file = paste0(output.dir, "cis_", runname, ".rda")) #res2015
  res.old <- res2015
  load(file = paste0(output.dir, "cis_", runname, "_new.rda")) #res.full
  res.new <- res.full
}

cutoff.death <- 10

res.diff <- res.new

for (name in names(res.new)) {
  if (length(grep("qt", name)) == 1) {
    
    if (length(grep("D", name)) == 1) {
        for (q in 1:Per) {
          for (t in 1:Tend) {
            
            if (length(grep("cqt", name)) == 1) {
              for (c in 1:C) {
                res.diff[[name]][c, q, t] <- (res.new[[name]][c, q, t] - res.old[[name]][c, q, t]) /
                  ((res.new[[name]][c, q, t] + res.old[[name]][c, q, t]) / 2) * 100
                
                if (abs((res.new[[name]][c, q, t] + res.old[[name]][c, q, t]) / 2) <= cutoff.death &
                      !is.na(res.diff[[name]][c, q, t])) {
                  res.diff[[name]][c, q, t] <- NA
                }         
              }#end of c loop
            }#end of if cqt
            
            if (length(grep("rqt", name)) == 1) {
              for (r in 1:R) {
                res.diff[[name]][r, q, t] <- (res.new[[name]][r, q, t] - res.old[[name]][r, q, t]) /
                  ((res.new[[name]][r, q, t] + res.old[[name]][r, q, t]) / 2) * 100
                
                if (abs((res.new[[name]][r, q, t] + res.old[[name]][r, q, t]) / 2) <= cutoff.death &
                      !is.na(res.diff[[name]][r, q, t])) {
                  res.diff[[name]][r, q, t] <- NA
                }         
              }#end of r loop
            }#end of if rqt
            
            if (length(dim(res.diff[[name]])) == 2) {
              res.diff[[name]][q, t] <- (res.new[[name]][q, t] - res.old[[name]][q, t]) /
                ((res.new[[name]][q, t] + res.old[[name]][q, t]) / 2) * 100
              
              if (abs((res.new[[name]][q, t] + res.old[[name]][q, t]) / 2) <= cutoff.death &
                    !is.na(res.diff[[name]][q, t])) {
                res.diff[[name]][q, t] <- NA
              }         
              
            }
                        
          }#end of t loop
        }#end of q loop
      
    }#end of if "D"
    
    if (length(grep("Q", name)) == 1) {
        for (q in 1:Per) {
          for (t in 1:Tend) {
            
            if (length(grep("cqt", name)) == 1) {
              for (c in 1:C) {
                res.diff[[name]][c, q, t] <- (res.new[[name]][c, q, t] - res.old[[name]][c, q, t]) * Qunit
              }
            }
            if (length(grep("rqt", name)) == 1) {
              for (r in 1:R) {
                res.diff[[name]][r, q, t] <- (res.new[[name]][r, q, t] - res.old[[name]][r, q, t]) * Qunit
              }
            }
            if (length(dim(res.diff[[name]])) == 2) {
              res.diff[[name]][q, t] <- (res.new[[name]][q, t] - res.old[[name]][q, t]) * Qunit
            }
          }#end of t loop
        }#end of q loop
      
    }#end of if "Q"
    
    if (length(grep("D", name)) == 0 &
          length(grep("Q", name)) == 0) {
        for (q in 1:Per) {
          for (t in 1:Tend) {
            if (length(grep("cqt", name)) == 1) {
              for (c in 1:C) {
                res.diff[[name]][c, q, t] <- res.new[[name]][c, q, t] - res.old[[name]][c, q, t]
              }
            }
            if (length(grep("rqt", name)) == 1) {
              for (r in 1:R) {
                res.diff[[name]][r, q, t] <- res.new[[name]][r, q, t] - res.old[[name]][r, q, t]
              }
            }
            if (length(dim(res.diff[[name]])) == 2) {
              res.diff[[name]][q, t] <- res.new[[name]][q, t] - res.old[[name]][q, t]
            }
          }#end of t loop
        }#end of q loop
      
    }#end of ifelse D"
    
  }#end of if "qt
}#end of name loop

percentile <- c(0, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
diff <- matrix(NA, nr = length(grep("qt", names(res.new))), nc = length(percentile))
rownames(diff) <- names(res.new)[grep("qt", names(res.new))]
colnames(diff) <- c("0%", "25%", "50%", "75%", "95%", "99%", "100%")
for (name in rownames(diff)) {
  diff[name, ] <- quantile(c(res.diff[[name]]), percentile, na.rm = TRUE)
}#end of row loop
if (Main.run) {
  write.csv(diff, paste0(output.dir, "reproducibility_deathabove",
                         cutoff.death, "_", runname, ".csv"), row.names = TRUE)
}
if (!Main.run) {
  write.csv(diff, paste0(output.dir, "reproducibility", "_", runname, ".csv"), row.names = TRUE)
}

## the end ##


# ## test ##
# name <- "excD1f.cqt"
# for (c in 1:C) {
#   for (q in 1:Per) {
#     for (t in 1:Tend) {
#       diff <- (res.new[[name]][c, q, t] - res.old[[name]][c, q, t]) /
#         ((res.new[[name]][c, q, t] + res.old[[name]][c, q, t]) / 2) * 100
#       rule1 <- abs(diff) >= 100
#       rule2 <- abs((res.new[[name]][c, q, t] + res.old[[name]][c, q, t]) / 2) > cutoff.death &
#         !is.na(res.new[[name]][c, q, t]) & !is.na(res.old[[name]][c, q, t])
#       
#       if (rule1 & rule2) {
#         print(paste(c, name.c[c], floor(years.t[t]), percentiles[q],
#                     round(res.old[[name]][c, q, t]), round(res.new[[name]][c, q, t]),
#                     round(diff, 1)))
#       }#end of if (rule1 & rule2)
#       
#     }#end of t loop
#   }#end of q loop
# }#end of c loop



## Lancet Global Health paper review info requested by reviewers
dataraw <- read.csv("data/interim/dataset_raw_2013.csv", header = TRUE,
                    stringsAsFactors = FALSE, strip.white = TRUE)
s.i <- dataraw[, "Sex.Ratio"]
a <- 10; b <- 10
sum(s.i > a & s.i <=b, na.rm = TRUE); mean(s.i > a & s.i <=b, na.rm = TRUE) * 100
sum(s.i >= a, na.rm = TRUE); mean(s.i >= a, na.rm = TRUE) * 100

summary(s.i)
sum(s.i <= 1/10, na.rm = TRUE)
table(s.i[s.i<=1/10])
unique(na.omit(dataraw[s.i <= 1/10, c("Country.Name", "Series.Name")]))
sum((s.i <= 1/10)[grep("WHO", dataraw[, "Series.Name"])], na.rm = TRUE)
sum((s.i == 0)[grep("WHO", dataraw[, "Series.Name"])], na.rm = TRUE)

sum(s.i >= 10, na.rm = TRUE)
table(s.i[s.i >= 10])
unique(na.omit(dataraw[s.i >= 10, c("Country.Name", "Series.Name")]))
sum((s.i >= 10)[grep("WHO", dataraw[, "Series.Name"])], na.rm = TRUE)
sum((s.i == Inf)[grep("WHO", dataraw[, "Series.Name"])], na.rm = TRUE)


## how many data sources in U5MR meta have no match in gender database
u5.base <- read.csv("data/inputforpaper/Metafile_U5MR_CMEInfo.csv",
                    header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
sr.base <- read.csv("data/interim/dataset2013_fullyclean_2014-05-23.csv",
                     header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

sourceID.i <-
  apply(
    sr.base[, c("Country.Code", "Series.Name", "Series.Year", "Series.Type")],
    1, paste, collapse = "_")

sr.id <- unique(sourceID.i)
u5.id <- u5.base[, "sourceID.s"]

u5.id <- gsub("Direct (5 year)", "Direct", u5.id, fixed = TRUE)
u5.id <- gsub("Direct (Single year)", "Direct", u5.id, fixed = TRUE)
u5.id <- gsub("Direct (Various periods)", "Direct", u5.id, fixed = TRUE)
u5.id <- unique(u5.id)

sr.id <- gsub(pattern = "Direct (5 year)", "Direct", sr.id, fixed = TRUE)
sr.id <- gsub("Direct (Single year)", "Direct", sr.id, fixed = TRUE)
sr.id <- gsub("Direct (Various periods)", "Direct", sr.id, fixed = TRUE)
sr.id <- unique(sr.id)

sourceID.i <- gsub(pattern = "Direct (5 year)", "Direct", sourceID.i, fixed = TRUE)
sourceID.i <- gsub("Direct (Single year)", "Direct", sourceID.i, fixed = TRUE)
sourceID.i <- gsub("Direct (Various periods)", "Direct", sourceID.i, fixed = TRUE)

# need extra cleaning...
table(is.element(sr.id, u5.id))
# FALSE  TRUE 
# 41      536
# data series in gender database but not U5MR database
unmatch.id <- sr.id[!is.element(sr.id, u5.id)]
mean(is.element(sourceID.i, unmatch.id))
sum(!is.element(u5.id, sr.id))

# how many series in U5MR database but not in gender database
table(is.element(u5.id, sr.id))

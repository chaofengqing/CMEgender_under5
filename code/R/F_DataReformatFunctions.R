

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# F_DataReformatFunctions.R
# 
# This script contains all functions related to data reformatting, combining,
# creating, etc.
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# GetSpreadSample(..)
# GetLimitCountryData(..)
# GetResultExcel(..)
# SamplesToUI(..)
# ReformatPopulationData(..)
# GetDataSource(..)
# ReformatCCdata(.., GetDataSource(2), ..)
# GetInputData(.., ReformatCCdata(3), ..)
# 
###############################################################################

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetSpreadSample <- function (
  L.in, # number of samples to be select from 
  L.out # number of samples to select
  ) {
  ## select a subset of sample as spread out as possible ##
  
  gap.level1 <- floor(L.in / L.out)
  
  if (gap.level1 > 1) {
    selectfromInput.j <- seq(1, L.in, by = gap.level1)[1:L.out]    
  } else {
    gap.level2 <- ceiling((L.out - 1) / (L.in - L.out))
    gap.level3 <- ceiling((L.out - 1) /
                            ((L.in - L.out) - ceiling((L.out - 1) / gap.level2)))
    
    selectfromInput.j <- rep(NA, L.out)
    selectfromInput.j[1] <- 1
    for (j in 2:L.out) {
      selectfromInput.j[j] <- selectfromInput.j[j - 1] + 1 +
        ifelse(j %% gap.level2 == 0, 1, 0) +
        ifelse(j %% gap.level3 == 0, 1, 0)
    }#end of j loop
  }#end of ifelse(gap > 1)
  
  return(selectfromInput.j)
}#end of function GetSpreadSample()


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetLimitCountryData <- function (
  data.in,
  year.t = years.t, # full sequence of years
  year.upto = yearResult # save data.in upto the year year.upto
) {
  
  ## set NA to year-related objects in a list after year.upto ##
  
  listName.i <- names(data.in)[grep("qt", names(data.in))]
  data.out <- data.in #results for [1950, year.upto]
  
  for (i in 1:length(listName.i)) {
    ALL.qt <- data.in[[listName.i[i]]]
    
    if(is.matrix(data.in[[listName.i[i]]])) {
      ALL.qt[, year.t > year.upto] <- NA
    } else {
      ALL.qt[, , year.t > year.upto] <- NA
    }
    
    data.out[[listName.i[i]]] <- ALL.qt    
  }#end of i loop
  
  return(data.out)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetResultExcel <- function (
  data.in = res, # input data base
  output.variable, # variable name that the output file is about
  ageGroup.a = ages.a, # age group number according to modeling
  ageGroupNames.a = ageGroupName.a,
  countryNames.c = name.c,
  countryISO.c = iso.c,
  year.t = years.t,
  dataout.dir = output.dir
) {
  
  ## create csv files for country-specific estimates in seperate file for each age group ##
  A <- length(ageGroup.a)
  C <- length(countryNames.c)
  
  for (a in 1:A) {
    nameindicator <- 
      ifelse(output.variable == "Qmale", paste0(ageGroupNames.a[a], "male"),
             ifelse(output.variable == "Qfemale",
                    paste0(ageGroupNames.a[a], "female"),
                    ifelse(output.variable == "Sex ratio", 
                           paste("Sex ratio", ageGroupNames.a[a]),
                           paste0("P", ageGroup.a[a]))))
    
    if (output.variable == "Qmale") {
      res.cqt <- res[[paste0("Q", ageGroup.a[a], "m.cqt")]]
    }
    if (output.variable == "Qfemale") {
      res.cqt <- res[[paste0("Q", ageGroup.a[a], "f.cqt")]]
    }
    if (output.variable == "Sex ratio") {
      res.cqt <- res[[paste0("S", ageGroup.a[a], ".cqt")]]
    }
    if (output.variable == "P") {
      res.cqt <- res[[paste0("P", ageGroup.a[a], ".cqt")]]
    }
    
    results <- NULL
    Per <- dim(res.cqt)[2]
    
    for (c in 1:C) {
      for (q in 1:Per) {
        results <- rbind(results, res.cqt[c, q, ])
      }#end of q loop
    }#end of c loop
    
    results.all <- results
    results.output <- cbind(data.frame(rep(countryNames.c, each = Per), 
                                       rep(countryISO.c, each = Per),
                                       rep(c("Lower", "Median", "Upper"), C),
                                       rep(nameindicator, C * Per)),
                            results.all)  
    colnames(results.output) <- c("Country Name", "ISO Code",  
                                  "Quantile",  "Indicator", #"Subgroup", 
                                  year.t)
    write.csv(results.output, 
              file = paste0(dataout.dir, "Results_", nameindicator, ".csv"), 
              row.names = FALSE, na = "")
  }#end of a loop  
  
}#end of GetResultExcel function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# TrajectoryToUI <- function (
#   trajectory.input, # input trajectory
#   NA.input = NA.jt, # same dimension as trajectory.input; indicate missings

# LA: is NA.jt defined???

#   percentile.p = percentiles, #output percentiles for input
#   missing.entry = FALSE #if need to add in NA entries
# ) {
#   
#   ## transfer trajectory to UI and median ##
#   if (!missing.entry) {
#     NA.input <- 0
#   }
#   
#   if (is.vector(trajectory.input)) {
#     result <- quantile(trajectory.input + NA.input, probs = percentile.p, na.rm = TRUE)
#   }
#   if (is.matrix(trajectory.input)) {
#     result <- apply(trajectory.input + NA.input, 2, quantile, percentile.p, na.rm = TRUE)
#   }
#   
#   return (result)
#   
# }#end of TrajectoryToUI function

SamplesToUI <- function (
  samples.jt, # inputs, note that it also works for samples.j
  NA.jt = 0, # indicate which entries should be set to NA. If NULL, it's not used.
  percentile.q = percentiles # output percentiles for input
) {
  # output: result.qt
  
  if (is.vector(samples.jt)) {
    result.qt <- quantile(samples.jt + NA.jt, probs = percentile.q, na.rm = TRUE)
  }
  if (is.matrix(samples.jt)) {
    result.qt <- apply(samples.jt + NA.jt, 2, quantile, percentile.q, na.rm = TRUE)
  }
  
  return (result.qt)
  
}#end of SamplesToUI function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ReformatPopulationData <- function(
  populationData.dir = popdata.dir,
  a1.filename,
  pop.filename,
  countryISO.c = iso.c,
  k.LifeTablea4 = lifetablea4,
  year.t = years.t
) {
  
  ## Reformat population data so that it fit to SR database ##
  
  data.a1 <- read.csv(file = paste0(populationData.dir, a1.filename), 
                      header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  data.pop <- read.csv(file = paste0(populationData.dir, pop.filename), 
                       header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  
  # make sure vetors end with *.c is of length C; *.t with length Tend
  C    <- length(countryISO.c)
  Tend <- length(year.t)
  
  a1.c <- rep(NA, C)
  pop1.ct <- pop4.ct <- matrix(NA, nr = C, nc = Tend)
  
  for (c in 1:C) {
    
    c.selectA1 <- which(data.a1[, "iso"] == countryISO.c[c])
    a1.c[c]    <- data.a1[c.selectA1, "a0"]
    
    for (t in 1:Tend) {
      
      c.select     <- which(data.pop[, "ISO3Code"] == countryISO.c[c])
      t.selectAge1 <- which(colnames(data.pop) == paste0("pop0",    floor(year.t[t])))
      t.selectAge4 <- which(colnames(data.pop) == paste0("pop1to4", floor(year.t[t])))
      
      pop1.ct[c, t] <- data.pop[c.select, t.selectAge1]
      pop4.ct[c, t] <- data.pop[c.select, t.selectAge4]
      
    }#end of t loop    
  }#end of c loop
  
  a4.c <- rep(k.LifeTablea4, C)
  
  result <- list(a1.c = a1.c, a4.c = a4.c,
                 pop1.ct = pop1.ct, pop4.ct = pop4.ct)
  
  return(result)
  
}#end of ReformatPopulationData function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetDataSource <- function(
  data.in,
  # default set of variable names to summarize information of data.in:
  getvars = c("Country.Code", "Country.ISO", "Series.Name",
                     "Series.Year", "Start.date.of.Survey",
                     "End.date.of.Survey", "Average.date.of.Survey",
                     "Series.Type", "Series.Category",
                     "Data.Collection.Method", "Series.Quantity",
                     "Year.From", "Year.To", "Interval", "Reference.Date",
                     "Inclusion"),
  add.vari = NULL, # set of variables to add to getvars
  del.vari = NULL  # set of variables to remove from getvars
) {
  
  ## to get data to include information on identifying data series only ##
  # default is to identify unique data series from a standard dataset
  # add or remove element(s) from getvars based on level of information
  # that you want to summarize.
  if (!is.null(add.vari)) getvars <- c(getvars, add.vari)
  if (!is.null(del.vari)) getvars <- setdiff(getvars, del.vari)
  # note: by this step, all variables in getvars should be included as
  # columns in data.in.
  
  # data base to identify unique set of infomation
  dataSource <- data.in[, getvars]
  
  sourceFull.i <- apply(dataSource, 1, paste, collapse = "_")
  sourceUniq.u <- unique(sourceFull.i)
  U <- length(sourceUniq.u)
  
  selectiFromSourceFull.u <- rep(NA, U)
  for (u in 1:U) {
    selectiFromSourceFull.u[u] <- which(sourceFull.i == sourceUniq.u[u])[1]
  }
  
  dataSourceUniq <- dataSource[selectiFromSourceFull.u, ]
  
  output <- list(dataSource = dataSource, dataSourceUniq = dataSourceUniq, 
                 sourceFull.i = sourceFull.i, sourceUniq.u = sourceUniq.u,
                 selectiFromSourceFull.u = selectiFromSourceFull.u)
  
  return(output)
  
}#end of GetDataSource function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ReformatCCdata <- function(
  input.data.name, # input data must be in the folder: data/input/
  reference.data, # reformat to the same structure as this file
  all.countries = FALSE # whether the data is from all countries (TRUE) or
  # one country only (FALSE)
) {
  
  ## reformat country consultation data file ##
  # "data/input/datasetfinal20120624.csv" is the reference.data in 2013 project
  
  if (all.countries) {
    data.in <- read.xlsx(paste0("data/input/", input.data.name),
                         sheetName = "Database", stringsAsFactors = FALSE)
    # identify empty columns with column names "NA..*"
    rmvEmptyColumns <- grep("NA", names(data.in))
    data.in <- data.in[, -rmvEmptyColumns] # discard empty columns
    # remove data from countries with other updated data files:
    # "Haiti Gabon All_20130807.xlsx" and "Sudan_20130807.xlsx"
    data.in <- data.in[-which(is.element(data.in[, "Country.Name"],
                                         c("Haiti", "Gabon", "Sudan"))), ]
  } else {
    data.in <- read.xlsx(paste0("data/input/", input.data.name),
                         sheetIndex = 1, stringsAsFactors = FALSE)    
  }
  
  # inclusion criteria: a) Inclusion column; b) sex-specific mortality;
  # c) age groups of children is Infant, Child, and Under-five.
  include <- data.in[, "Inclusion"] == 1 & data.in[, "Sex"] != "Total" &
    data.in[, "Indicator"] != "Neonatal Mortality Rate"
  
  # additional inclusion criterion if is one-country data
  if (!all.countries) {
    include <- include & data.in[, "Interval"] == 5
  } 
  
  dataset <- data.in[include, ]
  
  sourceDataset <- GetDataSource(data.in = dataset, add.vari = "Indicator")
  
  # reformat the input data so that it has the excatly same column names and
  # column classes as the reference.data. Instead of assigning entries to a
  # matrix, we create each column as a variable seperately, so that we can
  # match the corresponding class and combine them by using data.frame instead
  # of cbind or rbind when first time get data.out, and still keep the assigned
  # classes for variables.
  
  # construct variables from commen columns of reference.data and dataset
  # without any computation.
  colBoth <- intersect(names(reference.data), names(dataset))
  
  for (var in colBoth) {
    assign(var, dataset[sourceDataset$selectiFromSourceFull.u, var])
    #     print(var)
  }#end of var loop
  
  # get sex specific variables
  Sex.Ratio <- Male <- Female <- Both.Sexes <- 
    Sex.Ratio.SE <- Male.SE <-  Female.SE <- Both.Sexes.SE <-
    rep(NA, length(sourceDataset$sourceUniq.u))
  
  for (i in 1:length(sourceDataset$sourceFull.i)) {
    j <- which(sourceDataset$sourceUniq.u == sourceDataset$sourceFull.i[i])
    
    if (dataset[i, "Sex"]=="Male") {
      Male[j]    <- dataset[i, "Estimates"]
      Male.SE[j] <- dataset[i, "Standard.Error.of.Estimates"]
    }
    
    if (dataset[i, "Sex"] == "Female") {
      Female[j]    <- dataset[i, "Estimates"]
      Female.SE[j] <- dataset[i, "Standard.Error.of.Estimates"]
    }
    
    # read in sex ratio and SE for one-country data; need to compute them
    # for all-country data outside this i loop.
    if (!all.countries & dataset[i, "Sex"] == "Sex Ratio") {
      Sex.Ratio[j]    <- dataset[i, "Estimates"]
      Sex.Ratio.SE[j] <- dataset[i, "Standard.Error.of.Estimates"]
    }    
  }#end of i loop
  
  # assign classes for variables
  for (var in names(reference.data)) {
    eval(parse(text = paste0("class(", var, ") <- class(reference.data$", var, ")")))
  }#end of var loop
  # make sure to get data.out as a data frame.
  data.out <- 
    eval(parse(text = paste0("data.frame(", paste(names(reference.data),
                                                  collapse = ", "), ")")))
  
  # compute and add in sex ratio for IMR, U5MR, and CMR related information.
  if (all.countries) {
    data.out[, "Sex.Ratio"] <- GetS(Male, Female)
    
    sourceDataout <- GetDataSource(data.in = data.out)    
    q4male <- q4female <- q4malePosi <- q4femalePosi <- q4Matrix <- NULL
    
    for (u in 1:length(sourceDataout$sourceUniq.u)) {

      j <- which(sourceDataout$sourceUniq.u[u] == sourceDataout$sourceFull.i)
      # the following computation is meaningful only if both IMR and U5MR
      # information exist, so that we can compute CMR-related variables.
      if (length(j) == 2) {
        j.U5MR <- j[which(data.out[j, "Indicator"] == "Under-five Mortality Rate")]
        j.IMR  <- j[which(data.out[j, "Indicator"] == "Infant Mortality Rate")]
        
        # copy all the other information and then replace CMR-specific entries.
        q4Row <- data.out[j[1], ]
        q4Row["Indicator"] <- "Child Mortality Rate"
        
        # IGME estimate of IMR, CMR and U5MR are with value *Qunit (=1000) already
        # so we need to first divide by Qunit to get the actual value
        # then multiply by Qunit again in order to be consistent to the IGME file
        q4Row["Male"] <- GetQ4fromQ15(q1 = data.out[j.IMR, "Male"] / Qunit,
                                      q5 = data.out[j.U5MR, "Male"] / Qunit) * Qunit
        q4Row["Female"] <- GetQ4fromQ15(q1 = data.out[j.IMR, "Female"] / Qunit,
                                        q5 = data.out[j.U5MR, "Female"] / Qunit) * Qunit
        q4Row["Sex.Ratio"] <- GetS(q4Row["Male"], q4Row["Female"])
        
        # not able to compute sex-specific SE based on available information
        q4Row[c("Male.SE", "Female.SE", "Sex.Ratio.SE")] <- NA
        
        q4Matrix <- rbind(q4Matrix, q4Row)        
      }#end of if (length(j) == 2)      
    }#end of u loop
    
    data.out <- rbind(data.out, q4Matrix)
    
  }#end of if (all.countries)
  
  return(data.out)
  
}#end of ReformatCCdata function

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

GetInputData <- function(
  inputBaseData.name = NULL, # data from all countries
  inputData.name1 = NULL,
  inputData.name2 = NULL,
  inputData.name3 = NULL
) {
  
  ## combine datasets to get input file for modeling ##
  
  #data from all countries
  dataraw1 <- read.csv(paste0("data/input/", inputBaseData.name), stringsAsFactors = FALSE)
  
  ## note: need to go back to dataraw1 to check if new data can make up the old missing data
  dataAdd1 <- ReformatCCdata(input.data.name = inputData.name1, reference.data = dataraw1, all.countries = TRUE)
  dataAdd2 <- ReformatCCdata(input.data.name = inputData.name2, reference.data = dataraw1)
  dataAdd3 <- ReformatCCdata(input.data.name = inputData.name3, reference.data = dataraw1)
  
  dataraw <- rbind(dataraw1, dataAdd1, dataAdd2, dataAdd3)
  
  return(dataraw)
  
}#end of GetInputData function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


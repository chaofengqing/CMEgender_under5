
##############################################################
## explore the relationship of P < 1 and excess Qfemale > 0 ##
for(yr in yearSex){
  yr.posi <- which(yr == years)
  for(age in ages){
    P.c <- res[[paste0("P",age,".cqt")]][,3,yr.posi]
    excQf.c <- res[[paste0("excQ",age,"f.cqt")]][,1,yr.posi]*1000
    
    ## outlying countries picked by P < 1 ##
    country.P <- name.c[P.c < 1]
    ## outlying countries picked by excQf > 0 ##
    country.excQf <- name.c[excQf.c > 0]
    
    if(identical(country.excQf, country.P)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by excess Qfemale and by P are the same."))
    }
    if(prod(is.element(country.excQf, country.P)) == 1 & !identical(country.excQf, country.P)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by excess Qfemale is a subset of those by P."))
    }#end of if
    if(prod(is.element(country.P, country.excQf)) == 1 & !identical(country.excQf, country.P)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by P is a subset of those by excess Qfemale."))
      print(setdiff(country.excQf,country.P))
    }#end of if    
  }#end of age loop  
}#end of yr loop
#outlying countries identifyied by excQf is always a subset of those by P..
#Because P does not take into account of uncertainty of Qtotal, it only relate to median: Q1/4/5.ct, not Q1/4/5.ctj!!
res[[paste0("expQ",4,"f.cqt")]][name.c=="Serbia",,years==2010.5]*1000
res[[paste0("P",4,".cqt")]][name.c=="Serbia",,years==2010.5]

res[[paste0("excQ",4,"f.cqt")]][name.c=="United Kingdom",,years==2010.5]*1000
res[[paste0("P",4,".cqt")]][name.c=="United Kingdom",,years==2010.5]

###############################################################
## explore the relationship of Px < 1 and excess Qfemale > 0 ##
for(yr in yearSex){
  yr.posi <- which(yr == years)
  for(age in ages){
    Px.c <- res[[paste0("Px",age,".cqt")]][,3,yr.posi]
    excQf.c <- res[[paste0("excQ",age,"f.cqt")]][,1,yr.posi]*1000
    
    ## outlying countries picked by P < 1 ##
    country.Px <- name.c[Px.c < 1]
    ## outlying countries picked by excQf > 0 ##
    country.excQf <- name.c[excQf.c > 0]
    
    if(identical(country.excQf, country.Px)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by excess Qfemale and by P are the same."))
    }
    if(prod(is.element(country.excQf, country.Px)) == 1 & !identical(country.excQf, country.Px)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by excess Qfemale is a subset of those by P."))
    }#end of if
    if(prod(is.element(country.Px, country.excQf)) == 1 & !identical(country.excQf, country.Px)){
      print(paste0("age group ",age,", year ",years[yr.posi],": Outlying countries picked by P is a subset of those by excess Qfemale."))
      print(setdiff(country.excQf,country.Px))
    }#end of if    
  }#end of age loop  
}#end of yr loop
#outlying countries identified by Px and excQf are the same for all age groups and all selected years!!


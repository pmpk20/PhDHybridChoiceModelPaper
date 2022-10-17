#### PhDP2: Fitting CV From Probit Models ####
## Function: Fits Estimated WTP
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 17/10/2022


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# [1] MASS_7.3-56    compiler_4.1.3 tools_4.1.3    renv_0.15.4  

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(tidyr)
library(DCchoice)
library(apollo)
library(tidyverse)
library(here)
library(data.table)
library(Rfast)
library(matrixStats)
library(ggdist)



#------------------------------
# Section 1: Import Data ####
#------------------------------


## This is the latest version of the data:
FullSurvey2 <- data.frame(fread(here("Data","FullSurvey2.csv")))


#-------------------------------------------------------------------------------------
# Section 2: Estimate Q1 WTP With Covariates ####
#-------------------------------------------------------------------------------------

## Estimation:
Q1_SBDCModel_Covariates <- sbchoice(
  Q6ResearchResponse ~ 
    Q1Gender + Q2Age + 
    Q3Distance+ Q16BP + 
    Q18Charity +Q6ResearchCertainty+ 
    Q21Experts + IncomeDummy +
    Q20Consequentiality| 
    Q6Bid, data = FullSurvey2,dist="normal")

Q1_SBDCModel_Covariates_WTP <- krCI(Q1_SBDCModel_Covariates)
summary(Q1_SBDCModel_Covariates)


#-------------------------------------------------------------------------------------
# Section 3: Estimate Q2 WTP With Covariates ####
#-------------------------------------------------------------------------------------



## Estimation:
Q2_SBDCModel_Covariates <-
  sbchoice(
    Q7TreatmentResponse ~ 
      Q1Gender + Q2Age + 
      Q3Distance + Q16BP + 
      Q18Charity + Q7TreatmentCertainty+ 
      Q21Experts + IncomeDummy + 
      Q20Consequentiality |
      Q7Bid,
    data = FullSurvey2,
    dist = "normal"
  )

Q2_SBDCModel_Covariates_WTP <- krCI(Q2_SBDCModel_Covariates)
summary(Q2_SBDCModel_Covariates)


#---------------------------------------
# Section 4: Fit Q1 WTP per Respondent ####
#---------------------------------------


## Initialise vector with value zeroes
FullSurvey2$Q1WTPFitted <- rep(0,nrow(FullSurvey2))


## Used to do in apply but now in function
### Estimate mean WTP for each person
for (i in 1:nrow(FullSurvey2)) {
  FullSurvey2$Q1WTPFitted[i] <- krCI(
    Q1_SBDCModel_Covariates,
    individual = data.frame(
      Q1Gender = FullSurvey2$Q1Gender[i],
      Q2Age = FullSurvey2$Q2Age[i],
      Q3Distance = FullSurvey2$Q3Distance[i],
      Q16BP = FullSurvey2$Q16BP[i],
      Q18Charity = FullSurvey2$Q18Charity[i],
      Q6ResearchCertainty = FullSurvey2$Q6ResearchCertainty[i],
      Q21Experts = FullSurvey2$Q21Experts[i],
      IncomeDummy = FullSurvey2$IncomeDummy[i],
      Q20Consequentiality = FullSurvey2$Q20Consequentiality[i]))$out[1,1]
}

FullSurvey2$Q1WTPFitted

#---------------------------------------
# Section 5: Fit Q2 WTP per Respondent ####
#---------------------------------------


## Initialise vector with value zeroes
FullSurvey2$Q2WTPFitted <- rep(0,nrow(FullSurvey2))


## Used to do in apply but now in function
### Estimate mean WTP for each person
for (i in 1:nrow(FullSurvey2)) {
  FullSurvey2$Q2WTPFitted[i] <- krCI(
    Q2_SBDCModel_Covariates,
    individual = data.frame(
      Q1Gender = FullSurvey2$Q1Gender[i],
      Q2Age = FullSurvey2$Q2Age[i],
      Q3Distance = FullSurvey2$Q3Distance[i],
      Q16BP = FullSurvey2$Q16BP[i],
      Q18Charity = FullSurvey2$Q18Charity[i],
      Q7TreatmentCertainty = FullSurvey2$Q7TreatmentCertainty[i],
      Q21Experts = FullSurvey2$Q21Experts[i],
      IncomeDummy = FullSurvey2$IncomeDummy[i],
      Q20Consequentiality = FullSurvey2$Q20Consequentiality[i]))$out[1,1]
}

FullSurvey2$Q2WTPFitted

#---------------------------------------
# Section 6: Export WTP ####
#---------------------------------------


saveRDS(FullSurvey2$Q1WTPFitted,"Q1_SBDCModel_Covariates_FittedWTP.rds")
saveRDS(FullSurvey2$Q2WTPFitted,"Q2_SBDCModel_Covariates_FittedWTP.rds")




# End Of Script ---------------------------------------------------
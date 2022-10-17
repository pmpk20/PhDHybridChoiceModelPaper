#### Willingness-to-pay for precautionary control of microplastics, a comparison of hybrid choice models. Paper ####
## Function: Summary of WTP from each CV model
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 17/10/2022
## TODO: add CV code


#-------------------------------
#### Replication Information ####
#-------------------------------

# R version 4.0.2 (2020-06-22)
# RStudio Version 1.3.959
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Package information:
# stats     graphics  grDevices utils     datasets  methods   base     
# Rcpp_1.0.5          BiocManager_1.30.10 compiler_4.0.2      RSGHB_1.2.2        
# prettyunits_1.1.1   miscTools_0.6-26    tools_4.0.2         digest_0.6.25      
# pkgbuild_1.0.8      lattice_0.20-41     Matrix_1.2-18       cli_2.0.2          
# rstudioapi_0.11     maxLik_1.4-4        mvtnorm_1.1-1       SparseM_1.78       
# xfun_0.15           coda_0.19-4         MatrixModels_0.4-1  grid_4.0.2         
# glue_1.4.1          R6_2.4.1            randtoolbox_1.30.1  processx_3.4.2     
# fansi_0.4.1         callr_3.4.3         ps_1.3.3            mcmc_0.9-7         
# MASS_7.3-51.6       assertthat_0.2.1    mnormt_2.0.2        xtable_1.8-4       
# numDeriv_2016.8-1.1 Deriv_4.1.0         quantreg_5.55       sandwich_2.5-1     
# tinytex_0.24        MCMCpack_1.4-9      rngWELL_0.10-6      tmvnsim_1.0-2      
# crayon_1.3.4        zoo_1.8-8           apollo_0.2.1   

#-------------------------------
#### Setup: ####
#-------------------------------

library(DCchoice)
FullSurvey2 <- data.frame(read.csv("FullSurvey2.csv")) 

#-------------------------------
#### Question One: Bid-Only ####
#-------------------------------


## Estimation:
Q1_SBDCModel_BidOnly <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = FullSurvey2,dist="logistic")
Q1_SBDCModel_BidOnly_WTP <- krCI(Q1_SBDCModel_BidOnly)
summary(Q1_SBDCModel_BidOnly)


## WTP (Median):
Q1_SBDCModel_BidOnly_WTP$out[4,1]

## Range:
cbind(Q1_SBDCModel_BidOnly_WTP$out[4,2],Q1_SBDCModel_BidOnly_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q1_SBDCModel_BidOnly)

## R2:
summary(Q1_SBDCModel_BidOnly)$adjpsdR2

## LogLik:
logLik(Q1_SBDCModel_BidOnly)


#-------------------------------
#### Question One: Covariates ####
#-------------------------------


## Estimation:
Q1_SBDCModel_Covariates <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                                    + Q16BP + Q18Charity +Q6ResearchCertainty
                                    + Q21Experts + IncomeDummy +Q20Consequentiality| Q6Bid, data = FullSurvey2,dist="normal")

Q1_SBDCModel_Covariates_WTP <- krCI(Q1_SBDCModel_Covariates)
summary(Q1_SBDCModel_Covariates)


## WTP (Median):
Q1_SBDCModel_Covariates_WTP$out[4,1]

## Range:
cbind(Q1_SBDCModel_Covariates_WTP$out[4,2],Q1_SBDCModel_Covariates_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q1_SBDCModel_Covariates)
BIC(Q1_SBDCModel_Covariates)

## R2:
summary(Q1_SBDCModel_Covariates)$adjpsdR2

## LogLik:
logLik(Q1_SBDCModel_Covariates)

Q1_SBDCModel_Covariates_FittedWTP <- data.frame(apply(FullSurvey2, 
                                                      1, 
                                                      function(i)
                                                        c(krCI(
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
                                                            Q20Consequentiality = FullSurvey2$Q20Consequentiality[i]
                                                          )
                                                        )$out[1, 1])))



#-------------------------------
#### Question One: Bid-Only Order 0 ####
#-------------------------------


Order0 <- FullSurvey2[FullSurvey2$Order==0,]
## Estimation:
Q1_SBDCModel_BidOnly_Order0 <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Order0,dist="logistic")
Q1_SBDCModel_BidOnly_Order0_WTP <- krCI(Q1_SBDCModel_BidOnly_Order0)
summary(Q1_SBDCModel_BidOnly_Order0)


## WTP (Median):
Q1_SBDCModel_BidOnly_Order0_WTP$out[4,1]

## Range:
cbind(Q1_SBDCModel_BidOnly_Order0_WTP$out[4,2],Q1_SBDCModel_BidOnly_Order0_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q1_SBDCModel_BidOnly_Order0)

## R2:
summary(Q1_SBDCModel_BidOnly_Order0)$adjpsdR2

## LogLik:
logLik(Q1_SBDCModel_BidOnly_Order0)



#-------------------------------
#### Question One: Bid-Only Order 1 ####
#-------------------------------


Order1 <- FullSurvey2[FullSurvey2$Order==1,]
## Estimation:
Q1_SBDCModel_BidOnly_Order1 <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Order1,dist="logistic")
Q1_SBDCModel_BidOnly_Order1_WTP <- krCI(Q1_SBDCModel_BidOnly_Order1)
summary(Q1_SBDCModel_BidOnly_Order1)


## WTP (Median):
Q1_SBDCModel_BidOnly_Order1_WTP$out[4,1]

## Range:
cbind(Q1_SBDCModel_BidOnly_Order1_WTP$out[4,2],Q1_SBDCModel_BidOnly_Order1_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q1_SBDCModel_BidOnly_Order1)

## R2:
summary(Q1_SBDCModel_BidOnly_Order1)$adjpsdR2

## LogLik:
logLik(Q1_SBDCModel_BidOnly_Order1)



#-------------------------------
#### Question One:Hybrid-Choice ####
#-------------------------------

Q1_HCM <- readRDS("Q1ICLV_model.rds")
AIC(Q1_HCM)
logLik(Q1_HCM)


#-------------------------------
#### Question Two: Bid-Only ####
#-------------------------------


## Estimation:
Q2_SBDCModel_BidOnly <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = FullSurvey2,dist="logistic")
Q2_SBDCModel_BidOnly_WTP <- krCI(Q2_SBDCModel_BidOnly)
summary(Q2_SBDCModel_BidOnly)


## WTP (Median):
Q2_SBDCModel_BidOnly_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_BidOnly_WTP$out[4,2],Q2_SBDCModel_BidOnly_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_BidOnly)

## R2:
summary(Q2_SBDCModel_BidOnly)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_BidOnly)


#-------------------------------
#### Question Two: Covariates ####
#-------------------------------


## Estimation:
Q2_SBDCModel_Covariates <- sbchoice(Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance
                                    + Q16BP + Q18Charity +Q7TreatmentCertainty
                                    + Q21Experts + IncomeDummy +Q20Consequentiality| Q7Bid, data = FullSurvey2,dist="normal")

Q2_SBDCModel_Covariates_WTP <- krCI(Q2_SBDCModel_Covariates)
summary(Q2_SBDCModel_Covariates)


## WTP (Median):
Q2_SBDCModel_Covariates_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_Covariates_WTP$out[4,2],Q2_SBDCModel_Covariates_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_Covariates)
BIC(Q2_SBDCModel_Covariates)

## R2:
summary(Q2_SBDCModel_Covariates)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_Covariates)





#-------------------------------
#### Question Two: Bid-Only Order 0 ####
#-------------------------------


Order0 <- FullSurvey2[FullSurvey2$Order==0,]
## Estimation:
Q2_SBDCModel_BidOnly_Order0 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Order0,dist="logistic")
Q2_SBDCModel_BidOnly_Order0_WTP <- krCI(Q2_SBDCModel_BidOnly_Order0)
summary(Q2_SBDCModel_BidOnly_Order0)


## WTP (Median):
Q2_SBDCModel_BidOnly_Order0_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_BidOnly_Order0_WTP$out[4,2],Q2_SBDCModel_BidOnly_Order0_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_BidOnly_Order0)

## R2:
summary(Q2_SBDCModel_BidOnly_Order0)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_BidOnly_Order0)



#-------------------------------
#### Question Two: Bid-Only Order 1 ####
#-------------------------------


Order1 <- FullSurvey2[FullSurvey2$Order==1,]
## Estimation:
Q2_SBDCModel_BidOnly_Order1 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Order1,dist="logistic")
Q2_SBDCModel_BidOnly_Order1_WTP <- krCI(Q2_SBDCModel_BidOnly_Order1)
summary(Q2_SBDCModel_BidOnly_Order1)


## WTP (Median):
Q2_SBDCModel_BidOnly_Order1_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_BidOnly_Order1_WTP$out[4,2],Q2_SBDCModel_BidOnly_Order1_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_BidOnly_Order1)

## R2:
Order0 <- FullSurvey2[FullSurvey2$Order==0,]
## Estimation:
Q2_SBDCModel_BidOnly_Order0 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Order0,dist="logistic")
Q2_SBDCModel_BidOnly_Order0_WTP <- krCI(Q2_SBDCModel_BidOnly_Order0)
summary(Q2_SBDCModel_BidOnly_Order0)


## WTP (Median):
Q2_SBDCModel_BidOnly_Order0_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_BidOnly_Order0_WTP$out[4,2],Q2_SBDCModel_BidOnly_Order0_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_BidOnly_Order0)

## R2:
summary(Q2_SBDCModel_BidOnly_Order0)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_BidOnly_Order0)



#-------------------------------
#### Question Two: Bid-Only Order 1 ####
#-------------------------------


Order1 <- FullSurvey2[FullSurvey2$Order==1,]
## Estimation:
Q2_SBDCModel_BidOnly_Order1 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Order1,dist="logistic")
Q2_SBDCModel_BidOnly_Order1_WTP <- krCI(Q2_SBDCModel_BidOnly_Order1)
summary(Q2_SBDCModel_BidOnly_Order1)


## WTP (Median):
Q2_SBDCModel_BidOnly_Order1_WTP$out[4,1]

## Range:
cbind(Q2_SBDCModel_BidOnly_Order1_WTP$out[4,2],Q2_SBDCModel_BidOnly_Order1_WTP$out[4,3])

## N:
nrow(FullSurvey2)

## AIC:
AIC(Q2_SBDCModel_BidOnly_Order1)

## R2:
summary(Q2_SBDCModel_BidOnly_Order1)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_BidOnly_Order1)
summary(Q2_SBDCModel_BidOnly_Order1)$adjpsdR2

## LogLik:
logLik(Q2_SBDCModel_BidOnly_Order1)


#-------------------------------
#### Question Two:Hybrid-Choice ####
#-------------------------------

Q2_HCM <- readRDS("Q2ICLV_model.rds")
AIC(Q2_HCM)
logLik(Q2_HCM)




#-------------------------------
#### Fitting individual-level WTP here ####
#-------------------------------


#### Median ####
Q1_SBDCModel_WTP <- data.frame(apply(FullSurvey2, 
                                     1, 
                                     function(i) c(krCI(Q1_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,1])))
Q2_SBDCModel_WTP <- data.frame(apply(FullSurvey2, 
                                     1, 
                                     function(i) c(krCI(Q2_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,1])))
saveRDS(Q1_SBDCModel_WTP,"Q1_SBDCModel_WTP.rds")
saveRDS(Q2_SBDCModel_WTP,"Q2_SBDCModel_WTP.rds")



#### Lower Bounds ####
Q1_SBDCModel_WTP_Lower <- data.frame("Q1WTP"=apply(FullSurvey2, 
                                                   1, 
                                                   function(i) c(krCI(Q1_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,2])))
Q2_SBDCModel_WTP_Lower <- data.frame(apply(FullSurvey2, 
                                           1, 
                                           function(i) c(krCI(Q2_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,2])))
saveRDS(Q1_SBDCModel_WTP_Lower,"Q1_SBDCModel_WTP_Lower.rds")
saveRDS(Q2_SBDCModel_WTP_Lower,"Q2_SBDCModel_WTP_Lower")



#### Upper Bounds ####

Q1_SBDCModel_WTP_Upper <- data.frame(apply(FullSurvey2, 
                                           1, 
                                           function(i) c(krCI(Q1_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,3])))
Q2_SBDCModel_WTP_Upper <- data.frame(apply(FullSurvey2, 
                                           1, 
                                           function(i) c(krCI(Q2_SBDCModel,individual = data.frame(Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i],Timing=FullSurvey2$Timing[i]))$out[1,3])))
saveRDS(Q1_SBDCModel_WTP_Upper,"Q1_SBDCModel_WTP_Upper.rds")
saveRDS(Q2_SBDCModel_WTP_Upper,"Q2_SBDCModel_WTP_Upper.rds")


#### End Of Script ####



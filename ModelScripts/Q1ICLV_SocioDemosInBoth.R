#### Willingness-to-pay for precautionary control of microplastics, a comparison of hybrid choice models. Paper ####
## Function: Estimates ICLV for Q6 ("Question One")
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 21/03/2022
## TODO: add plots

#### Replication Information ####
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

## Q6 ICLV:




rm(list=ls())
# pkgbuild::has_build_tools()
library(Hmisc)
library(xtable)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DCchoice)
library(apollo)
# library(mfx)
options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation


## Data Imports: 
Full_Final <- data.frame(read.csv("FinalData.csv")) # Import the final data for ease
Test_Apollo <- data.frame(read.csv("Test_Apollo.csv")) # Import the final data for ease

database <- Test_Apollo
# Setup the data for all truncated models:
# database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1


# Estimate model:
apollo_control = list(
  modelName  = "Q6ICLV_SDInBoth_2022_05_31",
  modelDescr = "Q6ICLV_SDInBoth_2022_05_31",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 10,
  noValidation=TRUE)


apollo_beta = c(intercept =0,b_bid    = 0,
                lambda            = 1, 
                beta_Age       = 0, 
                beta_Gender    = 0,
                beta_Distance  = 0, 
                beta_Income =0,
                beta_Experts =0,
                beta_BP =0,
                beta_Charity =0,
                beta_Certainty=0,
                beta_Cons=0,
                  gamma_Age=0,
                  gamma_Gender=0, 
                  gamma_Distance=0, 
                  gamma_Income=0, 
                  gamma_Experts=0, 
                  gamma_BP=0, 
                  gamma_Charity=0,
                  gamma_Certainty=0,
                  gamma_Cons=0,
                zeta_Q13   = 1, 
                zeta_Q14   = 1, 
                zeta_Q15   = 1, 
                tau_Q13_1  =-2, 
                tau_Q13_2  =-1, 
                tau_Q13_3  = 1, 
                tau_Q13_4  = 2, 
                tau_Q14_1  =-2, 
                tau_Q14_2  =-1, 
                tau_Q14_3  = 1, 
                tau_Q14_4  = 2, 
                tau_Q15_1  =-2, 
                tau_Q15_2  =-1, 
                tau_Q15_3  = 1, 
                tau_Q15_4  = 2)
apollo_fixed = c()


apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta"))


## Define structural model here
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] =   gamma_Age*Age +
    gamma_Gender*Q1Gender + 
    gamma_Distance*Distance + 
    gamma_Income*IncomeDummy + 
    gamma_Experts*Experts + 
    gamma_BP*BP + 
    gamma_Charity*Charity +
    gamma_Certainty*Q12CECertainty +
    gamma_Cons*Consequentiality +eta
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()


# ------------------------------------------------------------------------------
#### Section 3: Define Model ####
# ----------------------------------------------------------------------------


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  op_settings1 = list(outcomeOrdered = Q13CurrentThreatToSelf, 
                      V              = zeta_Q13*LV, 
                      tau            = c(tau_Q13_1, tau_Q13_2, tau_Q13_3, tau_Q13_4),
                      rows           = (Task==1),
                      componentName  = "indic_Q13")
  op_settings2 = list(outcomeOrdered = Q14FutureThreatToSelf, 
                      V              = zeta_Q14*LV, 
                      tau            = c(tau_Q14_1, tau_Q14_2, tau_Q14_3, tau_Q14_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q14")
  op_settings3 = list(outcomeOrdered = Q15ThreatToEnvironment, 
                      V              = zeta_Q15*LV, 
                      tau            = c(tau_Q15_1, tau_Q15_2, tau_Q15_3, tau_Q15_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q15")
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + 
                       b_bid*Q6Bid+
                       lambda*LV+
                       beta_Age*Age +
                       beta_Gender*Q1Gender + 
                       beta_Distance*Distance + 
                       beta_Income*IncomeDummy + 
                       beta_Experts*Experts + 
                       beta_BP*BP + 
                       beta_Charity*Charity +
                       beta_Certainty*Q12CECertainty +
                       beta_Cons*Consequentiality,
                     tau    = list(-100,0),
                     componentName  = "choice",
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ------------------------------------------------------------------------------
#### Section 4: Estimate, output, and save model here ####
# ----------------------------------------------------------------------------


Q6ICLV_SDInBoth_2022_05_31 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(Q6ICLV_SDInBoth_2022_05_31,modelOutput_settings = list(printPVal=TRUE))
xtable::xtable(round(data.frame(apollo_modelOutput(Q6ICLV_SDInBoth_2022_05_31,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
apollo_saveOutput(Q6ICLV_SDInBoth_2022_05_31,saveOutput_settings = list(printPVal=TRUE))

## Save wtp
Q6ICLV_SDInBoth_2022_05_31_UCWTP <- apollo_unconditionals(Q6ICLV_SDInBoth_2022_05_31,apollo_probabilities,apollo_inputs)
Q6ICLV_SDInBoth_2022_05_31_ConWTP <- apollo_conditionals(Q6ICLV_SDInBoth_2022_05_31,apollo_probabilities,apollo_inputs)


## Save outputs
saveRDS(Q6ICLV_SDInBoth_2022_05_31,"Q6ICLV_SDInBoth_2022_05_31.rds")
saveRDS(Q6ICLV_SDInBoth_2022_05_31_UCWTP,"Q6ICLV_SDInBoth_2022_05_31_UCWTP.rds")
saveRDS(Q6ICLV_SDInBoth_2022_05_31_ConWTP,"Q6ICLV_SDInBoth_2022_05_31_ConWTP.rds")
# Q6ICLV_SDInBoth_2022_05_31 <- readRDS("Q6ICLV_SDInBoth_2022_05_31.rds")

# WTP:
# Model <- Q6ICLV_SDInBoth_2022_05_31
# CVunconditionals7F <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs)
# ModelWTP <-apply((-Model$estimate["intercept"]/Model$estimate["b_bid"]+CVunconditionals7F$LV)*100,MARGIN = 1,FUN = mean)
# Q6ICLV_SDInBoth_2022_05_31WTP <- median(-Model$estimate["intercept"]+CVunconditionals7F$LV/Model$estimate["b_bid"])


# End Of Script ---------------------------------------------------
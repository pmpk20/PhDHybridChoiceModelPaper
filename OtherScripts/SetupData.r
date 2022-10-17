#### PhDP2: Import raw data and transform  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Function: Transform survey data to useful 
# Comment: I'm not rewriting this so it is horrible but functional
## Last change: 17/10/2022


#---------------------------------------------------------------------------------------------------------
#### Section 0: Replication Information ####
## Putting sessionInfo() here in case it helps
#----------------------------------------------------------------------------------------------------------
  
  
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
  
# Matrix products: default
# locale:
# [1] LC_COLLATE=English_United Kingdom.1252 
# [2] LC_CTYPE=English_United Kingdom.1252   
# [3] LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.1252 

## PACKAGE INFORMATION ##:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] DCchoice_0.1.0 scales_1.1.1   magrittr_2.0.1 dplyr_1.0.7    apollo_0.2.5  
# [6] raster_3.4-13  sp_1.4-5      
# 
# loaded via a namespace (and not attached):
# [1] tidyselect_1.1.1    zoo_1.8-9           purrr_0.3.4        
# [4] splines_4.1.1       lattice_0.20-44     colorspace_2.0-2   
# [7] generics_0.1.0      vctrs_0.3.8         MCMCpack_1.5-0     
# [10] utf8_1.2.2          survival_3.2-11     rlang_0.4.11       
# [13] pkgbuild_1.2.0      pillar_1.6.2        glue_1.4.2         
# [16] MLEcens_0.1-4       matrixStats_0.60.0  lifecycle_1.0.0    
# [19] MatrixModels_0.5-0  munsell_0.5.0       mvtnorm_1.1-2      
# [22] codetools_0.2-18    coda_0.19-4         miscTools_0.6-26   
# [25] callr_3.7.0         SparseM_1.81        ps_1.6.0           
# [28] RSGHB_1.2.2         quantreg_5.86       parallel_4.1.1     
# [31] fansi_0.5.0         Rcpp_1.0.7          xtable_1.8-4       
# [34] conquer_1.0.2       BiocManager_1.30.16 tmvnsim_1.0-2      
# [37] mcmc_0.9-7          maxLik_1.5-2        mnormt_2.0.2       
# [40] interval_1.1-0.7    Icens_1.62.0        digest_0.6.27      
# [43] processx_3.5.2      numDeriv_2016.8-1.1 grid_4.1.1         
# [46] cli_3.0.1           tools_4.1.1         sandwich_3.0-1     
# [49] perm_1.0-0.0        tibble_3.1.3        Formula_1.2-4      
# [52] crayon_1.4.1        pkgconfig_2.0.3     MASS_7.3-54        
# [55] ellipsis_0.3.2      Matrix_1.3-4        prettyunits_1.1.1  
# [58] randtoolbox_1.30.1  rstudioapi_0.13     R6_2.5.0           
# [61] rngWELL_0.10-6      compiler_4.1.1 



#----------------------------------------------------------------------------------------------------------
#### Section 1: PACKAGE SETUP ####
## Installs packages and imports data.
## Split into optional and libraries
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------

## Back in 2020 I used to write rm() and setwd() for each script
### I keep these in for completeness BUT they are bad practice:
### See: https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

rm(list=ls()) ## Clear workspace
# setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
# pkgbuild::has_build_tools() ## Used to have problems installing RTOOls on my laptop.
options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation


#----------------------------------------------------------------------------------------------------------
# Section 1: Importing Data ####
#----------------------------------------------------------------------------------------------------------


## Imports from the excel file straight from the survey companies website.
FullSurvey <- data.frame(fread(here("Data","RawData.csv")))


#----------------------------------------------------------------------------------------------------------
# Section 2: Renaming and Reordering ####
#----------------------------------------------------------------------------------------------------------



## Drop columns of no importance to the quantitative analysis, namely text responses.
FullSurvey <- FullSurvey[ -c(4,14,15,16,23,24,26,27,58,68,69)] 


## Renaming the survey from original names to made up ones to link to the survey better.
colnames(FullSurvey) <- c("ID","Timing","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
                          "Q5Knowledge","Q6Bid","Q7Bid","Q6ResearchResponse",
                          "Q6ResearchCertainty","Q7TreatmentResponse",
                          "Q7TreatmentCertainty","Q7Bid2Upper","Q7Bid2Lower",
                          "Q7TreatmentUpperResponse","Q7TreatmentLowerResponse","Q8DominatedTest",
                          "Q9Block","Q9Performance","Q9Emission","Q9Price",
                          "Q10Block","Q10Performance","Q10Emission","Q10Price",
                          "Q11Block","Q11Performance","Q11Emission","Q11Price",
                          "Q12Block","Q12Performance","Q12Emission","Q12Price",
                          "Q9Choice","Q10Choice","Q11Choice","Q12Choice","Q12CECertainty",
                          "Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment",
                          "Q16BP","Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other",
                          "Q18Charity","Q19Knowledge","Q20Consequentiality",
                          "Q21Experts","Q22Education","Q23Employment",
                          "Q24RonaImpact","Q24AIncome","Q25Understanding")  


FullSurvey2 <- FullSurvey ## Create a backup of the FullSurvey data


#----------------------------------------------------------------------------------------------------------


## Aim is to recode FACTORS as NUMERIC

# for (i in colnames(FullSurvey)){
#   if (is.factor(FullSurvey[[i]]) == TRUE){
#     FullSurvey2[[i]] <- as.numeric(FullSurvey[[i]])-1
#   }
# } ## Here convert all questions into numeric format for ease of analysis


for (i in colnames(FullSurvey)){
  if (is.factor(FullSurvey[[i]]) != TRUE){
    FullSurvey2[[i]] <- as.numeric(as.factor(as.character(FullSurvey[[i]])))-1
  }
} ## Use this if the previous one didn't work. An R update made the previous one not work. 


# FullSurvey2$Order[FullSurvey2$Order == 2] <-0 ## The order dummy should be 0 for Q6 > Q7 and 1 for Q7 > Q6


#----------------------------------------------------------------------------------------------------------
# Section 3: Recoding Variables ####
## This can obviously be done better but 
## it's >2yrs old and works so not touching it
#----------------------------------------------------------------------------------------------------------



## Here I update the age categories to take the midpoint of the brackets.
## There is almost certainlty an easier way but I wrote this ages ago and it works
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 0] <- 21.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 1] <- 32.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 2] <- 47.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 3] <- 63
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 4] <- 71
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 5.0] <- NA
FullSurvey2$Q2Age <- with(FullSurvey2, impute(FullSurvey2$Q2Age, 'random')) ## I replace the missing with a random imputed value
FullSurvey2$Q2Age <- as.numeric(FullSurvey2$Q2Age) ## Fixes error where the impute type isn't right.


## The loop got the distances ordered incorrectly and also didn't use midpoints.
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 7
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 6
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 2
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 7] <- 3
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 6] <- 1
FullSurvey2$Q3Distance <- FullSurvey2$Q3Distance + 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 6.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 15.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 4] <- 35
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 5] <- 50
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 6] <- NA
FullSurvey2$Q3Distance <- with(FullSurvey2, impute(FullSurvey2$Q3Distance, 'random')) ## I replace the missing with a random imputed value
FullSurvey2$Q3Distance <- as.numeric(FullSurvey2$Q3Distance)


## Reordering the knowledge categories to reflect higher knowledge = higher value
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 4] <- 5
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 1] <- 6
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 6] <- 4
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 3] <- 1
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 0] <- 3


## Changing to be unsure > quite sure > very sure
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 1] <-3
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 0] <- 1
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 3] <- 0


## Same here, the coder was confused over the ordering.
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 1] <-3
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 0] <- 1
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 3] <- 0


## More reordering here
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 1] <- 3
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 2] <- 1
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 3] <- 2


## More reordering here
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 1] <- 3
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 2] <- 1
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 3] <- 2


## Previously skipped or missed questions were set = 2 now they're NAs for ease of merging the two columns.
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 2] <- NA
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 2] <- NA


## As respondents did EITHER the upper or lower question there should only be one column. This requires using mutate and coalesce to merge the lower and upper responses.
FullSurvey <- mutate(Q7Bid2 = coalesce(FullSurvey$Q7Bid2Lower,FullSurvey$Q7Bid2Upper),.data = FullSurvey)
FullSurvey <- mutate(Q7Response2 = coalesce(FullSurvey$Q7TreatmentUpperResponse,FullSurvey$Q7TreatmentLowerResponse),.data = FullSurvey)


## Mutate merges two columns
FullSurvey2 <- mutate(Q7Bid2 = coalesce(FullSurvey2$Q7Bid2Lower,FullSurvey2$Q7Bid2Upper),.data = FullSurvey2)
FullSurvey2 <- mutate(Q7Response2 = coalesce(FullSurvey2$Q7TreatmentUpperResponse,FullSurvey2$Q7TreatmentLowerResponse),.data = FullSurvey2)


## These are fine so just copy across
FullSurvey2$Q6Bid <- FullSurvey$Q6Bid
FullSurvey2$Q7Bid <- FullSurvey$Q7Bid
FullSurvey2$Q7Bid2 <- FullSurvey$Q7Bid2


## The following section codes all the attributes as their actual values.
FullSurvey2$Q9Performance <- FullSurvey2$Q9Performance +1
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 2] <- 0.05
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 3] <- 0.50
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 1] <- 0.10
FullSurvey2$Q9Emission <- FullSurvey2$Q9Emission +1
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 2] <- 0.40
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 3] <- 0.90
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 1] <- 0.10
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 1] <- 2.5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 2] <- 5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 0] <- 1


## Converting automatic assignment into exact levels
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 1] <- 0.05
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 0] <- 0.10
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 0] <- 0.10
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 1] <- 0.40
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 2] <- 0.40
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 2] <- 1
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 1] <- 2.5
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 0] <- 0.5


## Converting automatic assignment into exact levels
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 0] <- 0.10
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 1] <- 0.05
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 0] <- 0.10
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 1] <- 0.90
FullSurvey2$Q11Price <- FullSurvey2$Q11Price+1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 1] <- 0.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 2] <- 1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 3] <- 2.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 4] <- 5


## Converting automatic assignment into exact levels
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 1] <- 0.05
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 2] <- 0.50
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 0] <- 0.10
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 1] <- 0.40
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 0] <- 0.10
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 1] <- 1
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 2] <- 2.5
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 0] <- 0.5
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 3] <- 5


## Again certainty was confused so reordering here for higher number = more certain.
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 1] <- 3
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 0] <- 1
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 3] <- 0


## The coder used zeros so changing that here by moving each value up one.
FullSurvey2$Q13CurrentThreatToSelf <- FullSurvey2$Q13CurrentThreatToSelf + 1
FullSurvey2$Q14FutureThreatToSelf <- FullSurvey2$Q14FutureThreatToSelf + 1
FullSurvey2$Q15ThreatToEnvironment <- FullSurvey2$Q15ThreatToEnvironment + 1


## More reordering of none > some > all
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 2] <- 3
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 0] <- 2
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 1] <- 0
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 3] <- 1


## Changing it to be 1 = Chosen, 0 = Not Chosen:
FullSurvey2$Q17_Firms <- 1-FullSurvey2$Q17_Firms
FullSurvey2$Q17_Cons <- 1-FullSurvey2$Q17_Cons
FullSurvey2$Q17_Gov <- 1-FullSurvey2$Q17_Gov
FullSurvey2$Q17_LA <- 1-FullSurvey2$Q17_LA
FullSurvey2$Q17_Other <- 1-FullSurvey2$Q17_Other


## More reordering here
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 2] <- 3
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 1] <- 2
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 3] <- 1


## Same problem with Q5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 4] <- 5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 1] <- 4
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 0] <- 1
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 3] <- 0
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 1] <- 3
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 0] <- 1


## Reordering the consequentiality beliefs
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 0] <- 3
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 1] <- 0
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 2] <- 1
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 3] <- 2
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==2] <- 3
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==1] <- 2
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==3] <- 1

## Belief in experts used a zero so just moving up one
FullSurvey2$Q21Experts <- FullSurvey2$Q21Experts +1


## Have to reorder education to GCSE > A level > Bachelor > Postgrad
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 4] <- 5
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 3] <- 4
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 1] <- 3
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 2] <- 1
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 0] <- 2
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 5] <- 0


## New order: NEET > Retired > Student > Part > Self > Full
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 0] <- 7
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 3] <- 0
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 6] <- 3
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 7] <- 6
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 2] <- 7
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 4] <- 2
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 7] <- 4


## Should be a dummy here with 2 for prefer not to say
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 1] <- 3
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 2] <- 1
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 3] <- 2


## Changing the income to the midpoint of the brackets
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 8] <- 750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 4] <- 2750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 5] <- 3500.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 2] <- 1750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 1] <- 1250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 3] <- 2250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 6] <- 4500.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 7] <- 5000
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 0] <- 250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 9] <- NA 
FullSurvey2$Q24AIncome <- with(FullSurvey2, impute(FullSurvey2$Q24AIncome, 'random')) ## Using random imputation for missing values
FullSurvey2$Q24AIncome <- as.numeric(FullSurvey2$Q24AIncome)
FullSurvey2$Q24RonaImpact <- as.numeric(FullSurvey2$Q24RonaImpact)


## Updating the final survey question
FullSurvey2$Q25Understanding[FullSurvey2$Q25Understanding == 1] <- 10
FullSurvey2$Q25Understanding[FullSurvey2$Q25Understanding == 0] <- 1


## Adding an ID column which replaces the respondent category in the original dataset.
FullSurvey2$ID <- seq.int(nrow(FullSurvey2))


## Correcting the survey timing column:
FullSurvey2$Timing <- FullSurvey$Timing


## Aim of the function is to express all variables in the FullSurvey data as factors
for (i in colnames(FullSurvey2)){
  if (is.factor(FullSurvey[[i]]) == TRUE){
    contrasts(FullSurvey2[,i]) <- contr.sum(nlevels(FullSurvey2[,i]))
  }
} ## Using dummy coding


#------------------------------
# Section 4: Adding CE Data ####
## Not pretty but works
#------------------------------


## Making a dataframe with the levels for Option A of the CE:
OptionA <- data.frame("Performance" =c(0,0,0,0), 
                      "Emission" =c(0,0,0,0),
                      "Price" =c(0,0,0,0))




## Full is a dataframe that transforms the FullSurvey data into an appropriate format for the estimation.
### The code repeats each row of the FullSurvey data for each choice the respondent made. Therefore, each respondent now has four rows one for Q9, Q10, Q11, Q12
Full <-
  cbind(slice(.data = FullSurvey2, 
              rep(1:n(), each = 4)), 
        slice(.data = OptionA, 
              rep(1:n(), times = nrow(FullSurvey2))))


##  Creating a dataframe with all the levels that the Price attribute took for alternative B in the CE. 
DBPrice_B <- data.frame(Price_B = 
                          c(t(data.frame(rep(data.frame(FullSurvey2["Q9Price"],
                                                        FullSurvey2["Q10Price"],
                                                        FullSurvey2["Q11Price"],
                                                        FullSurvey2["Q12Price"]),
                                             times=1)))[,]))


##  Creating a dataframe with all the levels that the Performance attribute took for alternative B in the CE. 
DBPerformance_B <- data.frame(Performance_B = 
                                c(t(data.frame(rep(data.frame(FullSurvey2["Q9Performance"],
                                                              FullSurvey2["Q10Performance"],
                                                              FullSurvey2["Q11Performance"],
                                                              FullSurvey2["Q12Performance"]),
                                                   times=1)))[,]))


##  Creating a dataframe with all the levels that the Emission attribute took for alternative B in the CE. 
DBEmission_B <- data.frame(Emission_B = 
                             c(t(data.frame(rep(data.frame(FullSurvey2["Q9Emission"],
                                                           FullSurvey2["Q10Emission"],
                                                           FullSurvey2["Q11Emission"],
                                                           FullSurvey2["Q12Emission"]),
                                                times=1)))[,]))


##  Creating a single column that contains all respondents CE choices. 
Choices <- data.frame(Choice = c(t(
  data.frame(rep(FullSurvey2[,36:39], times=1)))[,]))


##  Chopping and reorganising the columns of the Full dataframe into a new order which includes the attributes and their levels alongside all the choices in a single vector.
### The final argument creates a variable called TASK
Full <- data.frame(Full[,1:14],Full[,19],DBPrice_B, DBPerformance_B, DBEmission_B,
                   Full[,61:63],Choices, Full[,20],Full[,24],Full[,28],
                   Full[,32],Full[,40:60],
                   rep(1:4,times=nrow(FullSurvey2)))


##  Assigning column names for ease of analysis.
colnames(Full) <- c("ID","Timing","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
                    "Q5Knowledge","Q6Bid","Q7Bid","Q6ResearchResponse",
                    "Q6ResearchCertainty","Q7TreatmentResponse",
                    "Q7TreatmentCertainty","Q8DominatedTest",
                    "Price_B","Performance_B","Emission_B", 
                    "Performance_A","Emission_A","Price_A","Choice",
                    "Q9Block","Q10Block","Q11Block","Q12Block","Q12CECertainty",
                    "Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment",
                    "Q16BP","Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other",
                    "Q18Charity","Q19Knowledge","Q20Consequentiality",
                    "Q21Experts","Q22Education","Q23Employment",
                    "Q24RonaImpact","Q24AIncome","Q25Understanding","Q7Bid2","Q7Response2",
                    "Task")  


## This little section adds in the data about firms responsibility
## It just adds the variables together, renames them, and adds back
Responsibility <- data.frame(cbind(Full$Q17_Firms,Full$Q17_Cons,Full$Q17_Gov,Full$Q17_LA,Full$Q17_Other))
colnames(Responsibility) <- c("Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other")
Full <- cbind(Full,"Responsibility" =rowSums(Responsibility))


Fulls <- Full # Later I use the Full dataframe but just without choices as A or B
Full$av_A <- rep(1,nrow(Full)) # Add a vector of ones to show that the alternative choice is always available to respondents.
Full$av_B <- rep(1,nrow(Full)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
Full$Choice[Full$Choice == 0] <- "A"  ## Necessary here to change numeric to string
Full$Choice[Full$Choice == 1] <- "B" ## The MFORMULA looks for _B or _A so choice must be SQ or ALT


# Adding in income dummy:
FullSurvey2$IncomeDummy <- as.numeric(ifelse(FullSurvey2$Q24AIncome <=median(FullSurvey2$Q24AIncome),0,1))
# Full_Final$IncomeDummy <- as.numeric(ifelse(Full_Final$Q24AIncome <=median(FullSurvey2$Q24AIncome),0,1))




#----------------------------------------------------------------------------------------------------------
# Section 5: Changes for Apollo ####
#----------------------------------------------------------------------------------------------------------


## Collecting variables together 
Test_Apollo <- data.frame(Fulls$ID,Fulls$Task, Fulls$Q1Gender,
                          Fulls$Q2Age,as.numeric(Fulls$Q3Distance),Fulls$Q4Trips,
                          Fulls$Q13CurrentThreatToSelf,Fulls$Q14FutureThreatToSelf,
                          Fulls$Q15ThreatToEnvironment,
                          Fulls$Q16BP,Fulls$Q18Charity,
                          Fulls$Q20Consequentiality,Fulls$Q21Experts,
                          Fulls$Q22Education,Fulls$Q23Employment,
                          Fulls$Q25Understanding,Fulls[,16:21],
                          Fulls$Choice,as.numeric(Fulls$Q24AIncome),
                          Fulls$Order, as.numeric(Fulls$Task), Fulls$Q20Consequentiality,
                          Fulls$Q21Experts,
                          Fulls$Q6ResearchResponse,Fulls$Q6Bid,
                          Fulls$Q7Bid,Fulls$Q7Bid2,
                          Fulls$Q7TreatmentResponse,Fulls$Q7Response2,Fulls$Timing,
                          Fulls$Q12CECertainty,Fulls$Q6ResearchCertainty,Fulls$Q7TreatmentCertainty)

## Renaming for ease
colnames(Test_Apollo) <- c("ID","Task","Q1Gender","Age","Distance",
                           "Trips","Q13CurrentThreatToSelf","Q14FutureThreatToSelf",
                           "Q15ThreatToEnvironment","BP","Charity",
                           "Consequentiality",
                           "Experts","Education","Employment","Survey",
                           "Price_B","Performance_B","Emission_B",
                           "Performance_A","Emission_A","Price_A"
                           ,"Choice","Income", "Order","Task",
                           "Consequentiality","Experts",
                           "Q6ResearchResponse","Q6Bid",
                           "Q7Bid","Q7Bid2",
                           "Q7TreatmentResponse","Q7Response2","Timing",
                           "Q12CECertainty","Q6ResearchCertainty","Q7TreatmentCertainty")

# Tests_Dominated <- Test_Apollo[!Test_Apollo$ID %in% c(Test_Apollo$ID[ ((Test_Apollo$Task == 1) & (Test_Apollo$Choice ==1) & (grepl("SQ",rownames(Test_Apollo),fixed = TRUE) == FALSE)) ]),]
# Tests_Understanding <- Tests_Dominated[!Tests_Dominated$ID %in% c( unique(Tests_Dominated$ID[Tests_Dominated$Survey <= 5])),]
# Test_Apollo <- Tests_Understanding
Test_Apollo$IncomeDummy <- as.numeric(ifelse(Test_Apollo$Income <=median(FullSurvey2$Q24AIncome),0,1))

## Each alternative is always available so I just rep the 1 value for all respondents
Test_Apollo$av_A <- rep(1,nrow(Full)) 
Test_Apollo$av_B <- rep(1,nrow(Full)) 


## I think I change this later but basically changing [0,1] to [1,2] for the code
Test_Apollo$Q6ResearchResponse <- Test_Apollo$Q6ResearchResponse +1
Test_Apollo$Q7TreatmentResponse <- Test_Apollo$Q7TreatmentResponse +1
Test_Apollo$Bid_Alt <- rep(0,nrow(Test_Apollo)) ## I don't use this anymore but it was the alternative scenario for the CV estimation


## Here I recode the attribute levels:
## I did a lot of testing on which levels worked best and I think this is those:
## The negative sign for performance represents the expected WTA
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.05] <- -5
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.10] <- -10
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.50] <- -50
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.1] <- 10
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.9] <- 90 
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.4] <- 40 


## Could also just do Test_Apollo$Choice <- Test_Apollo$Choice+1
## But Apollo wanted data in [1,2] rather than [0,1]
Test_Apollo$Choice[Test_Apollo$Choice == 1] <- 2
Test_Apollo$Choice[Test_Apollo$Choice == 0] <- 1


##  Apollo requires a variable called database:
database = Test_Apollo

## Making some changes to the variable coding from [0,1] to [1,2]
database$Q7Response2 <- database$Q7Response2+1
database$Q6ResearchResponse <- database$Q6ResearchResponse-1


# ## Providing a truncated sample
# Test_Truncated <- Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] ## Fully truncated:
# Fulls2 <- Fulls[ (Fulls$ID) %in% c(AllCriteria),] ## The excluded responses
# database <- Test_Truncated ## Use this to estimate with the truncated sample instead



#----------------------------------------------------------------------------------------------------------
# Section 6: Export Data For Analysis ####
## I've started adding dates to filenames
## And I've added this here to avoid duplication when replicating:
#----------------------------------------------------------------------------------------------------------


## This is in the right format for Apollo
write.csv(database,file = paste0("Test_Apollo_",Sys.Date(),".csv"))

## Reformatted Survey Data:
write.csv(FullSurvey2,file = paste0("SurveyDataReformatted_",Sys.Date(),".csv"))


# End Of Script ------------------------------------------------------------------------------------------------------
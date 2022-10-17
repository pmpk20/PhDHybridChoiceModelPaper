#### PhDP2: Make Figure 2 from scratch ####
## Function: Reads in and combines fitted WTP then makes figure
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


## fread() much faster than read.csv()
FullSurvey2 <- data.frame(fread(here("Data","FullSurvey2.csv")))


## So this is fitted WTP from the probit models:
Q1_WTP <- data.frame(readRDS(here("Data","Q1_SBDCModel_Covariates_FittedWTP.rds")))
Q2_WTP <- data.frame(readRDS(here("Data","Q2_SBDCModel_Covariates_FittedWTP.rds")))

## Still from probit but upper and lower bounds
Q1_WTP_Lower <- data.frame(readRDS(here("Data","Q1_SBDCModel_WTP_Lower.rds")))
Q1_WTP_Upper <- data.frame(readRDS(here("Data","Q1_SBDCModel_WTP_Upper.rds")))

Q2_WTP_Lower <- data.frame(readRDS(here("Data","Q2_SBDCModel_WTP_Lower.rds")))
Q2_WTP_Upper <- data.frame(readRDS(here("Data","Q2_SBDCModel_WTP_Upper.rds")))


## These are the individual-level Latent Variables from the ICLV models:
Q1_LV <- data.frame(readRDS(here("Data","Q1ICLV_Conditionals.rds")))
Q2_LV <- data.frame(readRDS(here("Data","Q2ICLV_Conditionals.rds")))


#-------------------------------
#### Merging: ####
#-------------------------------

FullSurvey2 <- cbind(
  FullSurvey2,
  "Q1WTP"=Q1_WTP$readRDS.here..Data....Q1_SBDCModel_Covariates_FittedWTP.rds...,
  "Q2WTP"=Q2_WTP$readRDS.here..Data....Q2_SBDCModel_Covariates_FittedWTP.rds...,
  
  "Q1WTPLower"=Q1_WTP_Lower$Q1WTP,
  "Q2WTPLower"=Q2_WTP_Lower$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  
  "Q1WTPUpper"=Q1_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q6_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  "Q2WTPUpper"=Q2_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  
  "Q1LV"=Q1_LV$post..mean,
  "Q2LV"=Q2_LV$post..mean 
)



#-------------------------------
#### Boxplot of CV vs LV: ####
#-------------------------------




# Replicating Abate et al (2020) box and whiskers:
FullSurvey2$LVQuantilesQ1 <- (ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
FullSurvey2$LVQuantilesQ2 <- (ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
# quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))


## This part is Q1
Q1Box <- ggplot(FullSurvey2, aes(LVQuantilesQ1, Q1WTP)) +   
  geom_boxplot(coef=5,
               aes(group = LVQuantilesQ1),
               width = 0.1)+
  theme_bw()+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,150),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Question 1: WTP by percentile of latent variable.")+
  geom_text(x = 1.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2))),color="red")+
  geom_text(x = 2.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2))),color="red")+
  geom_text(x = 3.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2))),color="red")+
  geom_text(x = 4.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2))),color="red")+
  geom_text(x = 5.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2))),color="red")+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank())

## This is Q2
Q2Box <- ggplot(FullSurvey2, aes(LVQuantilesQ2, Q2WTP)) +   
  geom_boxplot(coef=5,
               aes(group = LVQuantilesQ2),
               width = 0.1)+
  theme_bw()+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,150),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Question 2: WTP by percentile of latent variable.")+
  geom_text(x = 1.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2))),color="blue")+
  geom_text(x = 2.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2))),color="blue")+
  geom_text(x = 3.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2))),color="blue")+
  geom_text(x = 4.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2))),color="blue")+
  geom_text(x = 5.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2))),color="blue")+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank())

#---------------------------------------
# Export Plots ####
#---------------------------------------


ggsave(grid.arrange(Q1Box,Q2Box), device = "jpeg",
       filename = "Figure2.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


# End Of Script ---------------------------------------------------
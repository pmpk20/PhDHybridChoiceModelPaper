#### Willingness-to-pay for precautionary control of microplastics, a comparison of hybrid choice models. Paper ####
## Function: Plots CV vs LV
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 22/03/2022
## TODO: tidy up plots



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
#### Density Plots of WTP: ####
#-------------------------------


Labels <- c("Q1\n Research Scenario", "Q2\n Treatment Scenario")


Figure3 <-  reshape2::melt(data.frame(cbind(
  "Q1"=Q1_LV$post..mean,
  "Q2"=Q2_LV$post..mean))) %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
  stat_histinterval()+
  scale_x_continuous(name="Estimate Of Latent Precaution", limits=c(-1,5),breaks = seq(-1,5,1))+
  scale_y_discrete(label=Labels)+
  coord_cartesian(xlim=c(-1,5),clip='off')+
  geom_vline(xintercept=0,linetype='dashed')+
  scale_fill_manual(name="Question",
                    values=c("red","blue"),
                    label=Labels,
                    guide=guide_legend(reverse = TRUE))+
  theme_bw()+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank())


#---------------------------------------
# Export Plots ####
#---------------------------------------

ggsave(Figure3, device = "jpeg",
       filename = "Figure3.jpeg",
       width=20,height=15,units = "cm",dpi=500)


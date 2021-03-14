rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
library(tidyverse)
library(gamlr)
library(xtable)
library(grid)
library(gridExtra)
library(cowplot)
#source(paste0(directoryname,"/R/Libraries.R"))
figdirectory<-paste0(directoryname,"/Figures/")
# Average Category Elasticity
# Second stage method for price elasticities is ordinary least squares
second_stage_method_names<<-c("OLS")

source(paste0(directoryname,"/R/utils_app.R"))
source(paste0(directoryname,"/R/SecondStage.R"))
source(paste0(directoryname,"/R/ss_methods.R"))
source(paste0(directoryname,"/R/Main.R"))

second_stage_method_names<<-c("OLS","Lasso","DebiasedLasso")
categorynames = c("Dairy","NonEdible","Snacks")
run_fs=FALSE
xlims<-list()
xlims[["Dairy"]]<-c(-10,10)
xlims[["Snacks"]]<-c(-5,5)
xlims[["NonEdible"]]<-c(-10,10)
grouping_level<-"Level1_Name"
figdirectory=paste0(directoryname,"/Figures/Figure3/")
tabledirectory=paste0(directoryname,"/Tables/Figure3/")

for (categoryname in c("Dairy","NonEdible","Snacks")) {
  ## load data
  my_data<-read.csv(paste0(directoryname,"/processed_data/",categoryname,".csv"))
  my_data$Level1<-as.factor(as.character(my_data$Level1))
  my_data$Level2<-as.factor(as.character(my_data$Level2))
  my_data$Level3<-as.factor(as.character(my_data$Level3))
  my_data$Level4<-as.factor(as.character(my_data$Level4))
  colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
  my_data$RowID<-as.character(my_data$RowID)
   
  inds_train<-(1:dim(my_data)[1])[(my_data$year==2013 & my_data$week<=8) + (my_data$year==2012)>=1]
  subset_inds<-inds_test<-setdiff( (1:dim(my_data)[1]),inds_train)
  
  ## load first stage estimates
  fs<-read.csv(paste0(directoryname,"/processed_data/first_stage/FirstStage",categoryname,".csv"))
  print (paste0("Reading estimated first-stage residuals from ",paste0(directoryname,"/processed_data/first_stage/",categoryname,".csv")))
  
  
  ## Average Categorywise Own Elasticity at Level 2
  
  
  het.name<-c("Level1","Level2")
  main(my_data=cbind(fs,my_data),
       categoryname=categoryname,het.name=het.name, 
       grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
       lambda_ridge=0.01,
       outname=paste0( "Level2",categoryname),xlims=xlims,figdirectory=figdirectory)
  

  het.name<-c("Level1","Level2","Level3")
  main(my_data=cbind(fs,my_data),
       categoryname=categoryname,het.name=het.name, 
       grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
       lambda_ridge=0.3,
       outname=paste0( "Level3",categoryname),figdirectory=figdirectory)
  
  # Average Category Elasticity at Level 4
  ## Snacks has no Level4 categories
  if (categoryname!="Snacks") {
    het.name<-c("Level1","Level2","Level3","Level4")
    main(my_data=cbind(fs,my_data),
         categoryname=categoryname,het.name=het.name, 
         grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
         lambda_ridge=0.9,
         outname=paste0( "Level4",categoryname),figdirectory=figdirectory)
  }
 
  
}
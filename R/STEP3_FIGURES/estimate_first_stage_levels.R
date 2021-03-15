rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
library(tidyverse)
library(gamlr)
library(xtable)
library(glmnet)
categorynames = c("Drinks", "Dairy","NonEdible","Snacks")
source(paste0(directoryname,"/R/FirstStage.R"))
treat_name="logpr"
outcome_name="logsales"
for (categoryname in categorynames) {
  ### load data
  my_data<-read.csv(paste0(directoryname,"/processed_data/",categoryname,".csv"))
  colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
  my_data$RowID<-as.character(my_data$RowID)
  my_data$logpr<-my_data$logprice-my_data$logprice_lag
  my_data$logsls<-my_data$logsales-my_data$logsales_lag
  
  #inds_train<-(1:dim(my_data)[1])[(my_data$year==2013 & my_data$week<=8) + (my_data$year==2012)>=1]
  # subset_inds<-inds_test<-setdiff( (1:dim(my_data)[1]),inds_train)
  
  ### estimate first stage in levels
  
  
  inds_train<-(1:dim(my_data)[1])
  lag.names<-grep("_lag",colnames(my_data),value=TRUE)
  level_names<-setdiff(grep("Level",colnames(my_data),value=TRUE  ),grep("_Name",colnames(my_data),value=TRUE  ))
  
  first_stage_price_formula<-as.formula(paste0("logpr~(",paste(c("week","month","SiteName","ChannelName","Item"),collapse="+"),
                                               ")+","(", paste(level_names,collapse="+"),")+Level1*(",
                                               paste(c("logprice_lag","logprice_lag_2"),collapse="+"),
                                               ")"))
  first_stage_sales_formula<-as.formula(paste0("~(",paste(c("week","month","SiteName","ChannelName","Item"),collapse="+"),
                                               ")+","(", paste(level_names,collapse="+"),")+(Level1+Level2+Level3)*(",
                                               paste(c("logprice_lag","logsales_lag"),collapse="+"),
                                               ")"))
  fs<-first_stage(treat=my_data[,treat_name],
                  outcome=my_data[,outcome_name],
                  first_stage_price_formula=first_stage_price_formula,
                  first_stage_sales_formula=first_stage_sales_formula,
                  inds_train=inds_train
  )
  
  write.csv(data.frame(treat_res=fs$treat,outcome_res=fs$outcome),paste0(directoryname,"/processed_data/first_stage_levels/FirstStage",categoryname,".csv"))
  
  
  ### estimate first stage in first differences
  
} 
rm(list=ls())
# set directoryname here
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)

## uncomment this line when first-time running
# install.packages(c("tidyverse", "gamlr", "xtable", "glmnet"))
library(tidyverse)
library(gamlr)
library(xtable)
library(glmnet)

source(paste0(directoryname,"/R/FirstStage.R"))
source(paste0(directoryname,"/R/utils_app.R"))
treat_name="logpr"
outcome_name="logsales"
categorynames = c( "Snacks","Dairy")

for (categoryname in categorynames ) {
    my_data<-read.csv(paste0(directoryname,"/processed_data/",categoryname,".csv"),
                  colClasses = c( "week" = "factor",
                                  "month"="factor",
                                  "SiteName"="factor",
                                  "ChannelName"="factor",
                                  "Item" = "factor",
                                  "Level1"="factor","Level2"="factor","Level3"="factor","Level4"="factor",
                                  "Level5"="factor"))

  colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
  my_data$RowID<-as.character(my_data$RowID)
  my_data$logpr<-my_data$logprice-my_data$logprice_lag
  my_data$logsls<-my_data$logsales-my_data$logsales_lag

  names( my_data)[names( my_data) == "logprice_lag"] <-"L1logprice"
  names( my_data)[names( my_data) == "logprice_lag_2"] <-"L2logprice"

  names( my_data)[names( my_data) == "logsales_lag"] <-"L1logsales"
  names( my_data)[names( my_data) == "logsales_lag_2"] <-"L2logsales"



  inds_train<-(1:dim(my_data)[1])

  level_names<-c("Level1_Name","Level2_Name","Level3_Name")
  first_stage_price_formula<-as.formula(paste0("~(",paste(c("Item","month","SiteName","ChannelName"),collapse="+"),
                                             ")+","(", paste(level_names,collapse="+"),")+Level1_Name*(",
                                             paste(c("L1logprice","L2logprice"),collapse="+"),
                                             ")"))

  level_names<-c("Level1_Name","Level2_Name","Level3_Name")
  first_stage_sales_formula<-as.formula(paste0("~(",paste(c("Item","month","SiteName","ChannelName"),collapse="+"),
                                             ")+","(", paste(level_names,collapse="+"),")+(Level1_Name+Level2_Name+Level3_Name)*(",
                                             paste(c("logprice","L1logprice","L1logsales"),collapse="+"),
                                             ")"))



  fs<-first_stage(treat=my_data[,treat_name],
                outcome=my_data[,outcome_name],
                first_stage_price_formula=first_stage_price_formula,
                first_stage_sales_formula=first_stage_sales_formula,
                inds_train=inds_train,my_cons=0.05)


  write.csv(data.frame(treat_res=fs$treat,outcome_res=fs$outcome),paste0(directoryname,"/processed_data/first_stage/FirstStage",categoryname,".csv"))

  price_equation<-data.frame(name=rownames( coef(fs$treat.fit$fit.c)),
                           est=as.numeric(coef(fs$treat.fit$fit.c)))

  price_equation<-price_equation[ price_equation$est!=0,]

  sales_equation<-data.frame(name=rownames( coef(fs$outcome.fit$fit.c)),
                           est=as.numeric(coef(fs$outcome.fit$fit.c)))

  sales_equation<-sales_equation[ sales_equation$est!=0,]


  write.table(print(xtable(price_equation,digits=3), type="latex",include.rownames =FALSE ),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=FALSE)
  write.table(as.character(first_stage_price_formula),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)
  write.table(paste0("lambda_price=", as.character(round(fs$treat.fit$fit.c$lambda,3))),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)
  write.table(paste0("sample_size=", as.character(round(fs$treat.fit$fit.c$nobs,3))),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)


  write.table(print(xtable(sales_equation,digits=3), type="latex",include.rownames =FALSE ),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=FALSE)
  write.table(as.character(first_stage_sales_formula),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)
  write.table(paste0("lambda_sales=", as.character(fs$outcome.fit$fit.c$lambda,3)),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)
  write.table(paste0("sample_size=", as.character(fs$outcome.fit$fit.c$nobs,3)),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)
}
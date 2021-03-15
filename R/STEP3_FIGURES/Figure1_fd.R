rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
library(tidyverse)
library(gamlr)
library(xtable)
#source(paste0(directoryname,"/R/Libraries.R"))

# Average Category Elasticity
# Second stage method for price elasticities is ordinary least squares


source(paste0(directoryname,"/R/utils_app.R"))
source(paste0(directoryname,"/R/SecondStage.R"))
source(paste0(directoryname,"/R/ss_methods.R"))

second_stage_method_names=c("OLS")
categorynames = c("Drinks", "Dairy","NonEdible","Snacks")




for (categoryname in categorynames) {
  ### load data
  my_data<-read.csv(paste0(directoryname,"/processed_data/",categoryname,".csv"))
  colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
  my_data$RowID<-as.character(my_data$RowID)
  my_data$logpr<-my_data$logprice-my_data$logprice_lag
  
  #inds_train<-(1:dim(my_data)[1])[(my_data$year==2013 & my_data$week<=8) + (my_data$year==2012)>=1]
  #subset_inds<-inds_test<-setdiff( (1:dim(my_data)[1]),inds_train)
  subset_inds<-inds_test<-(1:dim(my_data)[1])
  # load first stage residuals
  fs<-read.csv(paste0(directoryname,"/processed_data/first_stage_fd/FirstStage",categoryname,".csv"))
  print (paste0("Reading estimated first-stage residuals from ",paste0(directoryname,"/processed_data/first_stage_fd/",categoryname,".csv")))
  
  
  
  ## estimate elasticities and standard errors
  if (categoryname == "Drinks") {
    het.name="Level2_Name"
    ## elasticity estimate and standard error
    ss<-second_stage(my_data=cbind(fs,my_data)[subset_inds,],categoryname=categoryname,het.name="Level2_Name",
                     second_stage_method_names=c("OLS"))
    ## data cleaning
    ss$OLS<-left_join(ss$OLS,select(my_data,one_of("RowID",het.name,"Level2")),by=c("RowID"="RowID"))%>%
      select(.,-RowID) 
    colnames(ss$OLS)[ colnames(ss$OLS)==het.name]<-"xbreaks"
    ss$OLS<-ss$OLS[order(ss$OLS$Level2),]
    ss$OLS$est<-sapply( ss$OLS$est,round,3)
    ss$OLS$st.error.hat<-sapply( ss$OLS$st.error.hat,round,3)
    
    ## save the result
    figdirectory<-paste0(directoryname,"/Figures/Figure1_fd/")
    ## as figure
    boxwhisker(data=ss$OLS,het.name=het.name,outname=paste0(categoryname,"Level1"), figdirectory= figdirectory)
    ## as table
    write.csv(ss$OLS,paste0(directoryname,"/Tables/","/Figure1_fd/",categoryname,"Level1"))
  } else {
    het.name="Level1_Name"
    ss<-second_stage(my_data=cbind(fs,my_data)[subset_inds,],categoryname=categoryname,het.name="Level1_Name",
                     second_stage_method_names=c("OLS"))
    
    ss$OLS<-left_join(ss$OLS,select(my_data,one_of("RowID",het.name,"Level1")),by=c("RowID"="RowID")) %>%
      select(.,-RowID)
    colnames(ss$OLS)[ colnames(ss$OLS)==het.name]<-"xbreaks"
    ss$OLS<-ss$OLS[order(ss$OLS$Level1),]
    ss$OLS$est<-sapply( ss$OLS$est,round,3)
    ss$OLS$st.error.hat<-sapply( ss$OLS$st.error.hat,round,3)
    
    
    figdirectory<-paste0(directoryname,"/Figures/Figure1_fd/")
    boxwhisker(data=ss$OLS,het.name=het.name,outname=paste0(categoryname,"Level1"),figdirectory=figdirectory)
    write.csv(ss$OLS,paste0(directoryname,"/Tables/","/Figure1_fd/",categoryname,"Level1"))
    
  
    
    
  }
  
  
}


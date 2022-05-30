rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"

#directoryname<-"/Users/virasemenova/Dropbox (MIT)/orthoml_draft/"
setwd(directoryname)
library(tidyverse)
library(gamlr)
library(xtable)
library(grid)
library(gridExtra)
library(cowplot)
library(reshape)
library(grplasso)
#source(paste0(directoryname,"/R/Libraries.R"))
figdirectory<-paste0(directoryname,"/Figures/")
# Average Category Elasticity
# Second stage method for price elasticities is ordinary least squares

source(paste0(directoryname,"/R/utils_app.R"))
source(paste0(directoryname,"/R/SecondStage.R"))
source(paste0(directoryname,"/R/ss_methods.R"))
source(paste0(directoryname,"/R/Main.R"))

second_stage_method_names<<-c("OLS","Lasso","DebiasedLasso","GLasso","DebiasedGLasso")


run_fs=FALSE
grouping_level<-"Level1_Name"
figdirectory=paste0(directoryname,"/Figures/")
tabledirectory=paste0(directoryname,"/Tables/")

categoryname="Dairy"


my_data1<-read.csv(paste0(directoryname,"/processed_data/Dairy1.csv"),
                   colClasses = c( "week" = "factor",
                                   "month"="factor",
                                   "SiteName"="factor",
                                   "ChannelName"="factor",
                                   "Item" = "factor",
                                   "Level1"="factor","Level2"="factor","Level3"="factor","Level4"="factor",
                                   "Level5"="factor"))



my_data2<-read.csv(paste0(directoryname,"/processed_data/Dairy2.csv"),
                   colClasses = c( "week" = "factor",
                                   "month"="factor",
                                   "SiteName"="factor",
                                   "ChannelName"="factor",
                                   "Item" = "factor",
                                   "Level1"="factor","Level2"="factor","Level3"="factor","Level4"="factor",
                                   "Level5"="factor"))
my_data<-rbind(my_data1,my_data2)
colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
my_data$RowID<-as.character(my_data$RowID)
subset_inds<-inds_test<-(1:dim(my_data)[1])
## load first stage estimates


fs<-read.csv(paste0(directoryname,"/processed_data/first_stage/FirstStage",categoryname,".csv"))
print (paste0("Reading estimated first-stage residuals from ",paste0(directoryname,"/processed_data/first_stage/",categoryname,".csv")))

het.name<-c("Level1","Level2")
ss_estimates<-main(my_data=cbind(fs,my_data),
                   categoryname=categoryname,het.name=het.name, 
                   grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
                   lambda_ridge=2*log(length(subset_inds)),outname=paste0( "Level2",categoryname),
                   xlim=c(-3,1),figdirectory=figdirectory,all_levels=c("Level1_Name",    "Level2_Name"))

## number of distinct Lasso elasticities
length(unique(ss_estimates$Lasso))
## number of positive OLS elasticities
sum(ss_estimates$OLS>0)
## number of positive DebiasedLasso elasticities
sum(ss_estimates$DebiasedLasso>0)

sum(ss_estimates$DebiasedGLasso>0)

write.table(print(xtable(ss_estimates[,c(1:2,5,9,3,4,8,6,9,10)],digits=3),include.rownames=FALSE), paste0(tabledirectory,"/",categoryname,"/Level2.txt"))

#write.table(print(xtable(ss_estimates[,c(1,2,6,7,10)],digits=3),include.rownames=FALSE), paste0(tabledirectory,"/",categoryname,"/Level2group.txt"))



het.name<-c("Level1","Level2","Level3")
ss_estimates<-main(my_data=cbind(fs,my_data),
                   categoryname=categoryname,het.name=het.name, 
                   grouping_level=grouping_level,second_stage_method_names=c("OLS","Lasso","DebiasedLasso"),
                   lambda_ridge=2*log(length(subset_inds)),outname=paste0( "Level4",categoryname),
                   xlim=c(-7,0.5),figdirectory=figdirectory,table=FALSE,all_levels=c("Level1_Name",    "Level2_Name","Level3_Name"))
ss_estimates<-left_join(ss_estimates,my_data[,c("Level1","Level2","Level3",
                                                "Level1_Name","Level2_Name","Level3_Name")])
write.table(print(xtable(ss_estimates,digits=3),include.rownames=FALSE), paste0(tabledirectory,"/",categoryname,"/Level3.txt"))


  het.name<-c("Level1","Level2","Level3","Level4")
  ss_estimates<-main(my_data=cbind(fs,my_data),
                     categoryname=categoryname,het.name=het.name, 
                     grouping_level=grouping_level,second_stage_method_names=c("OLS","Lasso","DebiasedLasso"),
                     lambda_ridge=2*log(length(subset_inds)),outname=paste0( "Level3",categoryname),
                     xlim=c(-7,0.5),figdirectory=figdirectory,table=FALSE,
                     all_levels=c("Level1_Name",    "Level2_Name","Level3_Name","Level4_Name"))
  write.table(print(xtable(ss_estimates,digits=3),include.rownames=FALSE), paste0(tabledirectory,"/",categoryname,"/Level4.txt"))
  


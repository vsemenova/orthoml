rm(list=ls())
# set directoryname here
directoryname<-"/n/tata/orthoml/"
#directoryname<-"/Users/virasemenova/Dropbox (MIT)/orthoml_draft/"
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
categoryname="NonEdible"
my_cons=0.05
## partition into two halves for github upload
my_data1<-read.csv(paste0(directoryname,"/processed_data/NonEdible1.csv"),
                   colClasses = c( "week" = "factor",
                                   "month"="factor",
                                   "SiteName"="factor",
                                   "ChannelName"="factor",
                                   "Item" = "factor",
                                   "Level1"="factor","Level2"="factor","Level3"="factor","Level4"="factor",
                                   "Level5"="factor"))



my_data2<-read.csv(paste0(directoryname,"/processed_data/NonEdible2.csv"),
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
                                             paste(c("L1logprice"),collapse="+"),
                                             ")"))


level_names<-c("Level1_Name","Level2_Name","Level3_Name")
first_stage_sales_formula<-as.formula(paste0("~(",paste(c("Item","month","SiteName","ChannelName","L1logsales","L1logprice"),collapse="+"),
                                             ")+","(", paste(level_names,collapse="+"),")+(Level1_Name+Level2_Name+Level3_Name)*(",
                                             paste(c("logprice"),collapse="+"),
                                             ")"))




fe_pdata<-model.matrix(first_stage_price_formula,data=my_data )
controls<-as.matrix(fe_pdata)

n_items<-length(grep("Item",colnames(fe_pdata),value=TRUE))
### unpenalize treatment interactions with logprice_lag and logprice_lag_2
penalty.factor.treat=c(0,rep(1/sqrt(n_items),n_items),rep(1,length(colnames(fe_pdata))-n_items-5),rep(0,4))
lambda.treat=2*(log(length(inds_train)))^(3/2)/sqrt(length(inds_train))



t.treat<-remove_wrapper(controls = controls,
                        target =my_data[,treat_name],
                        lambda=lambda.treat,
                        penalty.factor=penalty.factor.treat,
                        inds_train=inds_train)
res.treat<-t.treat$res
treat.fit<-t.treat$fit.c

rownames(t.treat$fit.c$beta)<-sapply(rownames(t.treat$fit.c$beta), make.name)


fe_pdata<-model.matrix(first_stage_sales_formula,data=my_data )
controls<-as.matrix(fe_pdata)
n_items<-length(grep("Item",colnames(fe_pdata),value=TRUE))
penalty.factor.outcome=c(0,rep(1/sqrt(n_items),n_items),rep(1,length(colnames(fe_pdata))-n_items-1))
lambda.outcome=my_cons*(log(length(inds_train)))^(3/2)/sqrt(length(inds_train))


t.out<-remove_wrapper(controls = controls,
                      target =my_data[,outcome_name],
                      lambda=lambda.outcome,
                      penalty.factor=penalty.factor.outcome,
                      inds_train=inds_train)

logprice_inds<-setdiff(grep("logprice",rownames(t.out$fit.c$beta)), grep("L1logprice|L2logprice",rownames(t.out$fit.c$beta)))
res.outcome<-t.out$res +  res.treat*fe_pdata[,logprice_inds]%*%(t.out$fit.c$beta)[logprice_inds]


outcome.fit<-t.out$fit.c

rownames(t.out$fit.c$beta)<-sapply(rownames(t.out$fit.c$beta), make.name)

write.csv(data.frame(treat_res=res.treat,outcome_res=res.outcome),paste0(directoryname,"/processed_data/first_stage/FirstStage",categoryname,".csv"))

price_equation<-data.frame(name=rownames( coef(t.treat$fit.c)),
                           est=as.numeric(coef(t.treat$fit.c)))

price_equation<-price_equation[ price_equation$est!=0,]

sales_equation<-data.frame(name=rownames( coef(t.out$fit.c)),
                           est=as.numeric(coef(t.out$fit.c)))

sales_equation<-sales_equation[ sales_equation$est!=0,]


write.table(print(xtable(price_equation,digits=3), type="latex",include.rownames =FALSE ),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=FALSE)
write.table(as.character(first_stage_price_formula),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)
write.table(paste0("lambda_price=", as.character(round(t.treat$fit.c$lambda,3))),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)
write.table(paste0("sample_size=", as.character(round(t.treat$fit.c$nobs,3))),paste0(directoryname,"/Tables/", categoryname,"/price_eq.txt"),append=TRUE)


write.table(print(xtable(sales_equation,digits=3), type="latex",include.rownames =FALSE ),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=FALSE)
write.table(as.character(first_stage_sales_formula),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)
write.table(paste0("lambda_sales=", as.character(t.out$fit.c$lambda,3)),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)
write.table(paste0("sample_size=", as.character(t.out$fit.c$nobs,3)),paste0(directoryname,"/Tables/", categoryname, "/sales_eq.txt"),append=TRUE)

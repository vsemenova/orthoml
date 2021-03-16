rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
source(paste0(directoryname,"/R/auxiliary.R"))
library(xtable)

categorynames = c("Drinks", "Dairy","Snacks")



categoryname="Drinks"
drinks_levels<-read.csv(paste0(directoryname,"/Tables/","/Figure1_fd/",categoryname,"Level1"))
drinks_fd<-read.csv(paste0(directoryname,"/Tables/","/Figure1/",categoryname,"Level1"))
table_drinks=cbind(drinks_levels[,c("xbreaks","est")], drinks_fd[,"est"])
sd_drinks=cbind(drinks_levels[,c("xbreaks","st.error.hat")], drinks_fd[,"st.error.hat"])
colnames(table_drinks)<-c("Category", "First Differences","Correlated Random Effects")
colnames(sd_drinks)<-c("Category", "First Differences","Correlated Random Effects")

categoryname="Dairy"
dairy_levels<-read.csv(paste0(directoryname,"/Tables/","/Figure1_fd/",categoryname,"Level1"))
dairy_fd<-read.csv(paste0(directoryname,"/Tables/","/Figure1/",categoryname,"Level1"))
table_dairy=cbind(dairy_levels[,c("xbreaks","est")], dairy_fd[,"est"])
sd_dairy=cbind(dairy_levels[,c("xbreaks","st.error.hat")], dairy_fd[,"st.error.hat"])
colnames(table_dairy)<-c("Category", "First Differences","Correlated Random Effects")
colnames(sd_dairy)<-c("Category", "First Differences","Correlated Random Effects")

categoryname="Snacks"
snacks_levels<-read.csv(paste0(directoryname,"/Tables/","/Figure1_fd/",categoryname,"Level1"))
snacks_fd<-read.csv(paste0(directoryname,"/Tables/","/Figure1/",categoryname,"Level1"))
table_snacks=cbind(snacks_levels[,c("xbreaks","est")], snacks_fd[,"est"])
sd_snacks=cbind(snacks_levels[,c("xbreaks","st.error.hat")], snacks_fd[,"st.error.hat"])
colnames(table_snacks)<-c("Category", "First Differences","Correlated Random Effects")
colnames(sd_snacks)<-c("Category", "First Differences","Correlated Random Effects")

table<-rbind(table_drinks,table_dairy,table_snacks)
sd_table<-rbind(sd_drinks,sd_dairy,sd_snacks)


write.table(
  print(xtable(table,     include.colnames=T,
        include.rownames=T,digits=3)),paste0(directoryname,"/Tables/table1_est.txt"))
write.table(
  print(xtable(sd_table,     include.colnames=T,
               include.rownames=T,digits=3)),paste0(directoryname,"/Tables/table1_sd.txt"))
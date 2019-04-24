### This file makes the first pass of on the aggregated data AggData.csv

### It adds 4 lags of logprice/logsales 
###    sets the correct type of key variables
###    splits data into 4 subsets by category (Drinks/Protein/Non_Edibles/Snacks) and saves into 4 distinct files

rm(list=ls())
# set directoryname
directoryname<-"~"
setwd(directoryname)
source(paste0(directoryname,"/Code/Source.R"))
filename<-paste0(directoryname,"/Data/AggData.csv")
# read in data
my_data<-as_tibble(read.csv(filename)) 
# filter out zero prices and sales (log (0)=Inf)
my_data<- filter(my_data,abs(logprice)<Inf && abs(logmove)<Inf)  
# select relevant columns
my_data<-as.data.frame(my_data)
my_data<-my_data[,c("Item", "SalesDate","SiteName","ChannelName","Level1","Level2","Level3","Level4","Level5",
                    "year","month","week","logmove","logprice")]
colnames(my_data)[colnames(my_data)=="logmove"]<-"logsales"
write.csv(my_data,paste0(directoryname,"/Data/","AggDataCleaned.csv"))
### prepare data for each category

# Category Lists
level1_inds_list<-list()
category_list<-list()
outname_list<-list()
# Drinks
level1_inds_list[[1]]<-7
category_list[[1]]<-"Drinks"
# Meat, Fish, Poultry, Dairy
level1_inds_list[[2]]<-c(12,5,19,8)
category_list[[2]]<-"Dairy"
# Non-Edibles
level1_inds_list[[3]]<-c(15,11,4,26)
category_list[[3]]<-"NonEdible"
# Snacks, Sweeteners, Desserts, Spreads
level1_inds_list[[4]]<-c(27,6,28,25)
category_list[[4]]<-"Snacks"


key_database<-data.frame(Numeric=c(0,166,188,
                                   c(5,8,12,19),
                                   c(4,11,15,26),
                                   c(6,25,27,28)),Level1_Name=c("Alcohol","Sodas","Water",
                                                         c("Dairy","Seafood","Meat","Chicken"),
                                                         c("Tableware","Sanitation","Boxes","Stationery"),
                                                         c("Sweets","Snacks","Sugar","Vegeterian")) )
key_database$Numeric<-as.factor(key_database$Numeric)
month_database<-data.frame(month=as.factor(c(1:12)),month_name=month.abb)

for (k in 1:4) {
  ## take level 1 inds corresponding to a category
  level1_inds<<-level1_inds_list[[k]]
  categoryname<-category_list[[k]]
  ## add 4 lags of logsales and logprice
  my_data_subset<- my_data %>%
    filter(Level1 %in% level1_inds)%>%
    group_by(SiteName,ChannelName,Item) %>%
    arrange(SiteName,ChannelName,Item,SalesDate)%>%
    mutate_at(.funs=funs(lag,lag_2,lag_3,lag_4),
              .vars = vars(matches("logsales|logprice"))) %>%
    na.omit(.) %>%
    ungroup() 
  # Set the type of categories to be character and key/date names to be factor
  my_data_subset<-mutate_at(my_data_subset,.funs = funs(as.factor),
              .vars = vars(matches("SiteName|ChannelName|Item|week|month|year"))) %>%
    mutate_at(.funs = funs(as.character),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
    filter(logprice_lag+logprice_lag_2+logprice_lag_3+logprice_lag_4+
             logsales_lag+logsales_lag_2+logsales_lag_3+logsales_lag_4+ logprice  + logsales >-Inf &
             logprice_lag+logprice_lag_2+logprice_lag_3+logprice_lag_4+
             logsales_lag+logsales_lag_2+logsales_lag_3+logsales_lag_4+
             logprice  + logsales < Inf) 
  if (categoryname == "Drinks") {
    my_data_subset<-left_join(my_data_subset,key_database,by=c("Level2"="Numeric"))
  } else {
    my_data_subset<-left_join(my_data_subset,key_database,by=c("Level1"="Numeric"))
  }
  my_data_subset<-left_join(my_data_subset,month_database,by=c("month"="month"))
  write.csv(my_data_subset,paste0(directoryname,"/Data/AggData",categoryname,".csv"))
}


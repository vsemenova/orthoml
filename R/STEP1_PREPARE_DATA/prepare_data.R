### This file makes the first pass of on the aggregated data PEAggData.csv

### It adds 4 lags of logprice/logsales 
##     helper lag functions are in auxiliary.R
###    sets the correct type of key variables
###    splits data into 4 subsets by category (Drinks/Protein/Non_Edibles/Snacks) and saves into 4 distinct files

rm(list=ls())
library(tidyverse)

# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
filename<-paste0(directoryname,"/raw_data/PEAggData.csv")
source(paste0(directoryname,"/R/auxiliary.R"))
# read in master dataset
my_data<-as_tibble(read.csv(filename)) 
# ########### Merge product description with the master dataset #####
#determining the key of the table
find_key<-group_by(my_data,SiteName,ChannelName,Item,year,week)%>%count
# if this is TRUE, the key is SiteName,ChannelName,Item,year,week
all(find_key$n==1)
## otherwise, this is wrong

my_data$ChannelName<-as.character(my_data$ChannelName)
my_data$ChannelName[my_data$ChannelName=="Collection"]<-1
my_data$ChannelName[my_data$ChannelName=="Delivery"]<-2

my_data[,c("Level1","Level2","Level3","Level4","Level5")]<-sapply(my_data[,c("Level1","Level2","Level3","Level4","Level5")],as.factor)
## step 1: merge level number with its name.
category_level1<-read.csv(paste0(directoryname,"/raw_data/Key_CategoryLevel1.csv"),header=FALSE)
colnames(category_level1)<-c("Level1_Name","Level1")
category_level1$Level1<-as.factor(as.character((category_level1$Level1)))
my_data<-left_join(my_data,category_level1,by=c("Level1"="Level1"))
category_level2<-read.csv(paste0(directoryname,"/raw_data/Key_CategoryLevel2.csv"),header=FALSE)
colnames(category_level2)<-c("Level2_Name","Level2")
category_level2$Level2<-as.factor(as.character((category_level2$Level2)))

my_data<-left_join(my_data,category_level2,by=c("Level2"="Level2"))
category_level3<-read.csv(paste0(directoryname,"/raw_data/Key_CategoryLevel3.csv"),header=FALSE)
colnames(category_level3)<-c("Level3_Name","Level3")
category_level3$Level3<-as.factor(as.character((category_level3$Level3)))

my_data<-left_join(my_data,category_level3,by=c("Level3"="Level3"))
category_level4<-read.csv(paste0(directoryname,"/raw_data/Key_CategoryLevel4.csv"),header=FALSE)
colnames(category_level4)<-c("Level4_Name","Level4")
category_level4$Level4<-as.factor(as.character((category_level4$Level4)))

my_data<-left_join(my_data,category_level4,by=c("Level4"="Level4"))
category_level5<-read.csv(paste0(directoryname,"/raw_data/Key_CategoryLevel5.csv"),header=FALSE)
colnames(category_level5)<-c("Level5_Name","Level5")
category_level5$Level5<-as.factor(as.character((category_level5$Level5)))

my_data<-left_join(my_data,category_level5,by=c("Level5"="Level5"))

my_data[,c("Level1_Name","Level2_Name","Level3_Name","Level4_Name","Level5_Name")]<-sapply(my_data[,c("Level1_Name","Level2_Name","Level3_Name","Level4_Name","Level5_Name")],
                                                                                           function(x) toupper(as.character(x)))

my_data$Level2_Name[my_data$Level2_Name=="WATER"]<-"Water"
my_data$Level2_Name[my_data$Level2_Name=="SOFT DRINKS"]<-"Sodas"
my_data$Level2_Name[my_data$Level2_Name=="ALCOHOL"]<-"Alcohol"

my_data$Level1_Name[my_data$Level1_Name=="MEAT"]<-"Meat"
my_data$Level1_Name[my_data$Level1_Name=="DAIRY & EGG"]<-"Dairy"
my_data$Level1_Name[my_data$Level1_Name=="FISH & SEA FOOD"]<-"Fish"
my_data$Level1_Name[my_data$Level1_Name=="POULTRY"]<-"Chicken"


my_data$Level1_Name[my_data$Level1_Name=="PACKAGING"]<-"Boxes"
my_data$Level1_Name[my_data$Level1_Name=="HYGIENE"]<-"Hygiene"
my_data$Level1_Name[my_data$Level1_Name=="CROCKERY"]<-"Crockery"
my_data$Level1_Name[my_data$Level1_Name=="STATIONERY"]<-"Stationery"

my_data$Level1_Name[my_data$Level1_Name=="DESSERT & ICE CREAM"]<-"Sweets"
my_data$Level1_Name[my_data$Level1_Name=="SNACKS"]<-"Snacks"
my_data$Level1_Name[my_data$Level1_Name=="VEGGIE MEALS"]<-"Veggie Meals"
my_data$Level1_Name[my_data$Level1_Name=="SUGAR & SWEETENERS"]<-"Sugar"



my_data$Level1[is.na(my_data$Level1)]<-"-1"
my_data$Level2[is.na(my_data$Level2)]<-"-1"
my_data$Level3[is.na(my_data$Level3)]<-"-1"
my_data$Level4[is.na(my_data$Level4)]<-"-1"
my_data$Level5[is.na(my_data$Level5)]<-"-1"


my_data$Level1[my_data$Level1=="NULL"]<-"-1"
my_data$Level2[my_data$Level2=="NULL"]<-"-1"
my_data$Level3[my_data$Level3=="NULL"]<-"-1"
my_data$Level4[my_data$Level4=="NULL"]<-"-1"
my_data$Level5[my_data$Level5=="NULL"]<-"-1"


my_data$Level1_Name[is.na(my_data$Level1_Name)]<-"NULL"
my_data$Level2_Name[is.na(my_data$Level2_Name)]<-"NULL"
my_data$Level3_Name[is.na(my_data$Level3_Name)]<-"NULL"
my_data$Level4_Name[is.na(my_data$Level4_Name)]<-"NULL"
my_data$Level5_Name[is.na(my_data$Level5_Name)]<-"NULL"



################## Clean merged data ##################

# filter out zero prices and sales (log (0)=Inf)
my_data<- filter(my_data,abs(logprice)<Inf && abs(logmove)<Inf)  
# select relevant columns
my_data<-as.data.frame(my_data)
my_data<-my_data[,c("Item", "SalesDate","SiteName","ChannelName","Level1","Level2","Level3","Level4","Level5",
                    "Level1_Name","Level2_Name","Level3_Name","Level4_Name","Level5_Name",
                    "year","month","week","logmove","logprice")]
colnames(my_data)[colnames(my_data)=="logmove"]<-"logsales"
my_data<-my_data[my_data$logsales !=-Inf & my_data$logprice !=-Inf & my_data$logsales !=Inf & my_data$logsales !=Inf,  ]
write.csv(my_data,paste0(directoryname,"/raw_data/","PEAggDataCleaned.csv"))
### prepare data for each category

my_data<-read.csv(paste0(directoryname,"/raw_data/","PEAggDataCleaned.csv"))
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


for (k in c(2,3,4)) {
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
              .vars = vars(matches(paste(c("Level"),collapse="|"))))
  my_data_subset$month_name<-month.abb[my_data_subset$month]
  #my_data_subset$Level2_Name
  
  if (k==4) {
    write.csv(my_data_subset,paste0(directoryname,"/processed_data/",categoryname,".csv"))
    print(dim(my_data_subset))
  } else {
    write.csv(my_data_subset[1:222691,],paste0(directoryname,"/processed_data/",categoryname,"1.csv"))
     write.csv(my_data_subset[222692:dim(my_data_subset)[1],],paste0(directoryname,"/processed_data/",categoryname,"2.csv"))
    
  }
}

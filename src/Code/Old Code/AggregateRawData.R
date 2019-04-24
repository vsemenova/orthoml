rm(list=ls())

library(readr)
library(stringr)
library(dplyr)
library(plyr)
library(lubridate)
library(tidyverse)
# Set working directory
directoryname<-"/bbkinghome/vsemen/ALICE/"
directoryname<-"/Users/virasemenora/Dropbox (MIT)/MSR/Code/ALICE/"
#directoryname<-"C:/Users/t-viseme/Desktop/ALICE/"
#directoryname<-"C:/Users/mattgold/Dropbox/Code/ALICE/"
setwd(directoryname)
setwd("JJFoods/")
filename<-"PE_annon.csv"
in.name<-paste0(directoryname,'Data/', filename)
RawData <- read_delim(in.name, 
                     delim= ",", col_types = list("i","D","i","i","c","c","c","d","d","d"))
# Drop constant column
RawData <-  RawData %>%
  mutate (CustomerSegment = NULL)
# max num + 1of hierachy levels
max_num<-max(sapply(unique(RawData$ItemHierarchy),str_count, pattern = ','))
real_num<-max_num+1

RawDataH<-str_split_fixed(RawData$ItemHierarchy, ",",real_num)%>%
  as_tibble %>%
  mutate_all(function(x) {
    x<-as.factor(x)
    levels(x)[levels(x)==""]<-"NULL"
    return (x)}) %>%
  mutate(V5 = gsub(",","",V5) ) %>%
  as.matrix()
colnames(RawDataH)<- paste0("Level", as.character(1:real_num))

RawDataH<-dplyr::select(RawData,-ItemHierarchy) %>%
  cbind (., RawDataH[,1:real_num]) 




RawDataH<-RawDataH%>%
  mutate (year = as.factor(year(SalesDate)),
          month = as.factor(month(SalesDate)),
          week = as.factor(week(SalesDate)))
  

#RawData <- readr(file.name)
# Aggregate by (Item,SalesDate,Site,Channel) across Segments
# Drop duplicated rows
AggData<-RawDataH %>%
  group_by(Item,SalesDate,SiteName,ChannelName) %>%
  dplyr::summarise(Units = sum(Units), UnitPrice = mean(UnitPrice)) %>%
  left_join(dplyr::select(RawDataH,-Units, - UnitPrice), by = c("Item","SalesDate",
                                                                          "SiteName",
                                                                "ChannelName")) %>%
  slice(1L)

# Add logmove and logprice 
AggData<- AggData %>%
  mutate ( logmove = log (Units),
           logprice = log(UnitPrice)) %>%
  dplyr::select(-Units,-UnitPrice) %>%
  ungroup()
out.name<-paste0(directoryname,'Data/', "PEAggData.csv")
write.csv(AggData,file=out.name)


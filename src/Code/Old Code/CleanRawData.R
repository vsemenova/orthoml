rm(list=ls())
library(readr)
library(stringr)
library(dplyr)
library(plyr)
library(lubridate)
library(tidyverse)
days_in_week<-7

add_commas<-function (str,pattern=',',max_num) {
  
  ch_num<-str_count(str, pattern)
  chs<-rep(pattern,max_num-ch_num)
  str<-paste0(str,pattern)
  
}
make_inds <- function(df, cols=names(df))
{
  # do each variable separately to get around model.matrix dropping aliased columns
  do.call(cbind, c(df, lapply(cols, function(n) {
    x <- df[[n]]
    mm <- model.matrix(~ x - 1)
    colnames(mm) <- gsub("^x", paste(n, "_", sep=""), colnames(mm))
    mm
  })))
}

real_num<-5
file.name<-"C:/Users/t-viseme/Desktop/TransactionalData.csv"
RawData <- read_delim(file.name, 
                      ";", escape_double = FALSE, trim_ws = TRUE)
# max num of hierachy levels
max_num<-max(sapply(unique(RawData$ItemHierarchy),str_count, pattern = ','))

# split a string with commas into multiple cols for each hierarchy level
RawData$ItemHierarchy<-sapply(RawData$ItemHierarchy,add_commas,max_num=max_num)
# store the hierarchy levels
RawDataH<-str_split_fixed(RawData$ItemHierarchy, ",", max_num+1)%>%
  as_tibble %>%
  mutate_all(function(x) {
    x<-as.factor(x)
    levels(x)[levels(x)==""]<-"NULL"
  return (x)}) %>%
  mutate(V5 = gsub(",","",V5) ) %>%
  as.matrix()
colnames(RawDataH)<- paste0("Level", as.character(1:real_num))

# make sure absent hierarchy level is denoted by NULL, not ""

# Transform Hierarchy into separate levels
# Rename them in "Level1,Level2,.." 
# Replace " " factor level with NULL
RawData<-dplyr::select(RawData,-ItemHierarchy) %>%
  cbind (., RawDataH[,1:real_num]) 
  

colnames(RawData[,8:(7+real_num)])
#levels(RawData$Level2)[levels(RawData$Level2)==""]<-"NULL"
#levels(RawData$Level3)[1]<-"NULL"
#levels(RawData$Level4)[1]<-"NULL"
#levels(RawData$Level5)[1]<-"NULL"
# Process SalesDate, Site
RawData<-RawData%>%
  mutate (year = as.factor(year(SalesDate)),
          month = as.factor(month(SalesDate)),
          week = as.factor(week(SalesDate)+(year(SalesDate)-2012)*53),
          Site = sapply(strsplit(Site,split = " ", fixed = TRUE), function (x) x[[1]][1] ))

# Aggregate by (Item,SalesDate,Site,Channel) across Segments
# Drop duplicated rows
AggData<-RawData %>%
  group_by(Item,SalesDate,Site,Channel) %>%
  dplyr::summarise(Units = sum(Units), UnitPrice = mean(UnitPrice)) %>%
  left_join(dplyr::select(RawData,-Units, - UnitPrice, - Segment), by = c("Item",
                                                                   "SalesDate",
                                                                   "Site",
                                                                   "Channel")) %>%
  slice(1L)

# Add logmove and logprice 
AggData<- AggData %>%
  mutate ( logmove = log (Units),
           logprice = log(UnitPrice)) %>%
  dplyr::select(-Units,-UnitPrice) %>%
  ungroup()



write.csv(AggData,file="C:/Users/t-viseme/Desktop/ALICE/JJFoods/Output/TransactionalAggData.csv")
if(FALSE) {
  # Finally the penny dropped: add brand var
  # brand = Level K, where K is the largest K:LevelK is not NULL
  # this will determine the level at which own and cross-price elasticities are computed - this is wrong
  # Item is the smallest level of hiearachy
  
  level_up<-function(x,k,cols) {
    y<-cols[,k]
    x[is.null(x)]<-y[is.null(x)]
    if (sum(is.null(x))>0) {
      x<-level_up(x,k-1,cols)
    }
    return(x)
  }
  
  cols<-AggData %>%
    dplyr::select(AggData,starts_with("Level")) %>%
    as.matrix()
  AggData<- AggData %>%
    mutate (brand = level_up(Level5,Level))
}
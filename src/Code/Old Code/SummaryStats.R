rm(list=ls())
filename<-"/Users/virasemenora/Dropbox (MIT)/MSR/Code/ALICE/Data/PEAggData.csv"
## This is to fill in Matt Gold's empirical section
my_data<-as_tibble(read.csv(filename))
## Number of unique items
length(unique(my_data$Item))
# 4673
## Total sample size
dim(my_data)[1]
# 1889228
## Number of unique time series

## Start date
min(levels(my_data$SalesDate))
## End date
max(levels(my_data$SalesDate))
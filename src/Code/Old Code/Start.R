rm(list=ls())
# set directoryname
directoryname<-"/Users/virasemenora/Dropbox (MIT)/MSR/Orthogonal ML/ALICE/"
setwd(directoryname)


# Category Lists
level1_inds_list<-list()
outname_list<-list()
# Drinks
level1_inds_list[[1]]<-7
# Meat, Fish, Poultry, Dairy
level1_inds_list[[2]]<-c(12,5,19,8)

# Non Edibles
level1_inds_list[[3]]<-c(15,11,4,26)

# Snacks, Sweeteners, Desserts, Spreads
level1_inds_list[[4]]<-c(27,6,28,25)


# Choose food category
# k=1: Drinks
# k=2: Dairy, Meat,Fish, Poultry
# k=3: Non Edibles
# k=4: Snacks

k<<-2
level1_inds<<-level1_inds_list[[k]]
source("Source.R")


# First Stage
# run_fs = TRUE - estimate first stage
# run_fs = FALSE - upload saved price and sales residuals
res_fs<-fs_jjfoods(run_fs=FALSE)

if (k==1) {
  # Average month elasticity for Soft Drinks and Water
  # Average elasticity for Soft Drinks and Water
  source("RunOwnDrinks.R")
  
  
} else {
  # Average elasticity at Level1 (OLS)
  # Average month elasticity at Level 1 (OLS)
  # Average elasticity at Level2-Level4 (OLS,Lasso,Ridge)
  source("RunOwnFood.R")
  own_food(k)
  
}
# Cross price elasticity matrix
cross_price(k)


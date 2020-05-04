## This code replicates  from the paper "Estimation and Inference about Heterogeneous Treatment Effects in High-Dimensional Dynamic Panels"
## Figure is saved in paste0(directoryname,"/Output/Figures/")


# Average category-level elasticities at Level2/Level1 (for Drinks) and Level 1 (for rest of
# categories)

rm(list=ls())
# set directoryname
directoryname<-"/n/tata/orthoml/"
setwd(directoryname)
source(paste0(directoryname,"/Code/Libraries.R"))
figdirectory<-paste0(directoryname,"/Output/Figures/")
# Average Category Elasticity
# Method to estimate first stage price regression is  gamlr (Taddy, 2011) with penalty parameter chosen by cross-validation
method.treat <<- cv.gamlr
# Method to estimate first stage sales regression is  gamlr (Taddy, 2011) with penalty parameter chosen by cross-validation
method.outcome <<- cv.gamlr
# Second stage method for price elasticities is ordinary least squares
second_stage_method_names<<-c("OLS")

## 
source(paste0(directoryname,"/Code/Utils.R"))
source(paste0(directoryname,"/Code/FirstStage.R"))
source(paste0(directoryname,"/Code/SecondStage.R"))
source(paste0(directoryname,"/Code/Main.R"))
## Arguments
#@ categoryname:  string: name of the category of products to conduct analysis
#@ het.name    :  string:  name of categorical feature whose price elasticities are of interest
#@second_stage_method_names:  names of estimators to use in the second stage. For Figure 3, it always 
                 # Ordinary Least Squares (OLS)
#@run_fs:                    binary feature, run_fs=TRUE: replicate first stage resuls (slow), run_fs=FALSE: load results from disk
# outname         figurename
## Drinks


main(categoryname="Drinks",het.name="Level1_Name",second_stage_method_names=second_stage_method_names,
     run_fs=FALSE,outname="DrinksLevel1")

main(categoryname="Dairy",het.name="Level1_Name", run_fs=FALSE,second_stage_method_names=second_stage_method_names,
     outname="DairyLevel1")

## Non_Edible
main(categoryname="NonEdible",het.name="Level1_Name", run_fs=FALSE,second_stage_method_names=second_stage_method_names,
     outname="NonEdibleLevel1")

## Snacks
main(categoryname="Snacks",het.name="Level1_Name", run_fs=FALSE,second_stage_method_names=second_stage_method_names,
     outname="SnacksLevel1")








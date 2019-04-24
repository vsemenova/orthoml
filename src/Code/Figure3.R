## This code replicates Figure 3 from the paper "Orthogonal Machine Learning for Demand Estimation:
# High-Dimensional Causal Inference in Dynamic Panels"

# Figure 3: average category-level elasticities at Level2/Level1 (for Drinks) and Level 1 (for rest of
# categories)

rm(list=ls())
# set directoryname
directoryname<-"~/orthoml/src"
setwd(directoryname)
source(paste0(directoryname,"/Code/Source.R"))
figdirectory<-paste0(directoryname,"/Output/Figures/")
# Average Category Elasticity
# Method to estimate first stage price regression is  gamlr (Taddy, 2011) with penalty parameter chosen by cross-validation
method.treat <<- cv.gamlr
# Method to estimate first stage sales regression is  gamlr (Taddy, 2011) with penalty parameter chosen by cross-validation
method.outcome <<- cv.gamlr
# Second stage method for price elasticities is ordinary least squares
second_stage_method_names<<-c("OLS")

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








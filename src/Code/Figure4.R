# Figure4 
# Average Month Elasticity for 5 Categories: Protein, Household Items, Snacks, Soft Drinks, Water


rm(list=ls())
# set directoryname
directoryname<-"~/orthoml/src"
setwd(directoryname)
source(paste0(directoryname,"/Code/Source.R"))
figdirectory<-paste0(directoryname,"/Output/Figures/")

## set heterogeneity pattern to indicate month
het.name="month_name"
method.treat = cv.gamlr
method.outcome = cv.gamlr
second_stage_method_names<<-c("OLS")
## Drinks

main(categoryname="Drinks",method.treat=method.treat,
     method.outcome=method.outcome,
     run_fs=FALSE,het.name=het.name,second_stage_method_names=second_stage_method_names,
     outname="Drinksmonth")



## Protein
main(categoryname="Dairy",method.treat=method.treat,
     method.outcome=method.outcome,
     run_fs=FALSE,het.name=het.name,second_stage_method_names=second_stage_method_names,
     outname="Dairymonth")

## Non_Edible
main(categoryname="NonEdible",method.treat=method.treat,
     method.outcome=method.outcome,
     run_fs=FALSE,het.name=het.name,second_stage_method_names=second_stage_method_names,
     outname="NonEdiblemonth")

## Snacks
main(categoryname="Snacks",method.treat=method.treat,
     method.outcome=method.outcome,
     run_fs=FALSE,het.name=het.name,second_stage_method_names=second_stage_method_names,
     outname="Snacksmonth")

## Soft Drinks
main(categoryname="Drinks",method.treat=method.treat,run_fs=FALSE,het.name=het.name,
     method.outcome=method.outcome,second_stage_method_names=second_stage_method_names,subset.name = "Sodas", outname="Drinksmonth")

## Water
main(categoryname="Drinks",method.treat=method.treat,run_fs=FALSE,het.name=het.name,
     method.outcome=method.outcome,second_stage_method_names=second_stage_method_names,subset.name = "Water", outname="Drinksmonth")

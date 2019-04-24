## This code replicates Figure 5 from the paper "Orthogonal Machine Learning for Demand Estimation:
# High-Dimensional Causal Inference in Dynamic Panels"

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
second_stage_method_names<<-c("OLS","Lasso","Ridge")
# Figure 3: average category-level elasticities at Level2/Level1 (for Drinks) and Level 1 (for rest of
# categories)

for (categoryname in c("Dairy","NonEdible","Snacks")) {
  ## Average Categorywise Own Elasticity at Level 2
  grouping_level<-"Level1_Name"
  het.name<-c("Level1","Level2")
  main(categoryname=categoryname,het.name=het.name, run_fs=FALSE,
       grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
       lambda_ridge=0.9,
       outname=paste0( "Level2",categoryname))
  
  # Average Category Elasticity at Level3
  grouping_level<-"Level1_Name"
  het.name<-c("Level1","Level2","Level3")
  main(categoryname=categoryname,het.name=het.name, run_fs=FALSE,
       grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
       lambda_ridge=0.9,
       outname=paste0( "Level3",categoryname))
  
  # Average Category Elasticity at Level 4
  if (categoryname !="NonEdible") {
    grouping_level<-"Level1_Name"
    het.name<-c("Level1","Level2","Level3","Level4")
    main(categoryname=categoryname,het.name=het.name, run_fs=FALSE,
         grouping_level=grouping_level,second_stage_method_names=second_stage_method_names,
         lambda_ridge=0.9,
         outname=paste0( "Level4",categoryname))
  }
}

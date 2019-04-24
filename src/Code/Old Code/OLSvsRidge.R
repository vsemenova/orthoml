# Compare OLS vs Ridge
source("CompareOLSRidge.R")
het<-"^(?=.*Level2|Level1)(?!.*lag)"
toplevel<-"Level1"
bottomlevel<-"Level2"
outname<-paste0("Level2",outname_list[[k]])
# Construct treatment matrix based on price residuals
het_treat_res<-construct_treat(het=het,price_res=res_fs$treat_res,
                               fe_data=res_fs$fe_pdata,perl=TRUE,tol = 0.01,subset=subset)




# Run second stage : outcome residuals on treatments
# Use OLS, Lasso, and Ridge
res_ss<-second_stage(het_treat=het_treat_res$het_treat,
                     by_het=het_treat_res$by_het,
                     outcome=res_fs$outcome_res,toplevel=toplevel)
# Back out own elasticities from average elasticities and added heterogeneity
res_ss_b<-recover_bottom_est(het_treat=het_treat_res$het_treat,
                             by_het=het_treat_res$by_het,het=het,htheta=res_ss$OLS,res_ss=res_ss)

res_ss_b<-res_ss_b[order(res_ss_b$OLS.st.error.hat,decreasing=TRUE),]
# Compare OLS vs Ridge for Meat (k==2)
# xlabels<-c("Milk Powder","Cream","Ghee","Poultry","Meat Specialties","Frozen Raw Seafood","Canned Fish",
#            "Turkey","Dairy","Butter")
 xlables<-NULL
  compare_ols_ridge(data=res_ss_b[1:length(xlabels),],xlabels=xlabels,outname="Level2Dairy")
  
  het<-"^(?=.*Level4|Level3|Level2|Level1)(?!.*lag)"
  toplevel<-"Level1"
  bottomlevel<-"Level4|Level3|Level2"
  outname<-paste0("Level4",outname_list[[k]])
  subset<
  # Construct treatment matrix based on price residuals
  het_treat_res<-construct_treat(het=het,price_res=res_fs$treat_res,
                                 fe_data=res_fs$fe_pdata,perl=TRUE,tol = 0.01,subset=subset)
  
  # Run second stage : outcome residuals on treatments
  # Use OLS, Lasso, and Ridge
  res_ss<-second_stage(het_treat=het_treat_res$het_treat,
                       by_het=het_treat_res$by_het,
                       outcome=res_fs$outcome_res,toplevel=toplevel,mu_bar=0.5)
  # Back out own elasticities from average elasticities and added heterogeneity
  res_ss_b<-recover_bottom_est(het_treat=het_treat_res$het_treat,
                               by_het=het_treat_res$by_het,het=het,htheta=res_ss$OLS,res_ss=res_ss)
  
  res_ss_b<-res_ss_b[order(res_ss_b$OLS.st.error.hat,decreasing=TRUE),]
  xlables<-NULL
 # xlabels<-c("Pork,Other","Chicken,Other","Milk Powder, Other","Frozen Fish","Pizza Cheese",
 #            "Seafood Mix","Frozen Beef","Canned Cod","Cream, Other","Scallops","Mussels","Frozen Coated Seafood","Halloumi Cheese")
 # xkeys<-c(137,28,105,69,129,213,100,28,46,210,161,117)
 # 17673,55244,262,10083,8603,196,155,193,4376,235,256,1333
  compare_ols_ridge(data=res_ss_b[3:length(xlabels),],xlabels=xlabels[3:length(xlabels)],outname="Level4Dairy")
  



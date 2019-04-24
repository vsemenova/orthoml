# Compute Cross-Price Elasticity for Drinks
# Matrix CP  is N x N big, where N is the number of leaf nodes in the hierarchy
# Hierachy is spanned by Level1-Level5 (or less,if certain leaves are
# coarsed in cleaning stage)


# A preliminary object for CP matrix is cross-price regression
# It contains  cross-price variables, corresponding to K groups, and own price vars
# Group 1 is all observations, cp_1 measures average cross-price elasticity
# Group 2 is all observations in a top hierarchy node, cp_2 measures
# added heterogeneity in cross-price effect for group 2
# Group 2-Group K are assumed to comprise TOP (first) level of the hirarchy

# from K regression coefs to CP matrix see "CPElasticities.pdf"

cross_price<-function(k,model_name="OwnCross") {
  # Select product category
  level1_inds<<-level1_inds_list[[k]]
  # Source libraries and functions
  source("Source.R")
  # Run first stage
  res_fs<-fs_jjfoods(run_fs=FALSE)
  # CP model uses only top hierarchy level
  if (level1_inds[1] ==7) {
    # top level for Drinks
    het<-"^(?=.*Level2|Level1)(?!.*lag)"
  } else {
    # top level for everything else
    het<-"^(?=.*Level1)(?!.*lag)"
  }
  
  het_treat_res<-construct_treat(het=het,price_res=res_fs$treat_res,
                                 fe_data=res_fs$fe_pdata,cp=TRUE,res_fs=res_fs)
  # Cross Price Regression
  res_ss<-second_stage(het_treat=het_treat_res$het_treat,
                       by_het=het_treat_res$by_het,outcome=res_fs$outcome_res)
  out<-as.matrix(cbind(est=as.numeric(res_ss$OLS), st.error.hat=attr(res_ss$OLS,"st.error.hat") ))
  tab<-xtable(out, caption = "Average Own Elasticity", label = paste0(het,level1_inds[1]),
              digits=3)
  
  
  print(tab,file=outtexname,append=TRUE, 
        include.rownames=TRUE,
        include.colnames=TRUE,digits=3)
  # Own Elasticities from Cross Price Regression (model_name ="OwnCP")
  res_ss_b<-recover_bottom_est(het_treat=het_treat_res$het_treat,
                               by_het=het_treat_res$by_het,het=het,htheta=res_ss$OLS,res_ss=res_ss,
                               model_name=model_name)
 
  # Own elasticities after accounting for cross-price effects

    outname<-paste0("OwnCPLevel1",outname_list[[k]])
    # do box and whisker plot only of Est and CI's 
    boxwhisker(data=res_ss_b,het=het,
               xbreaks = xbreaks[[k]],outname=outname,subset.name="")
  
  # Construct products
  het<-"^(?=.*Level4|Level3|Level2|Level1)(?!.*lag)"
  het_treat_res<-construct_treat(het=het,price_res=res_fs$treat_res,
                                 fe_data=res_fs$fe_pdata,cp=FALSE,res_fs=res_fs)
  
   # Define products and compute their market shares
  cp<-get_cross_price_matrix(het_treat=het_treat_res$het_treat,
                             by_het=het_treat_res$by_het,
                             htheta=res_ss$OLS,
                             my_data=res_fs$my_data,
                             price_res=res_fs$treat_res,
                             res_ss_b = res_ss_b,
                             digs=3)
  
  # Write down cp matrix 
  product_names<-product_list[[k]]
  
  rownames(cp$cp_matrix)<-product_names
  colnames(cp$cp_matrix)<-product_names
  rownames(cp$cp_matrix_sd)<-product_names
  colnames(cp$cp_matrix_sd)<-product_names
  write.csv(file=paste0(textdirectory,"CrossPriceEst", outname_list[[k]], ".csv"),cp$cp_matrix)
  write.csv(file=paste0(textdirectory,"CrossPriceSD", outname_list[[k]], ".csv"),cp$cp_matrix_sd)
}



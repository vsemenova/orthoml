main<-function(res_fs,het,outname=NULL,xbreaks=NULL,subset=NULL,toplevel=NULL,bottomlevel=NULL,legend_breaks=NULL,subset.name=NULL,
               ...) {
  
  # Construct treatment matrix based on price residuals
  het_treat_res<-construct_treat(het=het,price_res=res_fs$treat_res,
                                 fe_data=res_fs$fe_pdata,perl=TRUE,tol = 0.01,subset=subset)

  # Run second stage : outcome residuals on treatments
  # Use OLS, Lasso, and Ridge
  res_ss<-second_stage(het_treat=het_treat_res$het_treat,
                       by_het=het_treat_res$by_het,
                       outcome=res_fs$outcome_res,toplevel=toplevel,subset=subset,...)
  # Back out own elasticities from average elasticities and added heterogeneity
  res_ss_b<-recover_bottom_est(het_treat=het_treat_res$het_treat,
                               by_het=het_treat_res$by_het,het=het,htheta=res_ss$OLS,res_ss=res_ss)

  # If no grouping, 
  if (is.null(toplevel)) {
    # do box and whisker plot only of Est and CI's 
    boxwhisker(data=res_ss_b,het=het,
               xbreaks = xbreaks,outname=outname,subset.name=subset.name,...)
  } else {
    # Otherwise, histogram of coefs grouped by Level1 (Food) or Level2 (Drinks)
    if (level1_inds[1]!=7) {
      missing_cat<-
        setdiff(level1_inds,as.numeric(gsub("Level1","",grep("Level1",colnames(het_treat_res$by_het),value=TRUE))))
      
    } else {
      missing_cat<-NULL
    }
    
    hist_coef(data=res_ss_b[,grep("est",colnames(res_ss_b),perl=TRUE)],
              bottomlevel=bottomlevel,
              toplevel=toplevel,fe_data=res_fs$fe_pdata,
              legend_breaks=legend_breaks,outname=outname,missing_cat=missing_cat,...
    )
  }
 
  return(res_ss_b)
}


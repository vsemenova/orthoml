## input arguments

#@ categoryname  product category: Drinks, Protein, Non_Edibles, Snacks

#@ method.treat   method for partialling out controls from treatment
#@ method.outcome method for partialling out controls from outcome
#@ outpath        pathname to save treatment/outcome residuals
#@ num_splits     number of splits to train model (minimum 2)
#... any optional arguments for the method
# read in data
# 

main<-function(categoryname,
               grouping_level=NULL,
               outname,...) {
  

  
  if (is.null(subset_inds)) {
    subset_inds<-1:dim(my_data)[1]
  }
  ss<-second_stage(categoryname=categoryname,...)
  
  ## need to collapse the list with 3 estimators into (no standard errors) into a dataframe
    ss_estimates<-left_join(data.frame( Lasso=sapply(ss$Lasso$est,round,3),
                                        DebiasedLasso=sapply(ss$DebiasedLasso$est,round,3),
                                        OLS = sapply(ss$OLS$est,round,3),
                                        RowID=ss$OLS$RowID), select(my_data,one_of("RowID",grouping_level)),
                            by = c("RowID"="RowID")) %>%
      select(.,-RowID)
    # plot histogram of estimates for each second stage estimator
    # group points by colour determined by grouping_level
    hist_coef(data=ss_estimates,sample_size=dim(my_data)[1],
              grouping_level=grouping_level,
              categoryname= categoryname,outname=outname,...
    )
    #ss_estimates<-apply(ss_estimates,2,round,3)
    
   write.csv(ss_estimates,paste0(tabledirectory,outname))
}

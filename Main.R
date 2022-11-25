main<-function(categoryname,
               grouping_level=NULL,all_levels,table=TRUE,xlim,...) {
  
  
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
            categoryname= categoryname,xlimh=xlim,...
  )
  #ss_estimates<-apply(ss_estimates,2,round,3)
  
  return(ss_estimates)
}

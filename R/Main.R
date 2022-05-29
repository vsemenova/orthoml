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
  if (table) {
  ss_estimates<-left_join(data.frame( Lasso=sapply(ss$Lasso$est,round,3),
                                      DebiasedLasso=sapply(ss$DebiasedLasso$est,round,3),
                                      OLS = sapply(ss$OLS$est,round,3),
                                      GLasso = sapply(ss$GLasso$est,round,3),
                                      DebiasedGLasso = sapply(ss$DebiasedGLasso$est,round,3),
                                      RowID=ss$OLS$RowID), select(my_data,one_of("RowID",grouping_level,all_levels,c("Level1","Level2")
                                      )),
                          by = c("RowID"="RowID")) %>%
    arrange(.,Level1,Level2) %>%
    select(.,-RowID,-Level1,-Level2)
  ss_st_error<-data.frame( 
    DebiasedLasso=sapply(ss$DebiasedLasso$st.error.hat,round,3),
    OLS = sapply(ss$OLS$st.error.hat,round,3),
    DebiasedGLasso = sapply(ss$DebiasedGLasso$st.error.hat,round,3),
    RowID=ss$OLS$RowID)
  ss_estimates<-cbind(ss_estimates[,c(6:dim(ss_estimates)[2],1:5)],ss_st_error[,1:3])
  n_unique_Lasso<-length(unique(ss_estimates$Lasso))
  n_unique_DebiasedLasso<-length(unique(ss_estimates$DebiasedLasso))
  n_unique_OLS<-length(unique(ss_estimates$OLS))
  n_unique_RowID<-length(unique(ss$OLS$RowID))
  
  ss_estimates<-rbind(ss_estimates,c(n_unique_Lasso,n_unique_DebiasedLasso,n_unique_OLS,n_unique_RowID,rep(0,dim( ss_estimates)[2]-4)  ))
  colnames(ss_estimates)[(dim(ss_estimates)[2]-7):dim(ss_estimates)[2] ]<-c("Lasso","DebiasedLasso","OLS","GLasso","DebiasedGLasso",
                                                                            "DebiasedLasso_ss","OLS_ss","DebiasedGLasso_ss")
  
  }
  return(ss_estimates)
}

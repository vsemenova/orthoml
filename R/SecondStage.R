
second_stage<-function(my_data,
                        het.name,
                        categoryname,
                        second_stage_method_names,
                        ...) {
  ## identify categories that create multicollinearity
  names_to_exclude<-drop_multicollinear_categories(categoryname )
  #names_to_exclude<-c()
  ## cretaed hard-coded 0/1 representation of categories
  hard_coded_categories<-select(my_data,het.name) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
    model.matrix(as.formula(paste0("~",het.name,collapse="+")),.)
  ## double check absense of multicollinearity
  
  hard_coded_categories<-hard_coded_categories[,setdiff(colnames(hard_coded_categories), names_to_exclude)]
  hard_coded_categories_withrowid<-select(my_data,het.name,RowID) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(matches(paste(c("Level"),collapse="|"))))
  # select rows corresponding to distinct products/categories as determined by het.name
  # select rows corresponding to distinct products/categories as determined by het.name
  inds<-! duplicated(hard_coded_categories)
  hard_coded_unique_categories<-hard_coded_categories[inds,] 
  hard_coded_unique_categories_with_rowid<-hard_coded_categories_withrowid[ inds,]
  hard_coded_unique_categories_with_rowid<-as.data.frame(hard_coded_unique_categories_with_rowid)
  #hard_coded_unique_categories_with_rowid$RowID<-as.character(hard_coded_unique_categories_with_rowid$RowID)
  ## interact price_res with hard-coded columns correspondng to het.name 
  treat_res<-my_data$treat_res
  outcome_res<-my_data$outcome_res
  het_treat<-matrix(rep(treat_res,dim(hard_coded_categories)[2]),ncol= dim(hard_coded_categories)[2])*hard_coded_categories
  
  
  ## run second stage regression
  res<-list()
  htheta<-list()
  vcov<-list()
  for (method_name in second_stage_method_names) {
    # get second stage estimator
    #method<-get(method_name)
    # execute estimator 
    
    if (method_name == "OLS") {
      result<-OLS(x=het_treat,y=outcome_res,...)
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator),
                                     st.error.hat =as.numeric(sqrt(diag(hard_coded_unique_categories%*%result$vcov%*%t(hard_coded_unique_categories))))) 
      res[[method_name]]$RowID<-hard_coded_unique_categories_with_rowid[,"RowID"]
      
    }
    if (method_name == "Lasso") {
      result<-Lasso(x=het_treat,y=outcome_res,...)
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator) )
      htheta[[method_name]]<-result$estimator
    }
    if (method_name == "DebiasedLasso") {
      result<-DebiasedLasso(x=het_treat,y=outcome_res,htheta = htheta[["Lasso"]],...)
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator),
                                     st.error.hat =as.numeric(sqrt(diag(hard_coded_unique_categories%*%result$vcov%*%t(hard_coded_unique_categories))))) 
      
    }
    ### extended model
    phet<-dim(het_treat)[2]
    covs<-cbind(hard_coded_categories,het_treat)
    
    if (method_name =="GLasso") {
      lambda = lambdamax(x=covs, y = outcome_res, index = c(NA,2:phet,NA,2:phet), penscale = sqrt,
                         model = LinReg())/100
      fit<-grplasso(x=covs,y=outcome_res,index=c(NA,2:phet,NA,2:phet),standardize=TRUE,model=LinReg(),   penscale = sqrt,lambda=lambda)
      result<-list()
      result$estimator<-fit$coefficients
      
      
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator[(phet+1):(2*phet)]) )
      htheta[[method_name]]<-result$estimator
    }
    
    if (method_name == "DebiasedGLasso") {
      result<-DebiasedLasso(x=covs,y=outcome_res,htheta=htheta[["GLasso"]],...)
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator[(phet+1):(2*phet)]),
                                     st.error.hat =as.numeric(sqrt(diag(hard_coded_unique_categories%*%result$vcov[(phet+1):(2*phet),(phet+1):(2*phet)]%*%t(hard_coded_unique_categories))))) 
      
    }
    
    
    
    
  }
  
  return(res)
}
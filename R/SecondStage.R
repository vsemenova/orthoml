
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
    method<-get(method_name)
    # execute estimator 
    result<-method(x=het_treat,y=outcome_res,htheta=htheta[["Lasso"]],...)
    if (method_name=="Lasso") {
      htheta[["Lasso"]]<-result$estimator
    }
    if (method_name %in% c("OLS","DebiasedLasso")) {
      # add variance estimator for OLS and Ridge
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator),
                                     st.error.hat =as.numeric(sqrt(diag(hard_coded_unique_categories%*%result$vcov%*%t(hard_coded_unique_categories))))) 
      
    } else {
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator) )
      
    }
    # assign RowID column to each estimate
    res[[method_name]]$RowID<-hard_coded_unique_categories_with_rowid[,"RowID"]
    
    
    
    
  }
 
  return(res)
}
second_stage<-function(my_data,fs,
                       het.name,
                       categoryname,
                       second_stage_method_names,
                       ...) {
  ## identify categories that create multicollinearity
  names_to_exclude<-drop_multicollinear_categories(categoryname )
  ## cretaed hard-coded 0/1 representation of categories
  hard_coded_categories<-select(my_data,het.name) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
    model.matrix(as.formula(paste0("~",het.name,collapse="+")),.)
  ## double check absense of multicollinearity
  
  hard_coded_categories<-hard_coded_categories[,setdiff(colnames(hard_coded_categories), names_to_exclude)]
  
  ## interact price_res with hard-coded columns correspondng to het.name 
  treat_res<-fs$treat
  outcome_res<-fs$outcome
  het_treat<-matrix(rep(treat_res,dim(hard_coded_categories)[2]),ncol= dim(hard_coded_categories)[2])*hard_coded_categories
  # select rows corresponding to distinct products/categories as determined by het.name
  hard_coded_unique_categories<-hard_coded_categories[! duplicated(hard_coded_categories),] 
  ## run second stage regression
  res<-list()
  for (method_name in second_stage_method_names) {
    # get second stage estimator
    method<-get(method_name)
    # execute estimator 
    result<-method(x=het_treat,y=outcome_res,htheta = res[["Lasso"]]$est,...)
    if (method_name %in% c("OLS","Ridge")) {
      # add variance estimator for OLS and Ridge
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator),
                                     st.error.hat =as.numeric(sqrt(diag(hard_coded_unique_categories%*%result$vcov%*%t(hard_coded_unique_categories))))) 
      
    } else {
      res[[method_name]]<-data.frame(est =as.numeric(hard_coded_unique_categories%*%result$estimator) )
      
    }
    # assign RowID column to each estimate
    res[[method_name]]$RowID<-rownames(hard_coded_unique_categories)
    
  }
  return(res)
}

################## ESTIMATORS ##########################

###  Orthogonal Least Squares ###
OLS<-function(x,y,...) {
  S<-tryCatch(solve(t(x)%*%x))
  if (class(S[1] ) == "character") {
    stop("Singular matrix")
  } else {
    b.hat<-S%*%(t(x)%*%y)
  }
  
  vcov<-white_cov(x=x,y=y,b=b.hat)
  
  return(res=list(estimator=b.hat,
                  vcov = vcov))
}
### White Standard errors for OLS
white_cov<-function(x,y,b,M,...) {
  x<-as.matrix(x)
  n<-length(y)
  p<-dim(x)[2]
  x<-as.matrix(x)
  V<-matrix(0,p,n)
  y.hat<-x%*%b
  for (i in 1:n) {
    V[,i]<-(y[i] - y.hat[i])*x[i,]
  }
  if (missing(M)) {
    M<-1/n*(t(x)%*%x)
  }

  VV<-(V%*%t(V))/(n-p)
  VAR<-(solve(M)%*%(VV)%*% solve(M))/n
  
  return(VAR)
}
### Orthogonal Lasso ###
Lasso<-function(x,y,categoryname,...){
  # default choice is the top level of the hierarchy
  names_free_from_penalty<-c("(Intercept)",grep("Level1",colnames(x),value=TRUE))
 
  fit<-cv.gamlr(x,y,  standardize=FALSE,      intercept=TRUE,     lmr=1e-7,     free = names_free_from_penalty)
  htheta<-coef(fit,s="min")[-1]
  names(htheta)<-colnames(x)
  return(list(estimator=htheta))
}

### Double Orthogonal Ridge
Ridge<-function(x,y,lambda_ridge,htheta,...){
  p<-dim(x)[2]
  ## ridge regularized inverse
  M<-solve (t(x)%*% x +lambda_ridge*diag(p)) 
  ## Double Orthogonal Ridge estimator
  
  b.hat<-M%*% t(x)%*%( y - x%*%htheta) + htheta
  ## White-type standard error
  vcov<-white_cov(x=x,y=y,b=b.hat,M=M)
  return(list(estimator = b.hat,
             vcov=vcov ))
}
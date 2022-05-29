generate_second_stage<-function(seed,N,epsilon.own,TT=1,sigma.p=0.1, sigma.u=0.03,rho=0.9,...) {
  
  ## generate residuals
  set.seed(seed)
  P.tilde<-matrix(sigma.p*rnorm(N*TT),ncol=1)
  U<-matrix(sigma.u*rnorm(N*TT),ncol=1)
  
  p_het = length(epsilon.own)
  
  set.seed(seed)
  std_controls<-matrix(rnorm(N*TT*(p_het-1)),ncol=p_het-1)
  #std_controls<-matrix(rnorm(N*(p_het-1)),ncol=p_het-1)
  cov_matrix<-toeplitz(rho^(c(0:(p_het-2))))
  het_controls<-std_controls%*%chol(cov_matrix)
  het_controls<-cbind(rep(1,N*TT),het_controls)
  
  
  
  ### generate outcome residual
  Q.tilde<-P.tilde*(het_controls%*%epsilon.own)+U
  
  return(data.frame(
    het_controls=het_controls,
    P.tilde=as.numeric(P.tilde),
    U=as.numeric(U),
    Q.tilde=as.numeric(Q.tilde)
  ))
  
}

generate_first_stage<-function(epsilon.own,TT,N,p,alpha_price, alpha_P, alpha_E,delta_P,delta_E,delta_K,...) {
  
  ### begin with generate second stage 
  
  
  data<-generate_second_stage(epsilon.own=epsilon.own,TT=TT,N=N,...)
  
  P=P.tilde=data$P.tilde
  Q.tilde=data$Q.tilde
  Q=U=data$U
  het_controls1=data[,1:length(epsilon.own)]
  
  P<-matrix(P, ncol=TT)
  P.tilde<-matrix(P.tilde,ncol=TT)
  Q<-matrix(Q,ncol=TT)
  U<-matrix(U,ncol=TT)
  p_het = length(epsilon.own)
  
  
  
  ## generate first-stage time-variant strictly exogenous controls
  
  
  std_controls<-matrix(rnorm(N*TT*p),ncol=p)
  cov_matrix<-toeplitz(rho^(c(0:(p-1))))
  controls<-std_controls%*%chol(cov_matrix)
  
  het_controls<-array(0,c(N,p_het,TT))
  for (j in 1:p_het) {
    het_controls[,j,]<-matrix(data[,j],ncol=TT)
  }
  fsstage_controls<-array(0,c(N,p,TT)) 
  for (j in 1:p) {
    fsstage_controls[,j,]<-matrix(controls[,j],ncol=TT)
  }
  
  for (t in 3:TT) {
    
    P[,t]<-alpha_price[1]*P[,t-1]+alpha_price[2]*P[,t-2]+fsstage_controls[,,t]%*%delta_P + het_controls[,,t]%*%delta_K+alpha_P+P.tilde[,t]
    Q[,t]<-P[,t]*(het_controls[,,t]%*%epsilon.own) +fsstage_controls[,,t]%*%delta_E+ alpha_E+ U[,t]
  }
  
  ##
  
  P1<-P0<-fsstage_controls[,,1]%*%delta_P 
  Q1<-Q0<-fsstage_controls[,,1]%*%delta_E
  
  ## create lags of order 1
  Q_lag<-cbind(Q0,Q[,1:(TT-1)])
  P_lag<-cbind(P0,P[,1:(TT-1)])
  
  ## create lags of order 2
  Q_lag2<-cbind(Q0,Q0,Q[,1:(TT-2)])
  P_lag2<-cbind(P0,P0,P[,1:(TT-2)])
  
  
  data=list(Q=as.numeric(Q),
            P=as.numeric(P),
            Q.tilde=as.numeric(Q.tilde),
            P.tilde=as.numeric(P.tilde),
            Q_lag=as.numeric(Q_lag),
            Q_lag2=as.numeric(Q_lag2),
            P_lag=as.numeric(P_lag),
            P_lag2=as.numeric(P_lag2),
            het_controls=het_controls1,
            controls=controls)
  
  
  
  return(data)
}


second_stage<-function(fs,
                       second_stage_method_names,
                       controls,
                       target_controls,
                       nonzero_coords,
                       ...) {
  treat_res<-fs$treat
  outcome_res<-fs$outcome
  #controls<-as.matrix(mydata[,grep("controls",colnames(mydata),value=TRUE)])
  het_treat<-matrix(rep(treat_res,dim(controls)[2]),ncol= dim(controls)[2])*controls
  
  htheta<-list()
  res<-data.frame(RowID=c(1:dim(target_controls)[1]))
  est<-data.frame(RowID=c(1:dim(controls)[2]))
  for (method_name in second_stage_method_names) {
    
    # get second stage estimator
    
    if (method_name == "DebiasedGLasso") {
      result<-DebiasedLasso(x=het_treat,y=outcome_res,htheta=htheta[["GLasso"]],...)
    } else {
      method<-get(method_name)
    }
    
    
    if (method_name =="GLasso") {
      
      phet<-dim(controls)[2]
      covs<-cbind(het_treat,controls)
      
      
      lambda = lambdamax(x=covs, y = outcome_res, index = c(NA,2:phet,NA,2:phet), penscale = sqrt,
                         model = LinReg())/100
      fit<-grplasso(x=covs,y=outcome_res,index=c(NA,2:phet,NA,2:phet),standardize=TRUE,model=LinReg(),   penscale = sqrt,lambda=lambda)
      result<-list()
      result$estimator<-fit$coefficients[1:p_het]
      htheta[["GLasso"]]<-result$estimator
      
    }
    
    
    
    if (method_name =="DebiasedLasso") {
      # execute estimator 
      result<-method(x=het_treat,y=outcome_res,htheta=htheta[["Lasso"]],...)
      
    } 
    
    if (method_name=="Lasso") {
      result<-method(x=het_treat,y=outcome_res,...)
      htheta[["Lasso"]]<-result$estimator
    }
    
    if (method_name == "OLS") {
      
      result<-method(x=het_treat,y=outcome_res,...)
      
      
    }
    
    if (method_name == "Oracle") {
      fit<-method(x=het_treat[,nonzero_coords],y=outcome_res,...)
      result<-list()
      result$estimator<-true_epsilon.own
      result$estimator[nonzero_coords]<-fit$estimator
      result$estimator[setdiff(c(1:p_het),nonzero_coords)]<-0
    }
    
    if (method_name == "True") {
      
      result<-list()
      result$estimator<-true_epsilon.own
    }
    
    # execute estimator 
    
    
    
    
    
    
    
    
    
    # add variance estimator for OLS and Ridge
    res[,method_name]<-as.numeric(target_controls%*%result$estimator)
    est[,method_name]<-as.numeric(result$estimator)
    #res[[method_name]]$RowID<-hard_coded_unique_categories_with_rowid[,"RowID"]
    
  }
  
  
  ss_estimates<-as.data.frame(res)
  estimates<-as.data.frame(est)
  return(list(
    partial_effects=ss_estimates,
    estimates=estimates))
  
  
}



##### helper functions ####



Oracle<-function(x,y,nonzero_coords,...){
  ols.res<-OLS(x[,nonzero_coords],y,...)
  b.hat<-rep(0,dim(x)[2])
  vcov<-matrix(0,dim(x)[2],dim(x)[2])
  b.hat[nonzero_coords]<-ols.res$estimator
  vcov[nonzero_coords,nonzero_coords]<-ols.res$vcov
  
  return(res=list(estimator=b.hat,
                  vcov = vcov))
}
True<-function(true_parameter,...) {
  return(res=list(estimator=true_parameter,
                  vcov = matrix(0,length(true_parameter),length(true_parameter))))
}


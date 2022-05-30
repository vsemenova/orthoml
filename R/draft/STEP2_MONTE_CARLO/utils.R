generate_residuals<-function(seed,N,TT,sigma.p=0.02, sigma.u=0.01,...) {
  
  set.seed(seed)
  P.tilde<-matrix(sigma.p*rnorm(N*TT),ncol=TT)
  U<-matrix(sigma.u*rnorm(N*TT),ncol=TT)
  # Q.tilde<-P.tilde*matrix(rep(het_characteristics%*%epsilon.own, TT),ncol=TT)+U
  
  P.tilde_first_diff=matrix(P.tilde,ncol=TT)
  U_first_diff=matrix(U,ncol=TT)
  
  for (t in 2:TT) {
    ### logprice first difference 
    
    P.tilde_first_diff[,t]<-P.tilde[,t]-P.tilde[,t-1]
    U_first_diff[,t]<-U[,t]-U[,t-1]
  }
  
  return(data.frame(
    P.tilde=as.numeric(P.tilde),
    U=as.numeric(U),
    P.tilde_first_diff=as.numeric(P.tilde_first_diff),
    U_first_diff=as.numeric(U_first_diff)
  ))
}


generate_price_sales<-function(seed,N,TT,mydf,fs,het_beta0,price_fixed_effect=0,sales_fixed_effect=0,...) {
  set.seed(seed)
  residuals<-generate_residuals(seed=seed,TT=TT,N=N,...)
  P.tilde=matrix(residuals$P.tilde,ncol=TT)
  U=matrix(residuals$U,ncol=TT)
  
  P.tilde_first_diff=matrix(residuals$P.tilde_first_diff,ncol=TT)
  U_first_diff=matrix(residuals$U_first_diff,ncol=TT)
  
  logpr<-matrix(0,N,TT)
  logsls<-matrix(0,N,TT)
  logprice<-matrix(0,N,TT)
  logsales<-matrix(0,N,TT)
  
  logprice_lag<-matrix(0,N,TT)
  logsales_lag<-matrix(0,N,TT)
  logprice_lag_2<-matrix(0,N,TT)
  logsales_lag_2<-matrix(0,N,TT)
  logprice_lag_3<-matrix(0,N,TT)
  logsales_lag_3<-matrix(0,N,TT)
  logprice_lag_4<-matrix(0,N,TT)
  logsales_lag_4<-matrix(0,N,TT)
  
  logsales_lag_4[,1]<-logsales_lag_3[,1]<-logsales_lag_2[,1]<-logsales_lag[,1]<-logsales[,1]<-sales_fixed_effect
  logprice_lag_4[,1]<-logprice_lag_3[,1]<-logprice_lag_2[,1]<-logprice_lag[,1]<-logprice[,1]<-price_fixed_effect
  week<-matrix(0,N,TT)
  
  
  
  for (t in 5:TT) {
    ### logprice first difference 
    
    
    
    ## logpr = logprice - logprice_lag
    logpr[,t]<-predict(fs$treat.fit,mydf)+ P.tilde_first_diff[,t]
    ## generate logprice from logprice_lag and its first difference
    logprice[,t]<-logpr[,t]+logprice[,t-1]+price_fixed_effect
    
    ## logsls = logsales - logprice_lag
    logsls[,t]<-predict(fs$outcome.fit,mydf)+(het_beta0)*P.tilde_first_diff[,t] + U_first_diff[,t]
    ## generate logprice from logprice_lag and its first difference
    logsales[,t]<-logsls[,t]+logsales[,t-1]+sales_fixed_effect
    
    
    week[,t]<-t
    mydf$logprice_lag_4<-mydf$logprice_lag_3
    mydf$logprice_lag_3<-mydf$logprice_lag_2
    mydf$logprice_lag_2<-mydf$logprice_lag
    mydf$logprice_lag<-logprice[,t]
    
    mydf$logsales_lag_4<-mydf$logsales_lag_3
    mydf$logsales_lag_3<-mydf$logsales_lag_2
    mydf$logsales_lag_2<-mydf$logsales_lag
    mydf$logsales_lag<-logsales[,t]
    
    
    logsales_lag[,t]<-logsales[,t-1]
    logsales_lag_2[,t]<-logsales[,t-2]
    logsales_lag_3[,t]<-logsales[,t-3]
    logsales_lag_4[,t]<-logsales[,t-4]
    
    
    
    logprice_lag[,t]<-logprice[,t-1]
    logprice_lag_2[,t]<-logprice[,t-2]
    logprice_lag_3[,t]<-logprice[,t-3]
    logprice_lag_4[,t]<-logprice[,t-4]
    
    
  }
  return(data.frame(logsales=as.numeric(logsales),
                    logprice=as.numeric(logprice),
                    logsales_lag=as.numeric(logsales_lag),
                    logprice_lag=as.numeric(logprice_lag),
                    logsales_lag_2=as.numeric(logsales_lag_2),
                    logprice_lag_2=as.numeric(logprice_lag_2),
                    logsales_lag_3=as.numeric(logsales_lag_3),
                    logprice_lag_3=as.numeric(logprice_lag_3),
                    logsales_lag_4=as.numeric(logsales_lag_4),
                    logprice_lag_4=as.numeric(logprice_lag_4),
                    logprice_1df=as.numeric(logpr),
                    logsales_1df=as.numeric(logsls),
                    het_beta0 = as.numeric(kronecker(rep(1,TT),het_beta0)),
                    P.tilde=as.numeric(P.tilde),
                    U=as.numeric(U),
                    P.tilde_first_diff=as.numeric(P.tilde_first_diff),
                    U_first_diff=as.numeric(U_first_diff),
                    week=as.numeric(week),
                    Item=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Item))))),
                    SiteName=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$SiteName))))),
                    ChannelName=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$ChannelName))))),
                    Level1=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Level1))))),
                    Level2=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Level2))))),
                    Level3=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Level3))))),
                    Level4=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Level4))))),
                    Level5=as.factor(as.character(kronecker(rep(1,TT),as.numeric(as.character(mydf$Level5)))))
  ))
}

second_stage<-function(mydata,fs,
                       grouping_level,
                       categoryname,
                       second_stage_method_names,
                       controls,target_controls,
                       ...) {
  treat_res<-fs$treat
  outcome_res<-fs$outcome
  #controls<-as.matrix(mydata[,grep("controls",colnames(mydata),value=TRUE)])
  het_treat<-matrix(rep(treat_res,dim(controls)[2]),ncol= dim(controls)[2])*controls
  
  ## run second stage regression
  res<-data.frame(RowID=target_controls[,1])
  est<-data.frame(RowID=rep(1,dim(controls)[2]))
  htheta<-list()
  for (method_name in second_stage_method_names) {
    # get second stage estimator
    method<-get(method_name)
    if (method_name =="DebiasedLasso") {
      # execute estimator 
      result<-method(x=het_treat,y=outcome_res,htheta=htheta[["Lasso"]],...)
      
    } else {
      # execute estimator 
      result<-method(x=het_treat,y=outcome_res,...)
      if (method_name=="Lasso") {
        htheta[["Lasso"]]<-result$estimator
      }
    }
    
    # add variance estimator for OLS and Ridge
    res[,method_name]<-data.frame(est =as.numeric(target_controls%*%result$estimator))
    est[,method_name]<-data.frame(est =as.numeric(result$estimator))
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


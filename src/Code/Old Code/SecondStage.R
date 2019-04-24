second_stage<-function(het_treat,by_het,outcome,mu_bar=0.1,subset=NULL,
                       toplevel=NULL,...) {
  
 show(mu_bar)
  # If shrinkage level provided, Lasso, Ridge, OLS
  # Otherwise OLS only
  
  if (!is.null(subset)) {
    outcome<-outcome[subset]
  }
  
  run_ols<-is.invertible(t(het_treat)%*%het_treat)
  if (run_ols) {
    htheta.OLS<-OLS(het_treat,outcome)
    names(htheta.OLS)<-colnames(het_treat)
  }
  
  
  
  if (!is.null(toplevel)) {
    
    # Store 
    # Run Lasso and Ridge
    show(dim(het_treat))
    # Cross validated Lasso with no penalty on top level
    fit<-cv.gamlr(het_treat,outcome,
                  standardize=FALSE,
                  intercept=TRUE,
                  lmr=1e-7,
                  free = grep(toplevel,colnames(het_treat),value=TRUE))
    # Coefficients
    htheta<-coef(fit,s="min")[-1]
    names(htheta)<-colnames(het_treat)

    # Compute Ridge 
    htheta.JM<-JM(het_treat,outcome,intercept=FALSE,standardize=FALSE,htheta=htheta,mu_bar=mu_bar)
    

  } 

  out<-NULL
  if (!is.null(toplevel) & run_ols) {

    out<-list(OLS=htheta.OLS,
              Lasso = htheta,
              Ridge = htheta.JM
              )
  } else if ( run_ols) {
    out<-list(OLS=htheta.OLS)
    
  } else if (!is.null(toplevel)) {
   out<-list(Lasso = htheta,
             Ridge = htheta.JM)
  }
 return(out)
 
 ## need to clean up by_het matrix before running any regs 

}
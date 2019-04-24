## Basic functions

## OLS



OLS<-function(x,y,...) {
  S<-tryCatch(solve(t(x)%*%x))
  if (class(S[1] ) == "character") {
    b<-rep(10^6, dim(x)[2])
    show("Singular matrix")
  } else {
    b<-S%*%(t(x)%*%y)
  }
  
  res<-b
  class(res)<-'num'
  x<-as.matrix(x)
  VAR<-white_sd(x=x,y=y,b=b)
  attr(res,'st.error.hat')<-sqrt(diag(VAR))
  attr(res,'vars')<-1:dim(x)[2]
  attr(res,'White')<-VAR
  return(res)
}

predict.num<-function(t,newdata) {
  res<-newdata%*%t
  return(res)
}
## White standard errors
white_sd<-function(M,x,y,b) {
  
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
  S<-tryCatch(solve(M))
  if (class(S[1] ) == "character") {
    VAR<-diag(dim(x)[2])
  } else {
    VAR<-(solve(M)%*%(VV)%*% solve(M))/n
  }
  
  
  return(VAR)
}
# JM version of Lasso
Lasso <- function( x, y, lambda = NULL, intercept = TRUE,penalty,...){
  #
  # Compute the Lasso estimator:
  # - If lambda is given, use glmnet and standard Lasso
  # - If lambda is not given, use square root Lasso
  #
  p <- ncol(x);
  n <- nrow(x);
  
  if  (is.null(lambda)){
    lambda <- sqrt(qnorm(1-(0.1/p))/n);
    outLas <- slim(x,y,lambda=c(lambda),method="lq",q=2,verbose=FALSE);
    # Objective : sqrt(RSS/n) +lambda *penalty
    if (intercept==TRUE) {
      res<- (c(as.vector(outLas$intercept),as.vector(outLas$beta)))
    }  else {
      res<-as.vector(outLas$beta);
    }
    #return(outLas)
  } else {
    outLas<- glmnet(x, y, family = c("gaussian"), alpha =1, intercept = intercept );
    #return(outLas)
    # Objective :1/2 RSS/n +lambda *penalty
    if (intercept==TRUE){
     res<-as.vector(coef(outLas ,s=lambda));
    } else {
      res<-as.vector(coef(outLas ,s=lambda))[2:(p+1)];
    }
  }
  
 # res<-unbiased.Lasso
  
  class(res)<-'num'
  
  # res$true.dgp<-as.numeric(b)
  return(res)
  
  
}
ridge<-function (x,y,lambda,...) {
  x<-as.matrix(x)
  p<-dim(x)[2]
  S<-tryCatch(solve(1/n *t(x)%*%x+diag(lambda)))
  if (class(S[1] ) == "character") {
    b<-rep(10^6, dim(x)[2])
    show("Singular matrix")
  } else {
    b<-S%*%((t(x)%*%y)*1/n)
  }
  
  res<-b
  class(res)<-'num'
  x<-as.matrix(x)
  attr(res,'st.error.hat')<-ridge_sd(x=x,y=y,b=b,lambda=lambda)
  attr(res,'vars')<-1:dim(x)[2]
  return(res)
}
ridge_sd<-function(x,y,b,lambda,...) {
  x<-as.matrix(x)
  n<-length(y)
  p<-dim(x)[2]
  V<-matrix(0,p,n)
  y.hat<-x%*%b
  for (i in 1:n) {
    V[,i]<-(y[i] - y.hat[i])*x[i,]
  }
  M<-(solve(1/n*t(x)%*%x+diag(lambda)))
  
  
  
  VV<-(V%*%t(V))/(n-p)
  S<-tryCatch(solve(M))
  if (class(S[1] ) == "character") {
    VAR<-diag(dim(x)[2])
  } else {
    VAR<-(solve(M)%*%(VV)%*% solve(M))/n
  }
  
  
  return(sqrt(diag(VAR)))
}

Lasso2<-function(x,y,htheta,method.lasso,intercept=TRUE,standardize=FALSE,...) {
 
  res<-method.lasso(x=x,y=y,intercept = intercept,standardize = standardize,free=1)
  b.hat<-as.numeric(coef(res))[2:length(coef(res))]
  show(length(b.hat))
  return(b.hat)
}

PL<-function(x,y,htheta,...) {
  n<-length(y)
  intercept<-attr(htheta,'intercept')
  if (intercept) {
    
    x<-cbind(rep(1,n),x)
  }
  vars<-(1:dim(x)[2])[abs(htheta)>0]

  b<-OLS(x = x[,vars],y)
  b.hat<-rep(0,dim(x)[2])
  b.hat[vars]<-b
  attr(b.hat,'vars')<-vars
  if (intercept) {
    
    b.hat<-b.hat[2:dim(x)[2]]
  }
  return(b.hat)
}
# Way to deal with hierarchical/binary features
# x treatments
# y outcome
# b.prelim - preliminary est
# mu - regularizer
JM<-function (x,y,htheta,het =TRUE,mu_bar=0.01,ci_alpha=0.05,standardize, 
              intercept,lambda,b,maxiter = 50000, threshold = 1e-7, verbose = TRUE,mu = 0.1,...) {
  
  if (missing(standardize)) {
    show("HI")
    show(attr(htheta,'standardize'))
    standardize<-attr(htheta,'standardize')
    
  }
  if (missing(intercept)) {
    intercept<-attr(htheta,'intercept')
    
  }
  if (missing(lambda)) {
    lambda<-attr(htheta,'lambda')
    
  }
  
  x<-as.matrix(x)
  p <- ncol(x);
  n <- nrow(x);
  pp <- p;

  
  if (standardize) {
    col.norm <- (sqrt((1/n)*diag(t(x)%*%x)));
  } else {
    col.norm<-rep(1,pp)
  }
 
  x <- x %*% diag(1/col.norm);
  
 
  
 
  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),x);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
    show(pp)
  } else {
    Xb <- x;
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);
  
  if ((n>=2*p)){
    tmp <- eigen(sigma.hat)
    tmp <- min(tmp$values)/max(tmp$values)
  }else{
    tmp <- 0
  }
  
  if ((n>=2*p)&&(tmp>=1e-4)){
    M <- solve(sigma.hat)
  }else if (!is.null(mu_bar)){ 
    show("HI")
    muu<-mu_bar
    M<-solve(sigma.hat+muu*diag(pp))
    while (max(abs(M%*%sigma.hat-diag(pp)))>mu_bar) {
      muu<-muu/2
      M<-solve(sigma.hat+muu*diag(pp))
    }
    
  } else {
    show(mu_bar)
    M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
    
  }
  
 # if (missing(htheta)) {
 #   htheta <- Lasso (x,y,lambda=lambda,intercept=intercept);
    
 # }
  
  V<-matrix(0,pp,n)
  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n); 
 A <- M %*% sigma.hat %*% t(M);
  Xb <- Xb%*% diag(1/col.norm);
  y.hat<-Xb%*%(as.numeric(unbiased.Lasso))
  for (i in 1:n) {
    V[,i]<-(y[i] - y.hat[i])*Xb[i,]
  }
  # Experimental thing 
  # n-pp = proxy for a trace of some matrix
  sigma.hat.het<-(V%*%t(V))/(n-pp)
  
  A.het<-(diag(col.norm)%*%M%*%diag(col.norm)) %*% sigma.hat.het %*% t((diag(col.norm)%*%M%*%diag(col.norm)));
  
  SE<-sqrt(diag(A.het))/(sqrt(n)) 
  
 

 
  htheta <- htheta*col.norm;
  unbiased.Lasso <- unbiased.Lasso*col.norm;
 # addlength <- addlength*col.norm;
  
  
  
   
 # A <- M %*% sigma.hat %*% t(M);
  #SE<-s.hat*sqrt(diag(A))/sqrt(n)
  
 
 
 
  if (intercept) {
    res<-unbiased.Lasso[2:pp]
    attr(res, 'cons')<-unbiased.Lasso[1]
    unbiased.Lasso<-unbiased.Lasso[2:pp]
    SE<-SE[2:pp]
  } else {
    res<-unbiased.Lasso
  }
# res<-unbiased.Lasso
 attr(res,'st.error.hat')<-SE
 attr(res,"White")<-A.het/n
 #+(1/qnorm(1-ci_alpha/2))*addlength
 
 
  class(res)<-'num'
  st.error.hat<-SE
  attr(res,'st.error.hat')<-as.numeric(st.error.hat)
  return(res)
}

white_sd_JM<-function(x,y,b,htheta,intercept, standardize,ci_alpha,lambda ,mu_bar = 0.01,...) {
  if (missing(standardize)) {
    show("HI")
    standardize<-attr(b,'standardize')
    
  }
  if (missing(intercept)) {
    intercept<-attr(b,'intercept')
    
  }
  x<-as.matrix(x)
  p <- ncol(x);
  n <- nrow(x);
  
  pp <- p;
  show(pp)
  
  if (standardize) {
    col.norm <- 1/sqrt((1/n)*diag(t(x)%*%x));
  } else {
    col.norm<-rep(1,pp)
  }
  
  
  
  x <- x %*% diag(col.norm);
  
  
  
  
  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),x);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
    show(pp)
  } else {
    Xb <- x;
  }
  sigma.hat <- (1/n)*(t(Xb)%*%Xb);
  
 
  
  if ((n>=2*p)){
    tmp <- eigen(sigma.hat)
    tmp <- min(tmp$values)/max(tmp$values)
  }else{
    tmp <- 0
  }
  
  if ((n>=2*p)&&(tmp>=1e-4)){
    M <- solve(sigma.hat)
  }else if (!is.null(mu_bar)){   
    muu<-mu_bar
    M<-solve(sigma.hat+muu*diag(pp))
    while (max(abs(M%*%sigma.hat-diag(pp)))>mu_bar) {
      muu<-muu/2
      M<-solve(sigma.hat+muu*diag(pp))
    }
    
  } else {
    M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
    
  }
  
  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n); 
  A <- M %*% sigma.hat %*% t(M);
  noise <- NoiseSd(  unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  MM <- M%*%sigma.hat - diag(pp);
  addlength<-rep(0,pp)
  for (i in 1:pp){
    effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
    effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
    addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  }  
  
  
  
    V<-matrix(0,pp,n)
    
    
    Xb <- Xb%*% diag(1/col.norm);
    y.hat<-Xb%*%(as.numeric(b))
    for (i in 1:n) {
      V[,i]<-(y[i] - y.hat[i])*Xb[i,]
    }
  # Experimental thing 
  # n-pp = proxy for a trace of some matrix
    sigma.hat.het<-(V%*%t(V))/(n-pp)
    
    A.het<-(diag(col.norm)%*%M%*%diag(col.norm)) %*% sigma.hat.het %*% t((diag(col.norm)%*%M%*%diag(col.norm)));
    
  SE<-sqrt(diag(A.het))/(sqrt(n)) + 1/qnorm(1-ci_alpha/2)*addlength
  if (intercept == TRUE) {
    SE<-SE[2:pp]
  }
    return(SE) 
  }

RDML<-function(x,y,method.debiased=gamlr,beta=NULL,split = FALSE,...) {
  b.hat<-rep(0,dim(x)[2])
  st.error.hat<-rep(0,dim(x)[2])
  method.debiased<-gamlr
  
  
  # Initialize z
  z<-x
  if (split) {
    
    
    indices<-sample(1:dim(x)[1],dim(x)[1],replace=FALSE)
    inds.train<-indices[1:floor(dim(x)[1]/2)]
    inds.est<-setdiff(indices,inds.train)
    
    
    
    for (k in 1:dim(x)[2]) {
      fit_k<-method.debiased(x[inds.train,-k],x[inds.train,k],standardize=TRUE)
      z[inds.est,k]<-x[inds.est,k]-x[inds.est,-k]%*%(coef(fit_k)[-1])
      
      # This line is the only difference between debiased and RDML
      # debiased version:
      fit_y<-method.debiased(x[inds.train,-k],y,standardize=TRUE)
      b.est<-sum(zz[inds.est]*(y[inds.est]- x[inds.est,-k]%*%outcome.coef.train[-k]))/sum(zz[inds.est]^2)
      
      fit_k<-method.debiased(x[inds.est,-k],x[inds.est,k],standardize=TRUE)
      z[inds.train,k]<-x[inds.train,k]-x[inds.train,-k]%*%(coef(fit_k)[-1])
      
      b.train<-sum(zz[inds.train]*(y[inds.train]- x[inds.train,-k]%*%outcome.coef.est[-k]))/sum(zz[inds.train]*x[inds.train,k])
      
      
      b.hat[k]<-(b.est[k]+b.train[k])/2
    }
  } else {
    
    outcome.coef<-coef(method.debiased(x,y,standardize=FALSE,free=1))
    outcome.coef<-outcome.coef[-1]
    e.hat<-y-x%*%outcome.coef
    
    
    for (k in 1:dim(x)[2]) {
      
      fit_k<-method.debiased(x[,-k],x[,k],standardize=TRUE)
      z[,k]<-x[,k]-x[,-k]%*%(coef(fit_k)[-1])
      
      
      b.hat[k]<-sum(zz*(y- x[,-k]%*%outcome.coef[-k]))/sum(zz*x[,k])
      
    }
  }
  
  for (k in 1:dim(x)[2]) {
    
    st.error.hat[k]<-sqrt(mean( (e.hat*z[,k] - mean(e.hat*z[,k]))^2))
    
  }
  attr(b.hat,'st.error.hat')<-as.numeric(st.error.hat)
  return(b.hat)
}



debiased_general<-function(x,y,method.debiased=gamlr,beta=NULL,split = FALSE,res,...) {
  b.hat<-rep(0,dim(x)[2])
  st.error.hat<-rep(0,dim(x)[2])
  # For some reason default value does not go through
   method.debiased<-gamlr
  
  
  # Initialize z
  z<-x
  if (split) {
    
    
    indices<-sample(1:dim(x)[1],dim(x)[1],replace=FALSE)
    inds.train<-indices[1:floor(dim(x)[1]/2)]
    inds.est<-setdiff(indices,inds.train)
    
    outcome.coef.train<-coef(method.debiased(x[inds.train,],y[inds.train],standardize=TRUE,free=1))
    outcome.coef.train<-outcome.coef.train[-1]
    outcome.coef.est<-coef(method.debiased(x[inds.est,],y[inds.est],standardize=TRUE,free=1))
    outcome.coef.est<-outcome.coef.est[-1]
    
    e.hat<-y
    e.hat[inds.est]<-y[inds.est]-x[inds.est,]%*%outcome.coef.train
    e.hat[inds.train]<-y[inds.train]-x[inds.train,]%*%outcome.coef.est
    
    
    
    for (k in 1:dim(x)[2]) {
      #z[,k]<-res[,k]
      z[,k]<-x[,k]-x[,-k]%*%as.numeric(OLS(x[,-k],x[,k]))
      b.est<-sum(zz[inds.est]*(y[inds.est]- x[inds.est,-k]%*%outcome.coef.train[-k]))/sum(zz[inds.est]*x[inds.est,k])
      
      fit_k<-method.debiased(x[inds.est,-k],x[inds.est,k],standardize=TRUE)
      z[inds.train,k]<-x[inds.train,k]-x[inds.train,-k]%*%(coef(fit_k)[-1])
      
      b.train<-sum(zz[inds.train]*(y[inds.train]- x[inds.train,-k]%*%outcome.coef.est[-k]))/sum(zz[inds.train]*x[inds.train,k])
      
      
      b.hat[k]<-(b.est[k]+b.train[k])/2
    }
  } else {
    
    outcome.coef<-coef(method.debiased(x,y,standardize=FALSE,free=1))
    outcome.coef<-outcome.coef[-1]
    e.hat<-y-x%*%outcome.coef
    
    
    for (k in 1:dim(x)[2]) {
      show(k)
      # z[,k]<-res[,k]
      z[,k]<-x[,k]-x[,-k]%*%as.numeric(OLS(x[,-k],x[,k]))
      inds<-z[,k]!=0
      b.hat[k]<-sum(z[inds,k]*(y[inds]- x[inds,-k]%*%outcome.coef[-k]))/sum(z[inds,k]*x[inds,k])
      
    }
  }
  
  for (k in 1:dim(x)[2]) {
    
    st.error.hat[k]<-white_sd(x=z[,k],y=y-x[,-k]%*%outcome.coef[-k],b = b.hat[k])
    #st.error.hat[k]<-sqrt(mean( (e.hat*z[,k] - mean(e.hat*z[,k]))^2))
    
  }
  attr(b.hat,'st.error.hat')<-as.numeric(st.error.hat)
  return(b.hat)
}


debiased<-function(x,y,method.debiased=gamlr,beta=NULL,split = FALSE,...) {
  b.hat<-rep(0,dim(x)[2])
  st.error.hat<-rep(0,dim(x)[2])
  method.debiased<-gamlr
  
  
  # Initialize z
  z<-x
  if (split) {
    
    
    indices<-sample(1:dim(x)[1],dim(x)[1],replace=FALSE)
    inds.train<-indices[1:floor(dim(x)[1]/2)]
    inds.est<-setdiff(indices,inds.train)
    
    outcome.coef.train<-coef(method.debiased(x[inds.train,],y[inds.train],standardize=TRUE,free=1))
    outcome.coef.train<-outcome.coef.train[-1]
    outcome.coef.est<-coef(method.debiased(x[inds.est,],y[inds.est],standardize=TRUE,free=1))
    outcome.coef.est<-outcome.coef.est[-1]
    
    e.hat<-y
    e.hat[inds.est]<-y[inds.est]-x[inds.est,]%*%outcome.coef.train
    e.hat[inds.train]<-y[inds.train]-x[inds.train,]%*%outcome.coef.est
    
    
    
    for (k in 1:dim(x)[2]) {
   #   fit_k<-method.debiased(x[inds.train,-k],x[inds.train,k],standardize=TRUE)
     # z[inds.est,k]<-x[inds.est,k]-x[inds.est,-k]%*%(coef(fit_k)[-1])
      z[inds.est,k]<-x[inds.est,k]-x[inds.est,-k]%*%as.numeric(OLS(x[inds.est,
                                                                     -k],
                                                                   x[inds.est,k]))
      
      b.est<-sum(z[inds.est,k]*(y[inds.est]- x[inds.est,-k]%*%outcome.coef.train[-k]))/sum(z[inds.est,k]*x[inds.est,k])
      
    #  fit_k<-method.debiased(x[inds.est,-k],x[inds.est,k],standardize=TRUE)
    #  z[inds.train,k]<-x[inds.train,k]-x[inds.train,-k]%*%(coef(fit_k)[-1])
      z[inds.train,k]<-x[inds.train,k]-x[inds.train,-k]%*%as.numeric(OLS(x[inds.train,
                                                                     -k],
                                                                   x[inds.train,k]))
      
      b.train<-sum(z[inds.est,k]*(y[inds.train]- x[inds.train,-k]%*%outcome.coef.est[-k]))/sum(z[inds.est,k]*x[inds.train,k])
      
      
      b.hat[k]<-(b.est[k]+b.train[k])/2
    }
  } else {
    
    outcome.coef<-coef(method.debiased(x,y,standardize=FALSE,free=1))
    outcome.coef<-outcome.coef[-1]
    e.hat<-y-x%*%outcome.coef
    

    for (k in 1:dim(x)[2]) {
      
     # fit_k<-method.debiased(x[,-k],x[,k],standardize=FALSE)
     # z[,k]<-x[,k]-x[,-k]%*%(coef(fit_k)[-1])
      z[,k]<-x[,k]-x[,-k]%*%as.numeric(OLS(x[,-k],x[,k]))
      show(k)
     
      b.hat[k]<-sum(z[,k]*(y- x[,-k]%*%outcome.coef[-k]))/sum(z[,k]*x[,k])
      
    }
  }
  
  for (k in 1:dim(x)[2]) {
   
      
      st.error.hat[k]<-white_sd(x=z[,k],y=y-x[,-k]%*%outcome.coef[-k],b = b.hat[k])
      
    
    #st.error.hat[k]<-sqrt(mean( (e.hat*z[,k] - mean(e.hat*z[,k]))^2))
    
  }
  attr(b.hat,'st.error.hat')<-as.numeric(st.error.hat)
  return(b.hat)
}
  

## Collapse small categories

attr(OLS,'name')<-"OLS"
attr(PL, 'name')<-"PL"
attr(JM, 'name')<-"JM"
attr(RDML, 'name')<-"RDML"
attr(debiased, 'name')<-"debiased"
attr(debiased_general, 'name')<-"deb"
attr(ridge, 'name')<-"ridge"
attr(Lasso2, 'name')<-"Lasso2"
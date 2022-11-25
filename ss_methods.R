###  Orthogonal Least Squares ###
OLS<-function(x,y,...) {
  S<-tryCatch(solve(t(x)%*%x))
  if (class(S[1] ) == "character") {
    stop("Singular matrix")
  } else {
    b.hat<-S%*%(t(x)%*%y)
  }
  
  vcov<-white_cov(x=x,y=y,b=b.hat)
  #vcov=matrix(0,length(as.numeric(b.hat)),length(as.numeric(b.hat)))
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
  e<-matrix(rep((y-y.hat),p),ncol=p)
  V<-e*x
  # for (i in 1:n) {
  #  V[,i]<-(y[i] - y.hat[i])*x[i,]
  # }
  if (missing(M)) {
    M<-1/n*(t(x)%*%x)
  }
  
  VV<-(t(V)%*%V)/(n-p)
  VAR<-(solve(M)%*%(VV)%*% solve(M))/n
  
  return(VAR)
}
### Orthogonal Lasso ###
Lasso<-function(x,y,categoryname,...){
    fit<-cv.gamlr(x,y,  standardize=TRUE,      intercept=TRUE,     lmr=1e-7)
  htheta<-coef(fit,s="min")[-1]
  names(htheta)<-colnames(x)
  return(list(estimator=htheta))
}

### Double Orthogonal Lasso
DebiasedLasso<-function(x,y,lambda_ridge,htheta,...){
  p<-dim(x)[2]
  ## ridge regularized inverse
  M<-solve (t(x)%*% x +lambda_ridge*diag(p)) 
  ## Double Orthogonal Ridge estimator
  
  b.hat<-M%*% t(x)%*%( y - x%*%htheta) + htheta
  ## White-type standard error
  vcov<-white_cov(x=x,y=y,b=htheta,M=M)
  #vcov=matrix(0,length(as.numeric(b.hat)),length(as.numeric(b.hat)))
  return(list(estimator = b.hat,
              vcov=vcov ))
}

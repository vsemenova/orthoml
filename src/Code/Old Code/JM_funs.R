#
# Authors: Hamid Javadi, Adel Javanmard, Andrea Montanari, Sven Schmit
# Date: April 24th, 2014
#
# Confidence intervals for high-dimensional regression using the method of
# A. Javanmard, A. Montanari
# "Confidence intervals and hypothesis testing for high-dimensional regression"
# 2013, arXiv:1306.3171
#


SoftThreshold <- function( x, lambda ) {
#
# Standard soft thresholding
#
  if (x>lambda){
    return (x-lambda);}
  else {
    if (x< (-lambda)){
      return (x+lambda);}
    else {
      return (0); }
  }
}

InverseLinftyOneRow <- function ( sigma, i, mu, maxiter=50, threshold=1e-2 ) {
  p <- nrow(sigma);
  rho <- max(abs(sigma[i,-i])) / sigma[i,i];
  mu0 <- rho/(1+rho);
  beta <- rep(0,p);
    
  if (mu >= mu0){
    beta[i] <- (1-mu0)/sigma[i,i];
    returnlist <- list("optsol" = beta, "iter" = 0);
    return(returnlist);
  }
  
  diff.norm2 <- 1;
  last.norm2 <- 1;
  iter <- 1;
  iter.old <- 1;
  beta[i] <- (1-mu0)/sigma[i,i];
  beta.old <- beta;
  sigma.tilde <- sigma;
  diag(sigma.tilde) <- 0;
  vs <- -sigma.tilde%*%beta;
  
  while ((iter <= maxiter) && (diff.norm2 >= threshold*last.norm2)){    
    
    for (j in 1:p){
      oldval <- beta[j];
      v <- vs[j];
      if (j==i)
        v <- v+1;    
      beta[j] <- SoftThreshold(v,mu)/sigma[j,j];
      if (oldval != beta[j]){
      	vs <- vs + (oldval-beta[j])*sigma.tilde[,j];
      }
    }
    
    iter <- iter + 1;
    if (iter==2*iter.old){
      d <- beta - beta.old;
      diff.norm2 <- sqrt(sum(d*d));
      last.norm2 <-sqrt(sum(beta*beta));
      iter.old <- iter;
      beta.old <- beta;
      if (iter>10)
         vs <- -sigma.tilde%*%beta;
    }
  }

  returnlist <- list("optsol" = beta, "iter" = iter)
  return(returnlist)
}

InverseLinfty <- function(sigma, n, resol=1.5, mu=NULL, maxiter=50, threshold=1e-2, verbose = TRUE) {
  isgiven <- 1;
  if (is.null(mu)){
  	isgiven <- 0;
  }

  p <- nrow(sigma);
  M <- matrix(0, p, p);
  xperc = 0;
  xp = round(p/10);
  for (i in 1:p) {
        if ((i %% xp)==0){
          xperc = xperc+10;
          if (verbose) {
            print(paste(xperc,"% done",sep="")); }
        }
  	if (isgiven==0){
  	  mu <- (1/sqrt(n)) * qnorm(1-(0.1/(p^2)));
  	}
  	mu.stop <- 0;
  	try.no <- 1;
  	incr <- 0;
  	while ((mu.stop != 1)&&(try.no<10)){
  	  last.beta <- beta
  	  output <- InverseLinftyOneRow(sigma, i, mu, maxiter=maxiter, threshold=threshold)
  	  beta <- output$optsol
  	  iter <- output$iter
  	  if (isgiven==1){
  	  	mu.stop <- 1
  	  }
  	  else{
            if (try.no==1){
              if (iter == (maxiter+1)){
                incr <- 1;
                mu <- mu*resol;
              } else {
                incr <- 0;
                mu <- mu/resol;
              }
            }
            if (try.no > 1){
              if ((incr == 1)&&(iter == (maxiter+1))){
                mu <- mu*resol;
              }
              if ((incr == 1)&&(iter < (maxiter+1))){
                mu.stop <- 1;
              }
              if ((incr == 0)&&(iter < (maxiter+1))){
                mu <- mu/resol;
              }
              if ((incr == 0)&&(iter == (maxiter+1))){
                mu <- mu*resol;
                beta <- last.beta;
                mu.stop <- 1;
              }                        
            }
          }
  	  try.no <- try.no+1
  	}
  	M[i,] <- beta;
  }
  return(M)
}

NoiseSd <- function( yh, A, n ){
  ynorm <- sqrt(n)*(yh/sqrt(diag(A)));
  sd.hat0 <- mad(ynorm);

  zeros <- (abs(ynorm)<3*sd.hat0);
  y2norm <- sum(yh[zeros]^2);
  Atrace <- sum(diag(A)[zeros]);
  sd.hat1 <- sqrt(n*y2norm/Atrace);

  ratio <- sd.hat0/sd.hat1;
  if (max(ratio,1/ratio)>2)
    print("Warning: Noise estimate problematic");

  s0 <- sum(zeros==FALSE);
  return (list( "sd" = sd.hat1, "nz" = s0));
}


if (FALSE) {
  
  
  
  Lasso <- function( X, y, lambda = NULL, intercept = TRUE){
    #
    # Compute the Lasso estimator:
    # - If lambda is given, use glmnet and standard Lasso
    # - If lambda is not given, use square root Lasso
    #
    p <- ncol(X);
    n <- nrow(X);
    
    if  (is.null(lambda)){
      lambda <- sqrt(qnorm(1-(0.1/p))/n);
      outLas <- slim(X,y,lambda=c(lambda),method="lq",q=2,verbose=FALSE);
      # Objective : sqrt(RSS/n) +lambda *penalty
      if (intercept==TRUE) {
        return (c(as.vector(outLas$intercept),as.vector(outLas$beta)))
      }  else {
        return (as.vector(outLas$beta));
      }
      # return(outLas)
    } else {
      outLas <- glmnet(X, y, family = c("gaussian"), alpha =1, intercept = intercept );
      # return(outLas)
      # Objective :1/2 RSS/n +lambda *penalty
      if (intercept==TRUE){
        return (as.vector(coef(outLas ,s=lambda)));
      } else {
        return (as.vector(coef(outLas ,s=lambda))[2:(p+1)]);
      }
    }
  }
  
  
SSLasso <- function (X, y, het = TRUE,b=NULL,alpha=0.05, lambda = NULL, mu = NULL, intercept = TRUE, 
                     resol=1.3, maxiter=50, threshold=1e-2, verbose = TRUE,mu_bar=0.05) {
#
# Compute confidence intervals and p-values.
#
# Args:
#   X     :  design matrix
#   y     :  response
#   alpha :  significance level
#   lambda:  Lasso regularization parameter (if null, fixed by sqrt lasso)
#   mu    :  Linfty constraint on M (if null, searches)
#   resol :  step parameter for the function that computes M
#   maxiter: iteration parameter for computing M
#   threshold : tolerance criterion for computing M
#   verbose : verbose?
#
# Returns:
#   noise.sd: Estimate of the noise standard deviation
#   norm0   : Estimate of the number of 'significant' coefficients
#   coef    : Lasso estimated coefficients
#   unb.coef: Unbiased coefficient estimates
#   low.lim : Lower limits of confidence intervals
#   up.lim  : upper limit of confidence intervals
#   pvals   : p-values for the coefficients						 
#
  #show(het)
  p <- ncol(X);
  n <- nrow(X);
  pp <- p;
  col.norm <- 1/sqrt((1/n)*diag(t(X)%*%X));
  X <- X %*% diag(col.norm);

  htheta <- Lasso (X,y,lambda=lambda,intercept=intercept);

  if (intercept==TRUE){
    Xb <- cbind(rep(1,n),X);
    col.norm <- c(1,col.norm);
    pp <- (p+1);
  } else {
    Xb <- X;
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
  }else{
    #M <- InverseLinfty(sigma.hat, n, resol=resol, mu=mu, maxiter=maxiter, threshold=threshold, verbose=verbose);
    
    muu<-log(p)/n/p
    M<-solve(sigma.hat+muu*diag(pp))
    while (max(abs(M%*%sigma.hat-diag(pp)))>mu_bar) {
      muu<-muu/2
      M<-solve(sigma.hat+muu*diag(pp))
    }
  }
  
  unbiased.Lasso <- as.numeric(htheta + (M%*%t(Xb)%*%(y - Xb %*% htheta))/n);
  # homo st.errors
  
  
  A <- M %*% sigma.hat %*% t(M);
  
  # need to understand whether cols are normalized 
  # which b
  
  if (het) {
    if (is.null(b)) {
      b<-unbiased.Lasso*col.norm
    }
    V<-matrix(0,pp,n)
   if (intercept == TRUE) {
     col.norm<-c(1,col.norm)
   }

    X <- X %*% diag(1/col.norm);
    y.hat<-X%*%(b)
    for (i in 1:n) {
      V[,i]<-(y[i] - y.hat[i])*X[i,]
    }
    sigma.hat.het<-(V%*%t(V))/n
    #show("JM")
   
    A.het<-(diag(col.norm)%*%M%*%diag(col.norm)) %*% sigma.hat.het %*% t((diag(col.norm)%*%M%*%diag(col.norm)));
   
    SE.het<-sqrt(diag(A.het))/(sqrt(n))
    interval.sizes.het<-qnorm(1-(alpha/2))*sqrt(diag(A.het))/(sqrt(n));
    if (intercept == TRUE) {
      interval.sizes.het <- interval.sizes.het[2:pp];
    }
  #  p.vals.het<-2*(1-pnorm(sqrt(n)*abs(unbiased.Lasso)/(col.norm[(pp-p+1):pp]*sqrt(diag(A.het[(pp-p+1):pp,(pp-p+1):pp])))))
    
  } else {
    SE.het<-NULL
  }
 
 

  

  noise <- NoiseSd( unbiased.Lasso, A, n );
  s.hat <- noise$sd;
  
  interval.sizes <- qnorm(1-(alpha/2))*s.hat*sqrt(diag(A))/(sqrt(n));
  
  if  (is.null(lambda)){
    lambda <- s.hat*sqrt(qnorm(1-(0.1/p))/n);
  }
  
  addlength <- rep(0,pp);
  MM <- M%*%sigma.hat - diag(pp);
  for (i in 1:pp){
  	effectivemuvec <- sort(abs(MM[i,]),decreasing=TRUE);
	effectivemuvec <- effectivemuvec[0:(noise$nz-1)];
	addlength[i] <- sqrt(sum(effectivemuvec*effectivemuvec))*lambda;
  }  
 
  htheta <- htheta*col.norm;
  unbiased.Lasso <- unbiased.Lasso*col.norm;
  interval.sizes <- interval.sizes*col.norm;
  addlength <- addlength*col.norm;
  

  if (intercept==TRUE){
    htheta <- htheta[2:pp];
    unbiased.Lasso <- unbiased.Lasso[2:pp];
    interval.sizes <- interval.sizes[2:pp];
   
    addlength <- addlength[2:pp];
  }  
 p.vals <- 2*(1-pnorm(sqrt(n)*abs(unbiased.Lasso)/(s.hat*col.norm[(pp-p+1):pp]*sqrt(diag(A[(pp-p+1):pp,(pp-p+1):pp])))))
			
  returnList <- list("noise.sd" = s.hat,
                     "norm0" = noise$nz,
                     "coef" = htheta,
                     "unb.coef" = unbiased.Lasso,
                     "low.lim" = unbiased.Lasso - interval.sizes - addlength,
                     "up.lim" = unbiased.Lasso + interval.sizes + addlength,
					 "pvals" = p.vals,
					 "SE" = s.hat*sqrt(diag(A))/(sqrt(n))*col.norm,
					 "SE.het"=SE.het,
					 "addlength"=addlength,
           "M"=M,
           sigma.hat = sigma.hat)
					 
  return(returnList)
}

}

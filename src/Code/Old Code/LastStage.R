#### Last Stage
last_stage<-function(x,y,method,num_splits=1,htheta = NULL,coefs=NULL,ci_alpha=0.05,lambda=0.1,INDS,
           method.debiased=NULL,method.lasso=NULL,intercept=TRUE,standardize=FALSE,...) {
  x<-as.matrix(x)
  n<-dim(x)[1]
  b.hat<-method(x,y,htheta,lambda,coefs,method.lasso=method.lasso,method.debiased=method.debiased,...)
  name<-attr(method,'name')
  show(name)
  #if (name %in% c("OLS","JM","RDML","ridge","debiased")) {
  if (name =="OLS") {
    st.error.hat<-white_sd(x=x,y=y,b=b.hat)
  } else if (name == "JM") {
    
    lambda<- 0.1
    st.error.hat<-white_sd_JM(x=x,y=y,htheta = htheta, b = c( attr(b.hat,'cons'),b.hat),standardize = attr(htheta,'standardize'),
                              intercept = attr(htheta,'intercept'), lambda = lambda, ci_alpha = ci_alpha)
    
  }  else if (name == "RDML") {
    fit<-RDML(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')
    
    
  } else if (name == "debiased") {
    fit<-debiased(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')
  } else if (name == "deb") {
    fit<-debiased_general(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')
  }
  
  else if (name == "ridge") {
    lambda<-c(0,rep(1,dim(x)[2]-1))*0.01
    st.error.hat<-ridge_sd(x=x,y=y,b=b.hat,lambda=lambda)
    
  } else {
    st.error.hat<-rep(1,dim(x)[2])
  }
 

b.hat<-as.numeric(b.hat)
return(list(b.hat = b.hat,
            st.error.hat  = st.error.hat
))

}

  
last_stage_old<-function(x,y,method,num_splits=1,htheta = NULL,coefs=NULL,ci_alpha=0.05,lambda=0.1,INDS,
                     method.debiased=NULL,method.lasso=NULL,intercept=TRUE,standardize=FALSE,...) {
 
  x<-as.matrix(x)
  n<-dim(x)[1]
  show(num_splits)
  if (num_splits == 1){
     
    # if no flipping needed, proceed with method on the whole sample
    b.hat<-method(x,y,unlist(htheta),lambda,coefs,method.lasso=method.lasso,method.debiased=method.debiased,...)  
    st.error.hat<-attr(b.hat,'st.error.hat')
  }   else {
     #INDS<-lapply(1:num_splits, function(i) (n/num_splits * (i-1) + 1) : (n/num_splits * (i)) )
     b.hat.l<-lapply(1:num_splits, function(i,...) {  
      lambda<-c(0,rep(1,dim(x)[2]-1))*0.01
      h<-htheta[[i]]
      #show(h)
      inds<-INDS[[i]]

      method( x = x[inds,],y = y[inds],num_splits=1,htheta = h,lambda,coefs=coefs,
              method.lasso=method.lasso,method.debiased=method.debiased,...)  })
     b.hat<-Reduce(`+`, b.hat.l)/num_splits
     ##### This is a redundant part in case  we need more info than b.hat and st.error.hat
     # Unite selected vars by different splits
     b.hat.vars<-lapply(1:num_splits, function (i) attr(b.hat.l[[i]],'vars'))
     attr(b.hat,'vars')<-Reduce( union, b.hat.vars)
     # 
     b.hat.cons<-lapply(1:num_splits, function (i) attr(b.hat.l[[i]],'cons'))
     attr(b.hat,'cons')<-Reduce(`+`, b.hat.cons)/num_splits
     htheta.hat<-Reduce(`+`, htheta)/num_splits 
  
 # }
 
 
 name<-attr(method,'name')
 show(name)
 #if (name %in% c("OLS","JM","RDML","ridge","debiased")) {
  if (name =="OLS") {
    st.error.hat<-white_sd(x=x,y=y,b=b.hat)
  } else if (name == "JM") {

    lambda<- 0.1
    st.error.hat<-white_sd_JM(x=x,y=y,htheta = htheta.hat, b = c( attr(b.hat,'cons'),b.hat),standardize = attr(htheta[[1]],'standardize'),
                              intercept = attr(htheta[[1]],'intercept'), lambda = lambda, ci_alpha = ci_alpha)
    
  }  else if (name == "RDML") {
    fit<-RDML(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')

    
  } else if (name == "debiased") {
    fit<-debiased(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')
  } else if (name == "deb") {
    fit<-debiased_general(x=x,y=y,beta=b.hat)
    st.error.hat<-attr(fit,'st.error.hat')
  }
   
   else if (name == "ridge") {
    lambda<-c(0,rep(1,dim(x)[2]-1))*0.01
      st.error.hat<-ridge_sd(x=x,y=y,b=b.hat,lambda=lambda)
  
  } else {
  st.error.hat<-rep(1,dim(x)[2])
}
} 

  b.hat<-as.numeric(b.hat)
  return(list(b.hat = b.hat,
              st.error.hat  = st.error.hat
              ))

}
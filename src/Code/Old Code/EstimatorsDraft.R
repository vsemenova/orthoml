# drafts of estimators

RDML<-function(x, y,method.rdml,coefs=NULL,beta=NULL,...) {
  # 1 dimensional buckets only
  # no sample splitting so far - OK with lasso
  method<-gamlr
  b.hat<-rep(0,dim(x)[2])
  st.error.hat<-rep(0,dim(x)[2])
  if (is.null(coefs)) {
    coefs<-1:dim(x)[2]
  }
  
  #Quick and dirty attempt not to penalize own predecessors and immediate children
  # Need to use standardize = FALSE in all remove_wrapper
  # free[[1]]<-c(2:6)
  # free[[2]]<-c(1,)
  if (FALSE) {
    treats1cols<-x/matrix(rep(x[,1],dim(x)[2]),ncol=dim(x)[2])
    colnames(treats1cols)<-gsub("^op","",colnames(x))
    
    colnames(treats1cols)[(apply(treats1cols[treats1cols[,2]==1,],2,sum)>0)]
    
    
    key<-list()
    key[[1]]<-"Level2"
    maxkey<-0
    for (k in 2:dim(x)[2]) {
      
      
      key[[k]]<-paste0(setdiff(colnames(treats1cols)[(apply(treats1cols[treats1cols[,k]==1,],2,sum)>0)],
                               c("ownprice",colnames(treats1cols)[k])),collapse="|")
      
      #maxkey<-max(length(key[[k]]),maxkey)
    }
    
    key[[3]]<-paste0(c("Level3","Level4"),collapse="|")
    key[[20]]<-paste0(c("Level2","Level4"),collapse="|")
    key[[30]]<-paste0(c("Level2","Level3"),collapse="|")
  }
  
  for (k in coefs) {
    # treats<-remove_wrapper(controls = x[,-k],
    #                         target = x[,k], method = method,num_splits = 1,free=grep(key[[k]],colnames(treats1)[-k]))$res
    treats<-remove_wrapper(controls = x[,-k], target = x[,k], method =gamlr,num_splits =1,standardize=FALSE)$res
    # treats<-x[,k]
    # treats<-treats-method(x[,-k],)
    #fit<-method(x[,-k],x[,k])
    # t<-cbind(x[,1:3], apply(x[,4:dim(x)[2]],1,sum))
    #  treats<-x[,k]-t%*%OLS(t,x[,k])
    
    #treats<-as.numeric(treats)
    show(k)
    #  outcome<-remove_wrapper(controls =x[,-k],
    #                         target = y, method = method,num_splits =1,free=grep(key[[k]],colnames(treats1)[-k]))$res
    #z<-cbind(apply(x[,-k],1,sum),apply(x[,])
    outcome<-remove_wrapper(controls =x[,-k], target = y, method =gamlr,num_splits =1,standardize=FALSE)$res
    # outcome<-as.numeric(outcome)
    b.hat[k]<-OLS(as.numeric(treats),as.numeric(outcome))
    if (is.null(beta)) {
      beta<-b.hat
    }
    st.error.hat[k]<-white_sd(x=as.numeric(treats),y=as.numeric(outcome),b=beta[k])
  } 
  
  
  # show(k)
  # inds<-1:floor(length(y)/2)
  
  
  
  
  
  
  
  attr(b.hat,'st.error.hat')<-as.numeric(st.error.hat)
  return(b.hat)
  
}

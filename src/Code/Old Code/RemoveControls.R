### Partialling out functions
### ... are optional args parsed to method

## Fit a model using method
fit<-function(controls,target, method,at.true,...) {

  if (!missing (at.true)) {
    attr(target,'true')<-at.true
  } else {
    #show("No attribute parsed. Using the attached attribute if any.")
    
  }
 
  res<- method(x = controls, y = target,lmr = 1e-7,...)

  return(res)
}

predict.fit<-function(newdata,fit.c) {
 
  newdata<-as.matrix(newdata)
  pred<- predict(fit.c,newdata,s="min")
  return(pred)
}


remove.fit<-function(newdata,target, fit.c) {
  # takes matrix and a list of models used to fit treats
  # returns matrix
  target<-as.matrix(target)
  predict<-lapply(fit.c, predict.fit,newdata=newdata   )
  res<-do.call(cbind,predict)
  res<-target - res
  #show(res)
}
# partial out controls :
### train a model on a set of inds
### take residuals on set of inds.est
remove_fixed_inds<-function(inds,inds.est, controls,target,method,at.true=NULL,...) {
  controls<-as.matrix(controls)
  if (is.null(at.true)) {
    at.true.k<-attr(target,'true')
  } else {
    at.true.k<-at.true[[k]]
  }
  target<-as.matrix(target)
 
  
  fit.c<-lapply(1:dim(target)[2], function(k) fit(controls[inds,],
                                                  target[inds,k],
                                                  at.true = at.true.k,                                               
                                                  method = method,...) )
  res<-remove.fit(newdata = controls[inds.est,],
                  target = target[inds.est,],
                  fit.c = fit.c)

  
  return(list(res=res,
              fit.c =fit.c))
}

remove_wrapper<-function(controls, target, method, at.true=NULL,num_splits =2,INDS=NULL,...) {
  controls<-as.matrix(controls)
  n<-dim(controls)[1]

  if (num_splits ==1) {
 
   inds<-inds.est<-1:n
   a<-remove_fixed_inds(inds,inds.est,controls,target,at.true=NULL,method = method,...)
   res<-a$res
   lfit.c<-a$fit.c
   mse<-0
 }  else {
 
   if (is.null(INDS)){
     INDS<-lapply(1:num_splits,function (i) {((n/num_splits)*(i-1) + 1): min((ceiling(n/num_splits)*(i)),n
     )})
   }
   
  target<-as.matrix(target)
#  INDS<-lapply(1:num_splits, function(i) (ceiling(n/num_splits) * (i-1) + 1) : min((ceiling(n/num_splits) * (i)), n ) )
  
  l<-lapply(1:num_splits, function (i,...) {
   # show(n)
   # show(max(INDS[[i]]))
   # show(dim(controls))
   # show(length(setdiff(1:n,INDS[[i]])))
    a<-remove_fixed_inds(setdiff(1:n,INDS[[i]]),INDS[[i]],controls,target,at.true=NULL,method = method,...)}
   
  )
  res<-target
  for (i in 1:num_splits){
    res[INDS[[i]],]<-as.numeric(l[[i]]$res)
  }
  
  lres<-lapply(1:num_splits, function (i) {
    #
    l[[i]]$res
  }
  )
  lfit.c<-lapply(1:num_splits, function (i) {
    l[[i]]$fit.c
  }
  )
  
   #res<-as.matrix(do.call(rbind,lres))

   ## Only if true dgp is known

  if (FALSE) {
  l.true<-lapply(1:num_splits, function (i) {
    remove_fixed_inds(setdiff(1:n,INDS[[i]]),INDS[[i]],controls,target,at.true=NULL,method = true,...)}
)
  res.true<-do.call(rbind,l.true)
  } else {
    # compute out of sample mse 
    res.true<-matrix(0,dim(target)[1],dim(target)[2])
  }
  mse<-apply((res-res.true)^2,2,function(x) sqrt(mean(x)))
   #fits.all<- lapply(1:num_splits, function (k) { return(l[[k]]$fit.c)})
   #res.as.list<-lapply(1:num_splits, function (k) { return(l[[k]]$res)})
   #res.as.m<-do.call(rbind,res.as.list)
   #return(list( res = res.as.m, fit = fits.all))
 
 }
  #show(lfit.c)
return(list( res = res, mse=mse, fit.c = lfit.c))
 #show(head(res))
  
  
}

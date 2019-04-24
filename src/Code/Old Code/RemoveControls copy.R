### Partialling out functions
### ... are optional args parsed to method


# partial out controls :
### train a model on a set of inds
### take residuals on set of inds.est
remove_fixed_partition<-function(inds,inds.est, controls,target,method,...) {
  controls<-as.matrix(controls)
  # evaluate model
  fit.c<-method(controls[inds,], target[inds],...)
  # computed predicted values
  predicted<- predict(fit.c,newdata=controls,...)
  # take residuals between observed and predicted only on inds.test
  res<-target[inds.est]-predicted[inds.est]
  
  return(list(res=res,  fit.c =fit.c))
}

remove_wrapper<-function(controls, target, method, at.true=NULL,num_splits =2,INDS=NULL,...) {
  controls<-as.matrix(controls)
  n<-dim(controls)[1]

  if (num_splits ==1) {
 
   inds<-inds.est<-1:n
   a<-remove_fixed_partition(inds,inds.est,controls,target,at.true=NULL,method = method,...)
   res<-a$res
   lfit.c<-a$fit.c
 }  else {
 
   if (is.null(INDS)){
     INDS<-lapply(1:num_splits,function (i) {((n/num_splits)*(i-1) + 1): min((ceiling(n/num_splits)*(i)),n
     )})
   }
   
  target<-as.matrix(target)
  l<-lapply(1:num_splits, function (i,...) {
    a<-remove_fixed_partition(inds=setdiff(1:n,INDS[[i]]),inds.est=INDS[[i]],controls,target,method = method,...)}
   
  )
  res<-target
  for (i in 1:num_splits){
    res[INDS[[i]],]<-as.numeric(l[[i]]$res)
  }}
return(list( res = res))
  
}

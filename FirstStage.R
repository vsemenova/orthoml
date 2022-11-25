first_stage<-function(treat,outcome,mydata,
                      first_stage_price_formula,
                      first_stage_sales_formula,inds_train,inds_test,my_cons=1,...) {
  
  show("Partialling out controls from the treatment ...")
  
  fe_pdata<-model.matrix(first_stage_price_formula,data=my_data )
  controls<-as.matrix(fe_pdata)
  
  t.treat<-remove_wrapper(controls = controls,
                          target =treat,
                          inds_train=inds_train,...)
  res.treat<-t.treat$res
  treat.fit<-t.treat$fit.c
  
  rownames(t.treat$fit.c$beta)<-sapply(rownames(t.treat$fit.c$beta), make.name)
  ### Residualize prices
  show("Partialling out controls from the outcome ...")
  
  
  fe_pdata<-model.matrix(first_stage_sales_formula,data=my_data )
  controls<-as.matrix(fe_pdata)
  
  
  t.out<-remove_wrapper(controls = controls,
                        target = outcome,
                        inds_train=inds_train,...)
  res.outcome<-t.out$res
  outcome.fit<-t.out$fit.c
  
  rownames(t.out$fit.c$beta)<-sapply(rownames(t.out$fit.c$beta), make.name)
  
  return(list(outcome = res.outcome, 
              treat = res.treat,
              treat.fit=t.treat,
              outcome.fit=t.out))
}

remove_wrapper<-function(controls,target,inds_train,penalty.factor=rep(1,dim(controls)[2]),lambda=NULL,...) {
  
  # evaluate model
  fit.c<-glmnet(controls[inds_train,], target[inds_train],
                family="gaussian",
                standardize=TRUE,
                intercept=FALSE,
                penalty.factor=penalty.factor,
                lambda=lambda)
  # computed predicted values
  predicted<- predict(fit.c,newx=controls,...)
  # take residuals between observed and predicted only on inds.test
  res<-as.numeric(target-predicted)
  
  return(list(res=res,  fit.c =fit.c))
}
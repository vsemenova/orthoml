first_stage<-function(treat,outcome,mydata,
                      first_stage_price_formula,
                      first_stage_sales_formula,inds_train,inds_test,...) {
  
  #fe_pdata<-model.matrix(first_stage_price_formula,data=my_data )
  ### Residualize sales
  ### ... are settings of method.controls
  show("Partialling out controls from the treatment ...")
  ### extract treatment, outcome, and controls
  fe_pdata<-model.matrix(first_stage_price_formula,data=my_data )
  controls<-as.matrix(fe_pdata)
  
  n_items<-length(grep("Item",colnames(fe_pdata),value=TRUE))
  penalty.factor.treat=c(0,rep(1/sqrt(n_items),n_items),rep(1,length(colnames(fe_pdata))-n_items-1))
  lambda.treat=qnorm(1-0.1/(2*dim(fe_pdata)[2]))/sqrt(length(inds_train))
   penalty.factor.treat=rep(1,length(colnames(fe_pdata)))
   lambda.treat=qnorm(1-0.1/(2*dim(fe_pdata)[2]))/sqrt(length(inds_train))
 # t.treat<-lm(first_stage_price_formula,my_data[inds_train,])
 # treat.fit<-predict(t.treat,my_data)
 # res.treat<-treat-treat.fit
   t.treat<-remove_wrapper(controls = controls,
                         target =treat,
                         lambda=lambda.treat,
                         penalty.factor=penalty.factor.treat,
                         inds_train=inds_train,...)
   res.treat<-t.treat$res
   treat.fit<-t.treat$fit.c

  ### Residualize prices
  show("Partialling out controls from the outcome ...")
  
  
  fe_pdata<-model.matrix(first_stage_sales_formula,data=my_data )
  controls<-as.matrix(fe_pdata)
  n_items<-length(grep("Item",colnames(fe_pdata),value=TRUE))
  penalty.factor.outcome=c(0,rep(1/sqrt(n_items),n_items),rep(1,length(colnames(fe_pdata))-n_items-1))
  lambda.outcome=qnorm(1-0.1/(2*dim(fe_pdata)[2]))/sqrt(length(inds_train))
  
  
  t.out<-remove_wrapper(controls = controls,
                        target = outcome,
                        lambda=lambda.outcome,
                        penalty.factor=penalty.factor.outcome,
                        inds_train=inds_train,...)
  res.outcome<-t.out$res
  outcome.fit<-t.out$fit.c
  return(list(outcome = res.outcome, 
              treat = res.treat,
              treat.fit=t.treat,
              outcome.fit=t.out))
}

remove_wrapper<-function(controls,target,inds_train,penalty.factor,lambda,...) {
  
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

first_stage_1d<-function(treat,outcome,mydata,
                         first_stage_price_formula,
                         first_stage_sales_formula,...) {
  
  #fe_pdata<-model.matrix(first_stage_price_formula,data=my_data )
  ### Residualize sales
  ### ... are settings of method.controls
  show("Partialling out controls from the treatment ...")
  ### extract treatment, outcome, and controls
  
  
  t.treat<-lm(first_stage_price_formula,my_data)
  treat.fit<-predict(t.treat,my_data)
  res.treat<-treat-treat.fit
  
  ### Residualize prices
  show("Partialling out controls from the outcome ...")
  
  
  t.outcome<-lm(first_stage_sales_formula,my_data)
  outcome.fit<-predict( t.outcome,my_data)
  res.outcome<-outcome-outcome.fit
  
  return(list(outcome = res.outcome, 
              treat = res.treat,
              treat.fit=t.treat,
              outcome.fit=t.outcome))
}


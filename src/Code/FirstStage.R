### First stage partialling out
first_stage<-function(my_data, treat.name, outcome.name,
                      method.treat = gamlr,
                      method.outcome = gamlr,num_splits,...) {


  ### extract treatment, outcome, and controls
  controls<-prepare_controls_for_first_stage(my_data=my_data,...)
    
  ### SAMPLE SPLITTING
  n<-dim(controls)[1]
  set.seed(888)
  indices<-sample(1:n,size=n,replace=FALSE)
  INDS<-lapply(1:num_splits, function (i) indices[(ceiling(n/num_splits)*(i-1) + 1): min((ceiling(n/num_splits)*(i)),n)])
  ##### FIRST STAGE #############
  
  
  treat<-my_data[,treat.name]
  outcome<-my_data[,outcome.name]
 
  s_g<<-50
  ### Residualize sales
  ### ... are settings of method.controls
  show("Partialling out controls from the treatment ...")
  t<-remove_wrapper(controls = controls,
                          target =treat,
                          method = method.treat,
                          num_splits=num_splits,INDS=INDS,...)
  treat<-t$res
  ### Residualize prices
  show("Partialling out controls from the outcome ...")
  s_g<<-100
  out<-remove_wrapper(controls = controls,
                         target = outcome,
                         method = method.outcome,
                         num_splits=num_splits,INDS=INDS,...)
  outcome<-out$res
  return(list(outcome = outcome, 
              treat = treat))
}

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
    ## apply remove_fixed_partition for each partition
    resid_list<-lapply(1:num_splits, function (i,...) {
      a<-remove_fixed_partition(inds=setdiff(1:n,INDS[[i]]),inds.est=INDS[[i]],controls,target,method = method,...)}
      
    )
    ## initalize res vector
    res<-target
    ## fill in resdulas by the result in l
    for (i in 1:num_splits){
      res[INDS[[i]],]<-as.numeric(resid_list[[i]]$res)
    }
    
    
  }
  return(list( res = res))
}
  
## this function creates hard-coded representation of categorical features and interacts them with lags
prepare_controls_for_first_stage<-function(my_data,first_stage_formula=NULL,...) {
  
  my_data<-mutate_at(my_data,.funs = funs(as.factor),
                     .vars = vars(matches("SiteName|ChannelName|Item|week|month|year"))) %>%
    mutate_at(.funs = funs(as.character),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(matches(paste(c("Level"),collapse="|")))) 
  ## default value of formula is to use all available price/sales lags interacted with all hierarchy levels
  if (is.null(first_stage_formula)) {
    ### get lag names
    lag.names<-grep("_lag",colnames(my_data),value=TRUE)
    ### formula for fixed effects representation
    if (categoryname=="Drinks") {
      level_names<-setdiff(grep("Level",colnames(my_data),value=TRUE  ),"Level1")
    } else {
      level_names<-grep("Level",colnames(my_data),value=TRUE  )
    }
    level_names<-setdiff(level_names,c("Level1_Name","month_name"))
    first_stage_formula<-as.formula(paste0("~",paste(c("SiteName","ChannelName","Item","week","month","year"),collapse="+"),
                            "+","(", paste(level_names,collapse="+"),")*(",
                            paste(lag.names,collapse="+"),
                            ")"))
  } 
  show("First Stage Partialling Out formula")
  show(first_stage_formula)
  ## controls in hard coded form
  fe_pdata<-model.matrix(first_stage_formula,data=my_data )%>%
    as_tibble 
  
  
    
  return(fe_pdata)
  
}
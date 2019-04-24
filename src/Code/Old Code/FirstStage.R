### First stage partialling out
first_stage<-function(data,
                      treat.name, outcome.name,control.name,
                      method.treat = gamlr,
                      method.outcome = gamlr,num_splits,INDS,...) {
  
  # create sample splitting pattern
  
  
  
  ### Residualize sales
  ### ... are settings of method.controls
  show("Partialling out treatments ...")
  
  treat<-data[,treat.name]
  outcome<-data[,outcome.name]
  controls<-data[,control.name]
  s_g<<-50
  t<-remove_wrapper(controls = controls,
                          target =treat,
                          method = method.treat,
                          num_splits=num_splits,INDS=INDS,...)
  treat<-t$res
  treat.error<-t$mse
  ### Residualize prices
  show("Partialling out outcome ...")
  
  s_g<<-100
  out<-remove_wrapper(controls = controls,
                         target = outcome,
                         method = method.outcome,
                         num_splits=num_splits,INDS=INDS,...)
  outcome<-out$res
  outcome.error<-out$mse

  
  return(list(outcome = outcome, 
              treat = treat,
              outcome.error =outcome.error,
              treat.error = treat.error,
              tfit.c  = t$fit.c,
              outfit.c = out$fit.c))
}


### generate data


for (j in 1:sample_sizes) {
  print(j)
  sample_size<-sample_sizes[j]
  
  
  
  for (seed in 1:N_rep) {
    
    
    set.seed(seed)
    inds<-sample(sample_size, 1:dim(simulated_data)[1],replace=TRUE)
    
    estimated_fs<-data.frame(treat=simulated_data$P.tilde,
                             outcome=(simulated_data$P.tilde*simulated_data$het_beta0+simulated_data$U) )
    
    myres<-second_stage(my_data=simulated_data[inds,],
                        fs=list(treat=estimated_fs$treat[inds],
                                outcome=estimated_fs$outcome[inds]),
                        controls=controls_tt[inds,],
                        second_stage_method_names = c("True",second_stage_method_names),
                        lambda_ridge=lambda_ridge[j],true_parameter=true_epsilon.own,
                        target_controls = controls,
                        nonzero_coords=1:num_coords[j])
    
    
    true_partial_effect<-myres$partial_effects$True
    myres_M1[,seed,j]<-apply(as.matrix(myres$partial_effects[,second_stage_method_names]),2,mean)
  }
  average[,j]<-apply(as.matrix(myres_M1[,1:seed,j]),1,mean,na.rm=TRUE)
  bias[,j]<-average[,j]- rep(mean(true_partial_effect),num_estimates)
  se[,j]<-apply(myres_M1[,1:seed,j],1,sd,na.rm=TRUE)
  rmse[,j]<-sqrt(bias[,j]^2+se[,j]^2)
  tstat<-(myres_M1[,1:seed,j]-array(rep(mean(true_partial_effect),num_estimates*(seed)),c(num_estimates,seed)))/array(rep(se[,j],seed),c(num_estimates,seed))
  
  rej.freq[,j]<-apply(abs(tstat)>qnorm(1-ci_alpha/2),1,mean,na.rm=TRUE)
  rej.freq[,j]<-sapply(rej.freq[,j],round,3)
  
  
}
}
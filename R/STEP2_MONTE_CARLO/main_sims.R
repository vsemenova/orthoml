rm(list=ls())
library(SimDesign)
library(tidyverse)
library(xtable)
library(gamlr)
library(glmnet)
library(grplasso)
library(hdm)

directoryname<-"/net/holyparkesec/data/tata/orthoml/"
#directoryname<-"/Users/virasemenova/Dropbox (MIT)/orthoml_draft/"
source(paste0(directoryname,"/R/utils_sims.R"))
source(paste0(directoryname,"/R/ss_methods.R"))
source(paste0(directoryname,"/R/FirstStage.R"))


Tmax=20



N=1000
TT=270
## number of first-stage (time-variant) controls
p=300
## number of second-stage (time-invariant) controls
p_het=80
##
rho<-0.9
#

sigma.p=0.01
sigma.u=0.03
delta_P<-0.05/(1:p)^(2)
delta_K<-rep(0,p_het)
delta_E<-0.05/(1:p)^(2)
alpha_price=c(0.9,0.1)
set.seed(888)
alpha_P = rnorm(N,sd=(log(N))^(3/2)/sqrt(N)/sqrt(TT))
alpha_E =rnorm(N,sd=(log(N))^(3/2)/sqrt(N)/sqrt(TT))


second_stage_method_names=c("GLasso","DebiasedGLasso","Lasso","DebiasedLasso","OLS","Oracle")
num_estimates<-length(second_stage_method_names)
sample_sizes<-c(200,300,500)/Tmax
N_rep=1000
ci_alpha=0.05
myres_M1<-array(0,c(num_estimates,N_rep,length(sample_sizes)))
myres_M2<-array(0,c(p_het,num_estimates,N_rep,length(sample_sizes)))

## save results here
average<-array(0,c(num_estimates,length(sample_sizes)))
bias<-array(0,c(num_estimates,length(sample_sizes)))
se<-array(0,c(num_estimates,length(sample_sizes)))
rmse<-array(0,c(num_estimates,length(sample_sizes)))
rej.freq<-array(0,c(num_estimates,length(sample_sizes)))
unif.rej.freq<-array(0,c(num_estimates,length(sample_sizes)))


true_epsilon.own<-epsilon.own<-3*c(-1,-0.5,-0.25,-0.125, rep(0,p_het-4) )


print(epsilon.own)
num_coords<-rep(p_het,length(sample_sizes))

num_coords<-sapply(num_coords,min,p)
lambda_array<-log(sample_sizes)*log(p)/sample_sizes


data<-generate_first_stage(seed=1,p=p,TT=TT,N=N,rho=rho,epsilon.own=epsilon.own,
                           alpha_price=alpha_price, alpha_P=alpha_P, alpha_E=alpha_E,delta_P = delta_P, 
                           delta_E=delta_E,delta_K=delta_K)

#data<-generate_second_stage(seed=1,N=N,epsilon.own=epsilon.own,TT=TT,sigma.p=0.1, sigma.u=0.03,rho=0.9)


Plag=data$P_lag
Plag2=data$P_lag2
controls=data$controls
het_controls=data$het_controls
P_het<-matrix(rep(data$P,p_het),ncol=p_het)*het_controls



covs.treat=as.matrix(cbind(Plag=Plag,Plag2=Plag2,
                           controls,
                           het_controls))


covs.outcome=as.matrix(cbind(P_het=P_het,controls))

for (j in 1:length(sample_sizes)) {
  
  for(seed in 1:N_rep)  {
    print(j)
    set.seed(seed)
    sample_size<-sample_sizes[j]
    cross_section_inds<-sample(1:N,sample_size,replace=TRUE)
    sample_inds<-c( )
    for (t in 1:Tmax) {
      sample_inds<-c(cross_section_inds+(t-1)*N,sample_inds)
    }
    
    
    
    lambda.outcome=lambda.treat=2*(log(length(sample_inds)))^(3/2)/sqrt(length(sample_inds))
    n_items<-length(cross_section_inds)
    
    ### unpenalize treatment interactions with logprice_lag and logprice_lag_2
    penalty.factor.treat=c(c(0,0),rep(1,p+p_het),rep(1/sqrt(n_items),n_items))
    
    penalty.factor.outcome<-c(0,rep(1,p+p_het-1), rep(1/sqrt(n_items),n_items))
    
    
    
    
    
    t.treat<-remove_wrapper(controls=cbind(covs.treat[sample_inds,],kronecker(rep(1,Tmax), diag(n_items) )),
                            target =data$P[sample_inds],
                            penalty.factor=penalty.factor.treat,
                            lambda=lambda.treat)
    
    res.treat<-t.treat$res
    
    
    
    if (TRUE) {
      
      t.outcome<-remove_wrapper(controls=cbind(covs.outcome[sample_inds,],kronecker(rep(1,Tmax), diag(n_items) )),
                                target =data$Q[sample_inds],
                                penalty.factor=penalty.factor.outcome,
                                lambda=lambda.outcome)
      
      res.outcome<-t.outcome$res + res.treat*(as.matrix(het_controls[sample_inds,])%*%as.numeric(t.outcome$fit.c$beta[1:p_het]))
    }
    
    
    
    myres<-second_stage(
      fs=list(treat= res.treat,
              outcome= res.outcome),
      controls=as.matrix(het_controls[sample_inds,]),
      second_stage_method_names = c("True",second_stage_method_names),
      lambda_ridge=lambda_array[j],true_parameter=true_epsilon.own,
      target_controls=as.matrix(het_controls),
      nonzero_coords=1:num_coords[j])
    
    true_partial_effect<-myres$partial_effects$True
    myres_M1[,seed,j]<-apply(myres$partial_effects[,second_stage_method_names],2,mean)
    myres_M2[,,seed,j]<-as.matrix(myres$estimates[,second_stage_method_names])
  }
  average[,j]<-apply(myres_M1[,1:seed,j],1,mean,na.rm=TRUE)
  bias[,j]<-average[,j]- rep(mean(true_partial_effect),num_estimates)
  se[,j]<-apply(myres_M1[,1:seed,j],1,sd,na.rm=TRUE)
  rmse[,j]<-sqrt(bias[,j]^2+se[,j]^2)
  tstat<-(myres_M1[,1:seed,j]-array(rep(mean(true_partial_effect),num_estimates*(seed)),c(num_estimates,seed)))/array(rep(se[,j],seed),c(num_estimates,seed))
  
  rej.freq[,j]<-apply(abs(tstat)>qnorm(1-ci_alpha/2),1,mean,na.rm=TRUE)
  rej.freq[,j]<-sapply(rej.freq[,j],round,3)
  
}



result<-cbind(t(bias),t(se),t(rmse),1-t(rej.freq))
colnames(result)<-rep(second_stage_method_names,4)
result<-apply(result,2,round,3)

boundnames<-c("Bias","SE","RMSE", "Coverage Rate")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{4}{c}{', boundnames, '}', collapse=''), '\\\\')


print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2]))),digits = 3),add.to.row=addtorow, include.colnames=F,
      include.rownames=T,digits=2)

write.table(
  print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2])))),add.to.row=addtorow, include.colnames=F,
        include.rownames=T),paste0(directoryname,"/Tables/main.txt"))




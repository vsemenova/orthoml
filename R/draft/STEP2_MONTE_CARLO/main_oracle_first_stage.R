### test finite sample performance when the first-stage is known

library(tidyverse)
library(xtable)
library(gamlr)
library(glmnet)
library(hdm)

directoryname<-"/n/tata/orthoml/"
source(paste0(directoryname,"/R/STEP2_MONTE_CARLO/utils.R"))
#### simulate data

## number of independent units
N=1705
## time periods
TT=270
## number of controls
p=50
rho<-0.9
sigma.p=0.01
sigma.u=0.03
lambda_ridge=c(0.040,0.040,0.010,0.001,0.001)


second_stage_method_names=c("OLS","Lasso")
num_estimates<-length(second_stage_method_names)
sample_sizes<-c(100,200,300,500,1000)
N_rep=100
ci_alpha=0.05
myres_M1<-array(0,c(num_estimates,N_rep,length(sample_sizes)))
average<-array(0,c(num_estimates,length(sample_sizes)))
bias<-array(0,c(num_estimates,length(sample_sizes)))
se<-array(0,c(num_estimates,length(sample_sizes)))
rmse<-array(0,c(num_estimates,length(sample_sizes)))
rej.freq<-array(0,c(num_estimates,length(sample_sizes)))

#### generate continuous covariates

std_controls<-matrix(rnorm(N*p),ncol=p)
cov_matrix<-toeplitz(rho^(c(0:(p-1))))
controls<-std_controls%*%chol(cov_matrix)
colnames(controls)<-paste0("control",c(1:p))

true_epsilon.own<-epsilon.own<-3*c(-1,-0.5,-0.25,-0.125,0.5^(5:dim(controls)[2]) )
het_beta0<-controls%*%epsilon.own


print(epsilon.own)
controls_tt<-kronecker(rep(1,TT),controls)
simulated_data<-generate_price_sales(seed=1,N=N,TT=TT,mydf=mydf,
                                     fs=fs,
                                     het_beta0=het_beta0)
p_x<-dim(simulated_data)[2]
simulated_data<-cbind(simulated_data,controls_tt)
colnames(simulated_data)[(p_x+1):(dim(simulated_data)[2])]<-paste0("controls_",1:p)
simulated_data<-simulated_data[simulated_data$week>=5,]


simulated_data$het_beta0<-sapply(simulated_data$het_beta0,round,3)
selected_rows<-unique(simulated_data$het_beta0)[1:4]
num_estimates<-length(second_stage_method_names)

for (j in 1:length(sample_sizes)) {
  print(j)
  
  for(seed in 1:N_rep)  {
    
    set.seed(seed)
    sample_size<-sample_sizes[j]
    
    inds<-sample(1:length(simulated_data$P.tilde),sample_size,replace=TRUE)
    
     
    fs<-data.frame(treat=simulated_data$P.tilde,
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


result<-cbind(t(bias),t(se),t(rmse),1-t(rej.freq))
rownames(result)<-c("$100$","$200$","$300$","$500$","1000")
colnames(result)<-rep(second_stage_method_names,1)
result<-apply(result,2,round,3)

boundnames<-c("Bias","St. Dev.", "Coverage Rate")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{3}{c}{', boundnames, '}', collapse=''), '\\\\')


print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2])))),add.to.row=addtorow, include.colnames=F,
      include.rownames=T,digits=3)



result<-cbind(t(bias),t(rmse),1-t(rej.freq))
rownames(result)<-c("$100$","$200$","$300$","$500$","1000")
colnames(result)<-rep(second_stage_method_names,3)
result<-apply(result,2,round,3)

}

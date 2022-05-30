rm(list=ls())
library(SimDesign)
library(tidyverse)
library(xtable)
library(gamlr)
library(glmnet)
library(hdm)

directoryname<-"/net/holyparkesec/data/tata/orthoml/"
source(paste0(directoryname,"/R/utils_sims.R"))
source(paste0(directoryname,"/R/ss_methods.R"))
source(paste0(directoryname,"/R/FirstStage.R"))

N=10000
TT=5
p=80
rho<-0.9

sigma.p=0.01
sigma.u=0.03
price_coef<-0.05/(1:p)^(2)
sales_coef<-0.05/(1:p)^(2)

second_stage_method_names=c("Lasso","DebiasedLasso","OLS","Oracle")
num_estimates<-length(second_stage_method_names)
sample_sizes<-c(200,300,500,1000,2000)
N_rep=100
ci_alpha=0.05
myres_M1<-array(0,c(num_estimates,N_rep,length(sample_sizes)))
myres_M2<-array(0,c(p,num_estimates,N_rep,length(sample_sizes)))

## save results here
average<-array(0,c(num_estimates,length(sample_sizes)))
bias<-array(0,c(num_estimates,length(sample_sizes)))
se<-array(0,c(num_estimates,length(sample_sizes)))
rmse<-array(0,c(num_estimates,length(sample_sizes)))
rej.freq<-array(0,c(num_estimates,length(sample_sizes)))
unif.rej.freq<-array(0,c(num_estimates,length(sample_sizes)))

std_controls<-matrix(rnorm(N*p),ncol=p)
cov_matrix<-toeplitz(rho^(c(0:(p-1))))
controls<-std_controls%*%chol(cov_matrix)
colnames(controls)<-paste0("control",c(1:p))



true_epsilon.own<-epsilon.own<-3*c(-1,-0.5,-0.25,-0.125,0.5^(5:dim(controls)[2]) )
het_beta0<-controls%*%epsilon.own
print(epsilon.own)
num_coords<-c(4,4,8,8,10,10,10)
num_coords<-sapply(num_coords,min,p)
lambda_array<-log(sample_sizes)*log(p)/sample_sizes

data<-generate_residuals2(seed=1,N=N,TT=TT,controls=controls)
data<-generate_panel_data(seed=1,N=N,TT=TT,controls=controls,price_coef=price_coef,sales_coef=sales_coef,
                    alpha_price=c(0.9,0.01),alpha_sales=c(0.88,0.01))

seed=1
#price_formula<-as.formula(paste0("P~",paste0("controls",c(1:p),collapse="+")))
#sales_formula<-as.formula(paste0("Q~",paste0("controls",c(1:p),collapse="+")))

price_formula<-as.formula(paste0("P~P_lag+P_lag2+",paste0("controls",c(1:p),collapse="+")))
sales_formula<-as.formula(paste0("Q~Q_lag+Q_lag2+",paste0("controls",c(1:p),collapse="+")))


for (j in 1:length(sample_sizes)) {
  for(seed in 1:N_rep)  {
    print(j)
    set.seed(seed)
    sample_size<-sample_sizes[j]
    sample_inds<-sample(1:N*TT,sample_size,replace=TRUE)
    
    
    lambda.outcome=lambda.treat=2*(log(length(sample_inds)))^(3/2)/sqrt(length(sample_inds))
    
    t.treat<-remove_wrapper(controls = data$controls[sample_inds,],
                            target =data$P[sample_inds],
                            penalty.factor=1,
                            lambda=lambda.treat)
    res.treat<-t.treat$res
    
    t.outcome<-remove_wrapper(controls = data$controls[sample_inds,],
                              target =data$Q[sample_inds],
                              penalty.factor=1,
                              lambda=lambda.outcome)
    res.outcome<-t.outcome$res
    
    
    myres<-second_stage(
      fs=list(treat= res.treat,
              outcome= res.outcome),
      controls=data$controls[sample_inds,],
      second_stage_method_names = c("True",second_stage_method_names),
      lambda_ridge=lambda_array[j],true_parameter=true_epsilon.own,
      target_controls=controls,
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
rownames(result)<-c("$200$","$300$","$500$","$1000$","$2000$")
colnames(result)<-rep(second_stage_method_names,4)
result<-apply(result,2,round,3)

boundnames<-c("Bias","SE","RMSE", "Coverage Rate")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{4}{c}{', boundnames, '}', collapse=''), '\\\\')


print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2]))),digits = 2),add.to.row=addtorow, include.colnames=F,
      include.rownames=T,digits=2)

write.table(
  print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2])))),add.to.row=addtorow, include.colnames=F,
        include.rownames=T),paste0(directoryname,"/Tables/panel.txt"))


setwd("/net/holyparkesec/data/tata/orthoml/draft/R_old/STEP2_MONTE_CARLO")
directoryname<-"/net/holyparkesec/data/tata/orthoml/draft/R_old/"
source(paste0(directoryname,"utils_sims.R"))
source(paste0(directoryname,"ss_methods.R"))
source(paste0(directoryname,"auxiliary.R"))

library(SimDesign)
library(tidyverse)
library(xtable)
library(gamlr)
library(glmnet)
library(hdm)


#### READ IN DATA and ESTIMATE FIRST STAGE PARAMTERS ##

directoryname<-"/net/holyparkesec/data/tata/orthoml/"
my_data<-read.csv(paste0(directoryname,"/processed_data/Dairy.csv"))
colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
my_data$RowID<-as.character(my_data$RowID)
TT= dim(unique(my_data[,c("week","year")]))[1]
N=floor(dim(my_data)[1]/TT)
selected_inds<-sample(1:dim(my_data)[1],N,replace=FALSE)
inds_train<-(1:dim(my_data)[1])[(my_data$year==2013 & my_data$week<=8) + (my_data$year==2012)>=1]
inds_test<-setdiff( (1:dim(my_data)[1]),inds_train)
my_data$logprice_lag_fd<-my_data$logprice_lag - my_data$logprice_lag_2
my_data$logsales_lag_fd<-my_data$logsales_lag - my_data$logsales_lag_2

my_data$logprice_lag_fd_2<-my_data$logprice_lag_2 - my_data$logprice_lag_3
my_data$logsales_lag_fd_2<-my_data$logsales_lag_2 - my_data$logsales_lag_3
cal_data<-calibrate_data(my_data,selected_inds)
mydf<-cal_data$mydf
my_data<-cal_data$mydata
### simulation paramters
N=1705
TT=270
p=80
rho<-0.9
sigma.p=0.01
sigma.u=0.03
second_stage_method_names=c("Lasso","DebiasedLasso","OLS","Oracle")
num_estimates<-length(second_stage_method_names)
sample_sizes<-c(90,100,200,300,500,1000,2000)
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

##  

std_controls<-matrix(rnorm(N*p),ncol=p)
cov_matrix<-toeplitz(rho^(c(0:(p-1))))
controls<-std_controls%*%chol(cov_matrix)
colnames(controls)<-paste0("control",c(1:p))
price_coef<-0.05/(1:p)^(2)
sales_coef<-0.05/(1:p)^(2)
set.seed(1)
price_unit_effect<-0.05*rnorm(N)
sales_unit_effect<-0.05*rnorm(N)
simulated_price_fixed_effect<-predict(price.fit,mydf[,c("logprice","SiteName","ChannelName")])+price_unit_effect
simulated_sales_fixed_effect<-predict(sales.fit,mydf[,c("logsales","SiteName","ChannelName","Item","Level1",  "Level2" , "Level3" , "Level4",  "Level5")])+sales_unit_effect


my_data$logprice_lag_fd_2<-my_data$logprice_lag_2 - my_data$logprice_lag_3
my_data$logsales_lag_fd_2<-my_data$logsales_lag_2 - my_data$logsales_lag_3
cal_data<-calibrate_data(my_data,selected_inds)
mydf<-cal_data$mydf
my_data<-cal_data$mydata

simulated_first_stage_price_formula<-as.formula(paste0("logprice_1df~",paste(c("Level1"),collapse="+"),"*(",
                                                       paste(c("logprice_lag_fd","logprice_lag_fd_2"),collapse="+"),
                                                       ")"))
simulated_first_stage_sales_formula<-as.formula(paste0("logsales_1df~",paste(c("Level1"),collapse="+"),"*(",
                                                       paste(c("logprice_lag_fd","logprice_lag_fd_2","logsales_lag_fd","logsales_lag_fd_2"),collapse="+"),
                                                       ")"))


fs<-first_stage_1df(treat=my_data[,"logprice_1df"],
                    outcome=my_data[,"logsales_1df"],
                    mydata=my_data,
                    first_stage_price_formula=simulated_first_stage_price_formula,
                    first_stage_sales_formula=simulated_first_stage_sales_formula,
                    inds_train=inds_train
)

price.fit<-lm(logprice~., mydf[,c("logprice","SiteName","ChannelName")])
sales.fit<-rlasso(logsales~., mydf[,c("logsales","SiteName","ChannelName","Item","Level1",  "Level2" , "Level3" , "Level4",  "Level5")])


std_controls<-matrix(rnorm(N*p),ncol=p)
cov_matrix<-toeplitz(rho^(c(0:(p-1))))
controls<-std_controls%*%chol(cov_matrix)
colnames(controls)<-paste0("control",c(1:p))
price_coef<-0.05/(1:p)^(2)
sales_coef<-0.05/(1:p)^(2)
set.seed(1)
price_unit_effect<-0.05*rnorm(N)
sales_unit_effect<-0.05*rnorm(N)
simulated_price_fixed_effect<-predict(price.fit,mydf[,c("logprice","SiteName","ChannelName")])+price_unit_effect
simulated_sales_fixed_effect<-sales_unit_effect


true_epsilon.own<-epsilon.own<-3*c(-1,-0.5,-0.25,-0.125,0.5^(5:dim(controls)[2]) )
het_beta0<-controls%*%epsilon.own
#epsilon.own<- (-1)*(0.5^(1:dim(het_characteristics_present)[2]))
print(epsilon.own)
controls_tt<-kronecker(rep(1,TT),controls)
simulated_data<-generate_price_sales(seed=1,N=N,TT=TT,mydf=mydf,
                                     fs=fs,
                                     het_beta0=het_beta0,
                                     sales_fixed_effect=simulated_sales_fixed_effect,
                                     price_fixed_effect=simulated_price_fixed_effect)


simulated_data$logprice_lag_fd<-simulated_data$logprice_lag - simulated_data$logprice_lag_2
simulated_data$logsales_lag_fd<-simulated_data$logsales_lag - simulated_data$logsales_lag_2

simulated_data$logprice_lag_fd_2<-simulated_data$logprice_lag_2 - simulated_data$logprice_lag_3
simulated_data$logsales_lag_fd_2<-simulated_data$logsales_lag_2 - simulated_data$logsales_lag_3


simulated_data<-simulated_data[simulated_data$week>=7,]
num_coords<-c(4,4,8,8,10,10,10)
lambda_array<-log(sample_sizes)*log(p)/sample_sizes
B_rep<-5000

for (j in 1:7) {
  for(seed in 1:N_rep)  {
    print(j)
    set.seed(seed)
    sample_size<-sample_sizes[j]
    sample_inds<-sample(1:dim(simulated_data)[1],2*sample_size,replace=TRUE)
    inds_train<-sample_inds[1:sample_size]
    inds_test<-sample_inds[(1+sample_size):(2*sample_size)]
  
  
    estimated_fs<-first_stage_1df(treat=simulated_data[,"logprice_1df"],
                                outcome=simulated_data[,"logsales_1df"],
                                mydata=simulated_data,
                                first_stage_price_formula = simulated_first_stage_price_formula,
                                first_stage_sales_formula = simulated_first_stage_sales_formula,
                                inds_train =  inds_train
    )
  
  
  
  
    myres<-second_stage(my_data=simulated_data[inds_test,],
                      fs=list(treat=estimated_fs$treat[inds_test],
                              outcome=estimated_fs$outcome[inds_test]),
                      controls=controls_tt[inds_test,],
                      second_stage_method_names = c("True",second_stage_method_names),
                      lambda_ridge=lambda_array[j],true_parameter=true_epsilon.own,
                      target_controls = controls,
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
rm(list=ls())
##### SIMULATION SETUP
library(tidyverse)
library(xtable)
library(gamlr)
library(glmnet)
library(hdm)

directoryname<-"/n/tata/orthoml/"
source(paste0(directoryname,"/R/utils_for_simulations.R"))
source(paste0(directoryname,"/R/ss_methods.R"))
source(paste0(directoryname,"/R/auxiliary.R"))
#### READ IN DATA and ESTIMATE FIRST STAGE PARAMTERS ##

my_data<-read.csv(paste0(directoryname,"/processed_data/Protein.csv"))
colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
my_data$RowID<-as.character(my_data$RowID)
TT= dim(unique(my_data[,c("week","year")]))[1]
N=floor(dim(my_data)[1]/TT)
selected_inds<-sample(1:dim(my_data)[1],N,replace=FALSE)
inds_train<-(1:dim(my_data)[1])[(my_data$year==2013 & my_data$week<=8) + (my_data$year==2012)>=1]
inds_test<-setdiff( (1:dim(my_data)[1]),inds_train)
cal_data<-calibrate_data(my_data,selected_inds)
mydf<-cal_data$mydf
my_data<-cal_data$mydata

simulated_first_stage_price_formula<-as.formula(paste0("logprice_1df~logprice_lag+logprice_lag_2+(",paste(c("Level1"),collapse="+"),"):(",
                                                       paste(c("logprice_lag","logprice_lag_2"),collapse="+"),
                                                       ")"))
simulated_first_stage_sales_formula<-as.formula(paste0("logsales_1df~logprice_lag+logprice_lag_2+logsales_lag+logsales_lag_2+(",paste(c("Level1"),collapse="+"),"):(",
                                                       paste(c("logprice_lag","logprice_lag_2","logsales_lag","logsales_lag_2"),collapse="+"),
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



second_stage_method_names=c("Lasso","DebiasedLasso","Oracle")
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

#### SIMULATE DATA ###

N=1705
TT=270
p=50
rho<-0.9
sigma.p=0.01
sigma.u=0.03
#### generate continuous covariates



std_controls<-matrix(rnorm(N*p),ncol=p)
cov_matrix<-toeplitz(rho^(c(0:(p-1))))
controls<-std_controls%*%chol(cov_matrix)
colnames(controls)<-paste0("control",c(1:p))
#price_coef<-0.05/(1:p)^(2)
#sales_coef<-0.05/(1:p)^(2)
#simulated_price_fixed_effect<-predict(price.fit,mydf[,c("logprice","SiteName","ChannelName")])+controls%*%price_coef
#simulated_sales_fixed_effect<-predict(sales.fit,mydf[,c("logsales","SiteName","ChannelName","Item","Level1",  "Level2" , "Level3" , "Level4",  "Level5")])+controls%*%sales_coef
simulated_price_fixed_effect<-0
simulated_sales_fixed_effect<-0

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
p_x<-dim(simulated_data)[2]
simulated_data<-cbind(simulated_data,controls_tt)
colnames(simulated_data)[(p_x+1):(dim(simulated_data)[2])]<-paste0("controls_",1:p)
simulated_data<-simulated_data[simulated_data$week>=5,]


simulated_data$het_beta0<-sapply(simulated_data$het_beta0,round,3)
selected_rows<-unique(simulated_data$het_beta0)[1:4]

if (FALSE) {
  first_stage_price_formula<-as.formula(paste0("logprice~(",paste(c("SiteName","ChannelName"),collapse="+"),")+(",paste(c("Level1"),collapse="+"),")*(",
                                               paste(c("logprice_lag","logprice_lag_2"),collapse="+"),
                                               ")+(",paste0(paste0("controls_",1:p),collapse="+"),")"
  ))
  
  first_stage_sales_formula<-as.formula(paste0("~(",paste(c("SiteName","ChannelName","Item"),collapse="+"),")+(",paste(c("Level1","Level2","Level3","Level4"),collapse="+"),")*(",
                                               paste(c("logprice_lag","logprice_lag_2","logsales_lag","logsales_lag_2"),collapse="+"),
                                               ")+(",paste0(paste0("controls_",1:p),collapse="+"),")"
  ))
  
}

first_stage_price_formula<-as.formula(paste0("logprice~(",paste(c("SiteName","ChannelName"),collapse="+"),")+(",paste(c("Level1"),collapse="+"),")*(",
                                             paste(c("logprice_lag","logprice_lag_2"),collapse="+")
                                              ))


first_stage_sales_formula<-as.formula(paste0("~(",paste(c("SiteName","ChannelName","Item"),collapse="+"),")+(",paste(c("Level1","Level2","Level3","Level4"),collapse="+"),")*(",
                                             paste(c("logprice_lag","logprice_lag_2","logsales_lag","logsales_lag_2"),collapse="+")))



num_coords<-c(4,4,4,4,4,4)
lambda_ridge<-c(0.04,0.04,0.01,0.001)
for (j in 1:length(sample_sizes)) {
  for(seed in 1:N_rep)  {
    print(j)
    set.seed(seed)
    sample_size<-sample_sizes[j]
 
  
      
    sample_weeks<-sample(1:max(simulated_data$week),2*(sample_size/2),replace=TRUE)
    inds_train<- (1:dim(simulated_data)[1])[simulated_data$week %in%  sample_weeks[1:(length(  sample_weeks)/2)] & simulated_data$het_beta0 %in% selected_rows]
    inds_test<- (1:dim(simulated_data)[1])[simulated_data$week %in%  sample_weeks[(length(  sample_weeks)/2+1):(2*length(  sample_weeks)/2)] & simulated_data$het_beta0 %in% selected_rows]
    
   
    if (FALSE) {
      estimated_fs<-first_stage(treat=simulated_data[,"logprice"],
                                    outcome=simulated_data[,"logsales"],
                                    mydata=simulated_data,
                                    first_stage_price_formula = first_stage_price_formula,
                                    first_stage_sales_formula = first_stage_sales_formula,
                                    inds_train =  inds_train,inds_test= inds_test)
      
    }
    if (TRUE) {
      estimated_fs<-data.frame(treat=simulated_data$P.tilde[inds_test],
                                outcome=(simulated_data$P.tilde*simulated_data$het_beta0+simulated_data$U)[inds_test] )
    }

    
    myres<-second_stage(my_data=simulated_data[inds_test,],
                        fs=list(treat=estimated_fs$treat,
                                outcome=estimated_fs$outcome),
                        controls=controls_tt[inds_test,],
                        second_stage_method_names = c("True",second_stage_method_names),
                        lambda_ridge=lambda_ridge[j],true_parameter=true_epsilon.own,
                        target_controls = controls,
                        nonzero_coords=1:num_coords[j])
    
    
    true_partial_effect<-myres$partial_effects$True
    myres_M1[,seed,j]<-apply(myres$partial_effects[,second_stage_method_names],2,mean)
    #myres_M2[,seed,j]<-apply(myres$estimates[1,second_stage_method_names],2,mean)
    
  }
  
  average[,j]<-apply(myres_M1[,1:seed,j],1,mean,na.rm=TRUE)
  bias[,j]<-average[,j]- rep(mean(true_partial_effect),num_estimates)
  se[,j]<-apply(myres_M1[,1:seed,j],1,sd,na.rm=TRUE)
  rmse[,j]<-sqrt(bias[,j]^2+se[,j]^2)
  tstat<-(myres_M1[,1:seed,j]-array(rep(mean(true_partial_effect),num_estimates*(seed)),c(num_estimates,seed)))/array(rep(se[,j],seed),c(num_estimates,seed))
  
  rej.freq[,j]<-apply(abs(tstat)>qnorm(1-ci_alpha/2),1,mean,na.rm=TRUE)
  rej.freq[,j]<-sapply(rej.freq[,j],round,3)
  
  
}




result<-cbind(t(bias),t(rmse),1-t(rej.freq))
rownames(result)<-c("$100$","$200$","$300$","$500$","1000")
colnames(result)<-rep(second_stage_method_names,3)
result<-apply(result,2,round,3)

boundnames<-c("Bias","St. Dev.", "Coverage Rate")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{3}{c}{', boundnames, '}', collapse=''), '\\\\')


print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2])))),add.to.row=addtorow, include.colnames=F,
      include.rownames=T,digits=3)

write.table(
  print(xtable(result,align =paste0(c("c|",rep("c",dim(result)[2])))),add.to.row=addtorow, include.colnames=F,
        include.rownames=T),paste0(directoryname,"/Tables/sims_level.txt"))


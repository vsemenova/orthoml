fs_jjfoods<-function(num_splits=2,run_fs=FALSE) {
# Read in data
my_data<-as_tibble(read.csv(filename))
# Select categories at top level
my_data<-filter(my_data,Level1 %in% level1_inds)
# Add lags, filter +/- Inf 
my_data<-my_data%>% filter(abs(logprice)<Inf && abs(logmove)<Inf) 
# Select present products 
# A present product has average logmove_{t-1:t-4}
# >= 2

my_data<- my_data %>%
  group_by(SiteName,ChannelName,Item) %>%
  mutate_at(.funs=funs(lag,lag_2,lag_3,lag_4),
            .vars = vars(matches(paste(lag.name,collapse="|")))) %>%
  na.omit(.) %>%
  ungroup() %>%
  # Set key.name to be type factor
  mutate_at(.funs = funs(as.factor),
            .vars = vars(matches(paste(c(key.name,"week","year","month"),collapse="|")))) %>%
  mutate_at(.funs = funs(as.character),
            .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
  mutate_at(.funs = funs(as.factor),
            .vars = vars(matches(paste(c("Level"),collapse="|")))) %>%
  mutate(average_logmove = 1/4*(logmove_lag+logmove_lag_2+logmove_lag_3+logmove_lag_4)) %>%
  filter(average_logmove >2) %>%
  #Add log price ratio log(pt/p(t-1))
  mutate(my_data,logpr=logprice-logprice_lag) 

show("Constructed lags")

# Add interaction terms with lags
lag.names<-grep("_lag",colnames(my_data),value=TRUE)
show(lag.names)
form<-as.formula(paste0("~",paste(c("SiteName","ChannelName","Item","week","month","year"),collapse="+"),
                        "+","(", paste(grep("Level",colnames(my_data),value=TRUE  ),collapse="+"),")*(",
                        paste(lag.names,collapse="+"),
                        ")"))

fe_pdata<-model.matrix(form,data=my_data )%>%
  as_tibble 
fe_pdata<-  cbind(fe_pdata,as.matrix(my_data[,c("logsales","logprice","logpr")])) 
## Split the sample


control.name = setdiff(colnames(fe_pdata),c("logprice","logsales","logpr"))
show(control.name)
## Summary stats about fe_pdata
fe_pdata<-as.matrix(fe_pdata)
inds<-!rowSums(!is.finite(fe_pdata))
fe_pdata<-fe_pdata[inds,]
my_data<-my_data[inds,]
n<-dim(fe_pdata)[1]
set.seed(888)
indices<-sample(1:n,size=n,replace=FALSE)
INDS<-lapply(1:num_splits, function (i) indices[(ceiling(n/num_splits)*(i-1) + 1): min((ceiling(n/num_splits)*(i)),n)])

# if run first stage from scracth
if (run_fs) {
  fs<-first_stage(data = as.data.frame(fe_pdata),
                  treat.name = treat.name,
                  outcome.name =outcome.name,
                  control.name = control.name,
                  method.treat = method.treat,
                  method.outcome = method.outcome,
                  num_splits=2,INDS = INDS)
  results<-cbind(fs$treat,fs$outcome)
  colnames(results)<-c("price_res","sales_res")
  treat_res<-fs$treat
  outcome_res<-fs$outcome
  write.csv(file=outfile,x=results)
  
  
  ## Selected first stage coefs for prices
  prices<-round(as.numeric(coef(fs$tfit.c[[1]][[1]], s="min"))[-1],3)
  res<-cbind(colnames(fe_pdata)[which(prices!=0)],prices[which(prices!=0)])
  colnames(res)<-c("Name","Value")
  write(file=paste0("FS Results/FirstStagePrices",as.character(level1_inds[1]),".txt"), res)
  png(paste0("FS Results/FirstStagePrices",as.character(level1_inds[1]),".png"))
  plot(fs$tfit.c[[1]][[1]])
  dev.off()
  
  
  # Sales model
  # selects everything
  # log lambda = -6
  sales<-round(as.numeric(coef(fs$outfit.c[[1]][[1]], s="min"))[-1],3)
  sales[sales!=0]
  
  res<-cbind(colnames(fe_pdata)[which(sales!=0)],sales[which(sales!=0)])
  colnames(res)<-c("Name","Value")
  write(file=paste0("FS Results/FirstStageSales",as.character(level1_inds[1]),".txt"), res)
  png(paste0("FS Results/FirstStageSales",as.character(level1_inds[1]),".png"))
  plot(fs$outfit.c[[1]][[1]])
  dev.off()
} else {
  results<-read.csv(file=outfile,row.names=1)
  treat_res<-results[,1]
  outcome_res<-results[,2]
}


# Construct proper hierarchy 
# Make sure at each level there is an excluded category with min_cat_size
# Exclude category before running Lasso/Ridge/OLS
#het<-"^(?=.*Level5|Level4|Level3|Level2|Level1)(?!.*lag)"
#het.names<-grep(het,colnames(fe_data),value=TRUE,perl=TRUE)
#by_het<-fe_data[,het.names]
#by_het<-by_het[,sort(het.names,decreasing = TRUE)]

# select columns manually
if (level1_inds[1]==7) {
  colnames(fe_pdata)[1]<-"Level17"
  
}

return(list(treat_res = treat_res,outcome_res = outcome_res, fe_pdata=fe_pdata,my_data=my_data))
}


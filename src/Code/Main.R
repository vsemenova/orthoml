## input arguments

#@ categoryname  product category: Drinks, Protein, Non_Edibles, Snacks

#@ method.treat   method for partialling out controls from treatment
#@ method.outcome method for partialling out controls from outcome
#@ outpath        pathname to save treatment/outcome residuals
#@ num_splits     number of splits to train model (minimum 2)
#... any optional arguments for the method
# read in data
# 

main<-function(categoryname,het.name,
               method.treat=cv.gamlr,
               method.outcome = cv.gamlr,
               num_splits=2,
               seed=888,
               subset.name="",
               grouping_level=NULL,
               run_fs=TRUE,...) {
  
  ###### PREPARING log (PRICE) and log (SALES) for PREDICTION PROBLEM
  
  ## get fixed effects representation of categorical variables Level1:Level5
  ### read in data
  
  # Dairy file is too large for gihub; split into 2 halfs
  if (categoryname =="Dairy"){
    my_data1<-read.csv(paste0(directoryname,"/Data/AggDataDairyPart1.csv"))
    my_data2<-read.csv(paste0(directoryname,"/Data/AggDataDairyPart2.csv"))
    my_data<-rbind(my_data1,my_data2)
  } else {
    my_data<-read.csv(paste0(directoryname,"/Data/AggData",categoryname,".csv"))
  }
 
  colnames(my_data)[colnames(my_data)=="X"]<-"RowID"
  my_data$RowID<-as.character(my_data$RowID)
  ## treatment is logprice - logprice_lag = log (price/price_lag)=log(priceratio)
  my_data$logpr<-my_data$logprice-my_data$logprice_lag
  ## if replicate the first stage
if (run_fs) {
  ##### FIRST STAGE #############
  fs<-first_stage(my_data=my_data,treat.name = "logpr",
                  outcome.name ="logsales",
                  method.treat = method.treat,
                  method.outcome = method.outcome,
                  num_splits=2,...)
  print(paste0("Number of Nas in the first stage", as.character(sum(is.na(fs)))))
  write.csv(fs,paste0(directoryname,"/Output/FirstStage",categoryname,".csv"))
} else {
  # otherwise, load residuals
  fs<-read.csv(paste0(directoryname,"/Output/FirstStage",categoryname,".csv"))
 
} # make sure there are no NAs in the residuals
  if (sum(is.na(fs$treat) + is.na(fs$outcome))>0) {
    stop("NAs in the first stage. Check computation of the residuals")
  }
  ######## SECOND STAGE ############
  
  ## take target set of rows
  subset_inds<-1:dim(my_data)[1]
  if (subset.name=="Sodas") {
    subset_inds<-my_data$Level1_Name=="Sodas"
  }
  if (subset.name == "Water") {
    subset_inds<-my_data$Level1_Name=="Water"
  }
    
  
  # ss is a data.frame with 2 columns = estimate  st.error.hat for each method in second_stage_method_names
  # each estimate corresponds to each unique category/produdct determined by heterog_pattern
  ss<-second_stage(my_data=my_data[subset_inds,],
                   fs=fs[subset_inds,],categoryname=categoryname,het.name=het.name, ...)
  
  
  ## Code below is specific to Figures 
  
  if (is.null(grouping_level)) {
    # this branch holds for Figure 3 and Figure 4
    # do box and whisker plot only of Est and CI's 
    # assign xbreaks by joining to my_data
    
   
    if (het.name=="month_name") {
      ss$OLS<-left_join(ss$OLS,select(my_data,one_of("RowID",het.name,"month")),by=c("RowID"="RowID")) 
      colnames(ss$OLS)[ colnames(ss$OLS)==het.name]<-"xbreaks"
      ss$OLS<-ss$OLS[order(ss$OLS$month),]
    } else {
      if (categoryname == "Drinks") {
        ss$OLS<-left_join(ss$OLS,select(my_data,one_of("RowID",het.name,"Level2")),by=c("RowID"="RowID")) 
        colnames(ss$OLS)[ colnames(ss$OLS)==het.name]<-"xbreaks"
        ss$OLS<-ss$OLS[order(ss$OLS$Level2),]
      } else {
        ss$OLS<-left_join(ss$OLS,select(my_data,one_of("RowID",het.name,"Level1")),by=c("RowID"="RowID")) 
        colnames(ss$OLS)[ colnames(ss$OLS)==het.name]<-"xbreaks"
        ss$OLS<-ss$OLS[order(ss$OLS$Level1),]
      }
    
    }
    boxwhisker(data=ss$OLS,subset.name=subset.name,het.name=het.name,...)
  } else {
    # This branch holds for Figure 5
   
    ## need to collapse the list with 3 estimators into (no standard errors) into a dataframe
    ss_estimates<-left_join(data.frame( Lasso=ss$Lasso$est,
                             Ridge=ss$Ridge$est,
                             OLS = ss$OLS$est,
                             RowID=ss$OLS$RowID), select(my_data,one_of("RowID",grouping_level)),
                            by = c("RowID"="RowID")) %>%
      select(.,-RowID)
    # plot histogram of estimates for each second stage estimator
    # group points by colour determined by grouping_level
    hist_coef(data=ss_estimates,sample_size=dim(my_data)[1],
              grouping_level=grouping_level,
              categoryname= categoryname,...
    )
  }
}
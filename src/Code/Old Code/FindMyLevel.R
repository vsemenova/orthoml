## Step 1
# Plot  OLS and save
# Plot histogram of coefs
# Mark by color the grouping by higher levle
# Do that for week-> month
# Do that for level2>>level1
# Document some precisely estimated elasticities

m<-c(rep(1,4),1,rep(2,4),rep(3,5),rep(4,4),
     rep(5,5),rep(6,4),rep(7,4),rep(8,4),rep(9,4),
     rep(10,5),rep(11,4),rep(12,5))
weekmonth<-cbind(1:53,m)

grouping<-function(bottomlevel,grouplevel,fe_data) {
  het=paste0("(?=.*",bottomlevel,"|",grouplevel,")(?!.*lag)")
  fe_data<-fe_data[,grep(het,colnames(fe_data),perl=TRUE)]
  het1=paste0("(?=.*",grouplevel,")(?!.*lag)")
  het.names1<-grep(het1,colnames(fe_data),value=TRUE,perl=TRUE)
 # het.names2<-grep(het2,colnames(fe_data),value=TRUE,perl=TRUE)
  #by_het2<-fe_data[,het.names2]
  by_het2<-fe_data
  excl<-apply(by_het2,1,sum)==0
  if (sum(excl)>10){
    het.names2<-c(het.names2,"Level2N")
    by_het2<-cbind(Level2N=excl,by_het2)
  }
  
  by_het1<-fe_data[,het.names1]
  
  find_my_level<-t(by_het2)%*%by_het1
  #x<-colnames(find_my_level)[find_my_level[value,]>0]
  return( find_my_level)
  
}


get_my_level<-function(arg,map) {
  x<-colnames(map)[map[arg,]>0]
  if (length(x)==0||length(x)>1){
    x<-"Level1None"
  }
  return( x)
  
}

find_my_cat<-function(df){
  
  x<-as.factor(apply(df,1,function(foo){return(names(df)[which.max(foo)])}))
  
}

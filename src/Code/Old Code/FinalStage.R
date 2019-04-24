get_est<-function(htheta,subset=1:length(htheta),bh) {
  est<-as.numeric(htheta)
  #by_het<-by_het[,sort(colnames(by_het),decreasing = TRUE)]
 
  # This is always true since bh is sorted in the order of est
  imsens<-bh%*%est[subset]
  if (!is.null(attr(htheta,"White")) ) {
    A.het<-attr(htheta,"White")
    st.error.hat<-sqrt(diag(bh%*%A.het[subset,subset]%*%t(bh)))
    data<-data.frame(est=imsens,st.error.hat=st.error.hat)
  } else {
    st.error.hat<-NULL
    data<-data.frame(Lasso.est=imsens)
    colnames(data)<-"Lasso.est"
  }
  # Sort decreasing order to back out names
  bh<-bh[,sort(colnames(bh),decreasing = TRUE)]
  
  names<-colnames(bh)[apply(bh,1,which.max)]
  inds<-which(duplicated(names))
  if (length(inds)>0) {
    stop("Duplicated codes. Need to clean data")
    # Matt's coding was not a tree
    # he had same Levelk codes for different Level(k-1) codes
  }
  rownames(data)<-names
  
  
  ord<-as.numeric(gsub("Level1","",rownames(data)))
  ord<-order(ord)
  if (sum(is.na(ord))==0) {
    data<-as.data.frame(data[ord,])
    if (is.null(st.error.hat)) {
      colnames(data)<-"Lasso.est"
    }
    
  }
  show(colnames(data))
  return(data)
}





recover_bottom_est<-function(het,het_treat,by_het,res_ss,model_name="Own", htheta=NULL) {
  if (het=="month"|| het=="week") {
    est<-as.numeric(htheta)
    st.error.hat<-attr(htheta,"st.error.hat")
    data<-data.frame(OLS.est=est,OLS.st.error.hat=st.error.hat)
    rownames(data)<-colnames(het_treat)
  }else {
    
    cp_inds<-grep("cp",colnames(het_treat))
    op_inds<-setdiff(1:length(colnames(het_treat)),cp_inds)
    
   
    unik<-!(duplicated(by_het))
    bh<-by_het[unik,]
    if (length(cp_inds)==0){
      data<-lapply(res_ss,get_est,bh=bh)
      data<-as.data.frame(data)
      data_cp<-NULL
    
  } else {
    data_op<-lapply(res_ss,get_est,subset=op_inds,bh=bh)
    data<-as.data.frame(data_op)
    }
   
  
  
} 
  out<-as.matrix(data)
  tab<-xtable(out, caption = "Average Own Elasticity", label = paste0(het,level1_inds[1]),
              digits=3)
  
  
  print(tab,file=outtexname,append=TRUE, 
        include.rownames=TRUE,
        include.colnames=TRUE,digits=3)
  outcsvname<-paste0(textdirectory,model_name,colnames(het_treat)[grep(het,colnames(het_treat),perl=TRUE)][1],as.character(level1_inds)[1],".csv")
  
  write.csv(format(tab,digits=3) ,file=outcsvname)
  return(data)
}


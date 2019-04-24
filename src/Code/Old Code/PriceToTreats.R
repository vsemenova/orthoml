construct_treat<-function(het,price_res,fe_data,perl=TRUE,tol = 0.01,subset=NULL,cp=FALSE,res_fs=NULL) {
  
  # Take relevant observations
  if (!is.null(subset)) {
    fe_data<-fe_data[subset,]
    price_res<-price_res[subset]
    my_data<-res_fs$my_data[subset,]
  } else {
    my_data<-res_fs$my_data
  }
  
  het.names<-grep(het,colnames(fe_data),value=TRUE,perl=perl)
  by_het<-fe_data[,het.names]
  
  

  
  # Sort Levels from bottom to top
 
  # Make sure no excluded categories
  excl<-apply(by_het,1,sum)==0
  if (!cp) {
  if (sum(excl)>10){
    by_het<-cbind(Level1100001=excl,by_het)
    if (het=="month"|| het=="week") {
    } else {
      # sort names in decreasing order
      
        ord<-as.numeric(gsub("Level1","",colnames(by_het)))
        if (sum(is.na(ord))==0) {
          # missing category dropped due to model.matrix
          missing_cat<-setdiff(level1_inds,ord)[1]
          # this is exactly excluded category we namedLevel1100001 before
          colnames(by_het)[1]<-paste0("Level1",missing_cat)
          ord<-as.numeric(gsub("Level1","",colnames(by_het)))
          by_het<-by_het[,order(ord)]
      }
     
      }
     
    }
    
  }
  
  # Interact binary categories with treatment
  het_treat<-matrix(rep(price_res,dim(by_het)[2]),ncol=dim(by_het)[2])*by_het
  
  # Make sure every hierarchy branch has an excluded category

  het_treat_res<-clean_data(het_treat,by_het)
  het_treat<-het_treat_res$het_treat
  by_het<-het_treat_res$by_het
  
  if(cp==TRUE && !is.null(res_fs)) {
    by_het<-by_het[,sort(colnames(by_het),decreasing=TRUE)]
    if (level1_inds[1]!=7) {
      by_het<-cbind(by_het,Level01=1)
      
    }
    het_treat<-matrix(rep(price_res,dim(by_het)[2]),ncol=dim(by_het)[2])*by_het
    
    tib<-as_tibble(by_het)
    tib$ownprice<-price_res
    tibcpop<-tib%>%
      bind_cols(., dplyr::select(my_data,SiteName,ChannelName,SalesDate)) 
    
    tibcpop<-Reduce(crossprice_group,as.list(colnames(by_het)),init = tibcpop) 
    tibcpop<-as.matrix(dplyr::select(tibcpop,starts_with("cp"))) 
    het_treat<-cbind(het_treat,as.matrix(tibcpop))
    
  }
  
  # Exclude  categories with no variation
  inds_tol<-diag(t(het_treat)%*%het_treat)>tol
  het_treat<-het_treat[,inds_tol]
  by_het<-by_het[,colnames(by_het) %in% gsub("cp","",colnames( het_treat))]
  
  
  
  return(list(het_treat=het_treat,by_het=by_het))
}
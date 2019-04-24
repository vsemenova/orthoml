# Compute average price by group
average_group_price<-function(tib,group.name) {

  avprice<-as_tibble(tib) %>%
    filter(eval(as.symbol(group.name))==1 )%>%
    group_by(SiteName,ChannelName,SalesDate) %>%
    dplyr::summarise(meanprice = mean(ownprice,na.rm=TRUE))
  
  return(avprice)

}

# Own and cross price elasticities

# Cross price elasticities prelim
crossprice_group<-function(tib,group.name) {
  # Add an observation identifier
  tib$rowname<-1:dim(tib)[1]
  
  # Compute total sum of prices and count of observations by group for selected
  # set of observations (group.name)
  tibcp<-as_tibble(tib) %>%
    filter(eval(as.symbol(group.name))==1 )%>%
    group_by(SiteName,ChannelName,SalesDate) %>%
    dplyr::summarise(sumprice = sum(ownprice,na.rm=TRUE),
                     count = n()) 
  
  # For group.name set, compute average cross-price elasticity
  # For not group.name set, impute zeros
  
  tibcp<- as_tibble(tib) %>%
    filter(eval(as.symbol(group.name))==1 )%>%
    left_join(.,tibcp)%>%
    mutate(x = (sumprice - ownprice)/(count-1)) %>%
    left_join(tib,.) %>%
    mutate ( x= replace (x,is.na(x)| abs(x)==Inf,0)) 
  
  colnames(tibcp)[colnames(tibcp)=="x"]<-paste0("cp",group.name)
  # save group sizes
  #counts[[group.name]]<-tibcp$count
  # if all group sizes are one 
  # do not create cross-price index 
  if (all(tibcp$count==1,na.rm=TRUE)) {
    tibcp<-dplyr::select(tibcp,-one_of(paste0("cp",group.name)))
  }
  tibcp<-dplyr::select(tibcp,-one_of("sumprice","count"))
  return(tibcp)
}





ownprice_group<-function(tib,group.name) {
  tibop<-as_tibble(tib) %>%
    mutate(x = ownprice *eval(as.symbol(group.name)) )
  colnames(tibop)[colnames(tibop)=="x"]<-paste0("op",group.name)   
  return(tibop)
}

# Total number of sold copies of  each product
# Each product is a denoted by product_code - a binary vector product_code
count_prod<-function(by_het,product_code) {
  sum(apply(abs(by_het-matrix(rep(product_code,dim(by_het)[1]),nrow=dim(by_het)[1],byrow=TRUE)),1,sum)==0)
}

# Total number of observations in branch
count_branch_size<-function(name,my_data) {
  num<-unlist(strsplit( x=(gsub("Level","",name)),split=""))
  key<-paste0("Level",num[1])
  value<-as.numeric(paste0(num[-1],collapse=""))
  
  
  return(sum(my_data[,key]==value))
}

get_products<-function(het_treat,by_het) {

  unik<-!(duplicated(by_het))
  # Set of products
  bh<-by_het[unik,]
  # Order bh
  bh<-bh[order(apply(bh,1,which.max)),]
   # Make sure no duplicated products
  bh<-bh[,sort(colnames(bh),decreasing=TRUE)]
  # Order bh
  bh<-bh[order(apply(bh,1,which.max)),]
  by_het<-by_het[,colnames(bh)]
  # Number of copies per product
  count_products<- apply(bh,1,count_prod,by_het=by_het)
  
  names<-colnames(by_het)[apply(bh,1,which.max)]
  inds<-which(duplicated(names))
  if (length(inds)>0) {
    stop ("Duplicated products!")
  }
  return(list(products=bh,count=count_products))
}
  

get_branches<-function(htheta,my_data) {
  est<-as.numeric(htheta)
  names(est)<-names(htheta)
  inds_cp<-grep("cp",names(est))
  
  est<-est[inds_cp]
  A.het<-attr(htheta,"White")
  A.het<-A.het[inds_cp,inds_cp]
  inds_cp<-order(names(est))
  est<-est[inds_cp]
  A.het<-A.het[inds_cp,inds_cp]
  
  my_data$Level0<-1
  branch_sizes<-sapply(gsub("cp","",names(est)), count_branch_size,my_data=my_data)
  branch_sizes<-branch_sizes[order(names(branch_sizes))]
  return(list(est=est,A.het=A.het,branch_sizes=branch_sizes))
}
  
  
get_cross_price_matrix<-function(het_treat,by_het,htheta,my_data,
                                 digs=3,price_res,res_ss_b) {
  # Get products info
  by_het<-cbind(by_het,Level01=1)
  het_treat<-cbind(het_treat,Level01=price_res)
  
  prods<-get_products(het_treat=het_treat,by_het=by_het)
  # Get branches info
  branches<-get_branches(htheta=htheta,my_data=my_data)
  
  
  bh<-prods$products
  
  # Number of different products
  n_products<-dim(bh)[1]
  # Number of copies sold per product
  count_products<-prods$count
  # Estimated model of cross-price elasticity
  ## Estimator
  est<-branches$est
  # STD Error
  A.het<-branches$A.het
  branch_sizes<-branches$ branch_sizes
  # Matrix of estimates
  cp_matrix<-matrix(0,n_products,n_products)
  # Matrix of st.errors
  cp_matrix_sd<-matrix(0,n_products,n_products)
  
  for (i in 1:n_products) {
    for (j in 1:n_products) {
      # Cross-price on off-diagonal element
      if (i != j) {
        # Category of i
        name_k<-which.max(bh[i,(dim(bh)[2]+1-length(est)):(dim(bh)[2])])
        # Category of j
        name_j<-which.max(bh[j,(dim(bh)[2]+1-length(est)):(dim(bh)[2])])
        ## This assumes cross-price variables go up to level2 only
        if (name_k != name_j) {
          #cp_matrix[i,j]<-est[1]*count_products[j]/(branch_sizes[1]-1)
          #cp_matrix_sd[i,j]<-A.het[1,1]*count_products[j]/branch_sizes[1]
          cp_matrix[i,j]<-est[1]*(count_products[j])/(branch_sizes[1]-1)
          cp_matrix_sd[i,j]<-A.het[1,1]*(count_products[j])/(branch_sizes[1]-1)
          
           #cp_matrix[i,j]<-est[1]*(count_products[j]/branch_sizes[1])*(count_products[i]/branch_sizes[1])
           #cp_matrix_sd[i,j]<-A.het[1,1]*(count_products[j]/branch_sizes[1])*(count_products[i]/branch_sizes[1])
          
        } else {
          name.est<-paste0("cp",names(name_j))
        #  estkj<-c(est[1], est[name.est])/c(branch_sizes[1],branch_sizes[names(name_j)])
         # cp_matrix[i,j]<-sum(estkj)*count_products[j]
          
          #vec<-c(1/branch_sizes[1],1/branch_sizes[names(name_j)])
          name.1<-rownames(A.het)[1]
          #cp_matrix_sd[i,j]<-sqrt(t(vec)%*%A.het[c(name.1,name.est),c(name.1,name.est)]%*%vec)*count_products[j]
          
          #estkj<-c(est[1], est[name.est])/c((branch_sizes[1])^2,(branch_sizes[names(name_j)])^2)
          #cp_matrix[i,j]<-sum(estkj)*count_products[j]*count_products[i]
          #vec<-c(1/(branch_sizes[1])^2,1/(branch_sizes[names(name_j)])^2)
          #cp_matrix_sd[i,j]<-sqrt(t(vec)%*%A.het[c(name.1,name.est),c(name.1,name.est)]%*%vec)*count_products[j]*count_products[i]
          
          vec<-c(1/(branch_sizes[1]-1),1/(branch_sizes[names(name_j)]-1))
          estkj<-c(est[1], est[name.est])*vec
           cp_matrix[i,j]<-sum(estkj)*count_products[j]
           cp_matrix_sd[i,j]<-sqrt(t(vec)%*%A.het[c(name.1,name.est),c(name.1,name.est)]%*%vec)*count_products[j]
        }
        if (is.na(cp_matrix[i,j])||is.na(cp_matrix_sd[i,j])) {
          stop("Mistake in cross price estimation!")
        }
        
        
      } else {
        name_k<-which.max(bh[i,(dim(bh)[2]+1-length(est)):(dim(bh)[2])])
        cp_matrix[i,i]<-res_ss_b[names(name_k),]$OLS.est
        cp_matrix_sd[i,i]<-res_ss_b[names(name_k),]$OLS.st.error.hat
        if (is.na(cp_matrix[i,j])||is.na(cp_matrix_sd[i,j])) {
          stop("Mistake in cross price estimation!")
        }
      }
        
      }
      
  }
  cp_matrix<-round(cp_matrix,digits=digs)
  cp_matrix_sd<-round(cp_matrix_sd,digits=digs)
 return(list(cp_matrix=cp_matrix,cp_matrix_sd=cp_matrix_sd,bh=bh)) 
}

## 

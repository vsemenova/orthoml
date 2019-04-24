# This scrit excludes categories for purposes
# 1. To make a hierarchical representation w\out multicollinearity
# 2. Avoiding mistakes in hierarchy coding: i.e. Level4=170 means different products 
# that has different values of Level3,Level2 etc.
clean_data<-function(het_treat,by_het) {
  
  #Drinks
  if (level1_inds[1]==7) {
    names_to_exclude<-c("Level3NULL","Level3227","Level4NULL","Level5NULL","Level381")
    
  } else if (level1_inds[1] %in% c(8,5,12,19)) {
    names_to_exclude<-c("Level233","Level260","Level3NULL",
                        "Level396","Level392","Level360","Level399",
                        "Level3259","Level348","Level393","Level340","Level333",
                        "Level346","Level381","Level3181","Level3101","Level3229",
                        "Level361","Level4NULL","Level474","Level483",
                        "Level495","Level451","Level416","Level496","Level482","Level478",
                        "Level466","Level462","Level457","Level455","Level454","Level449",
                        "Level448","Level442","Level441","Level410",
                        "Level5NULL","Level533","Level541","Level538","Level532")
  }  else if (level1_inds[1] %in% c(6,27,28,25)) {
    names_to_exclude<-c("Level2183","Level279","Level3NULL","Level3108",
                        "Level4NULL","Level5NULL")
  } else if (level1_inds[1] %in% c(11,15,4,26)) {
    names_to_exclude<-c("Level2NULL","Level3NULL","Level377","Level3263",
                        "Level356","Level386","Level387","Level389","Level3241","Level3234","Level3261",
                        "Level4NULL","Level477","Level453","Level5NULL"
                        )
  } else {
    names_to_exclude<-NULL
  }
  #Meat,Fish,Poultry
  if(FALSE) {
  q<-het_treat_res$by_het
  q2<-t(q)%*%q
  q3<-q2[grep("Level4",colnames(q)),grep("Level3|Level2|Level1",colnames(q))]
  apply(q3,2,sum)
  apply(q[,grep("Level3|Level2|Level1",colnames(q))],2,sum)
  check<- apply(q3,2,sum)==apply(q[,grep("Level3|Level2|Level1",colnames(q))],2,sum)
  q3<-q3[,check]
  show(q3)
  ans<-apply(q3,2,function(x) {
    a<-rownames(q3)[x!=0]
    return(a[1])
  })
  }
  #Need to exclude 1 category in Level18 and Level119
  
  
 
  
 
  by_het<-by_het[,setdiff(colnames(by_het),names_to_exclude)]
  het_treat<-het_treat[,setdiff(colnames( het_treat),names_to_exclude)]
  
  return(list(het_treat=het_treat,by_het=by_het))
}
crossprice_group<-function(tib,group.name) {
  
  
  tibcp<-as_tibble(tib) %>%
    filter(eval(as.symbol(group.name))==1 ) %>%
    group_by(Site,Channel,week) %>%
    dplyr::summarise(sumprice = sum(ownprice,na.rm=TRUE),
                     count = n()) %>%
    left_join (tib,.) %>%
    mutate(x = (sumprice - ownprice)/(count-1)) %>%
    left_join(tib,.) %>%
    mutate ( x= replace (x,is.na(x)| abs(x)==Inf,0)) 
  colnames(tibcp)[colnames(tibcp)=="x"]<-paste0("cp",group.name)
  # save group sizes
  counts[[group.name]]<-tibcp$count
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


combine_names <- function(..., prefix = "", sep = "_") {
  paste0(prefix, levels(interaction(..., sep = sep)))
}
lag_2<-function(x) lag(x,2)
lag_3<-function(x) lag(x,3)
lag_4<-function(x) lag(x,4)
is.invertible<-function(m) class(try(solve(m),silent=T))=="matrix"

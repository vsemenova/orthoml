### lag functions for data cleaning
lag_2<-function(x){
  return(lag(x,2))
}

lag_3<-function(x){
  return(lag(x,3))
}

lag_4<-function(x){
  return(lag(x,4))
}



#### Create latex table "Bias,St.error,RMSE,Rej.freq. of different estimators
to.Latex<-function (digs,matrix,cap,lab,filename,k,step,measures) {
  ## arguments
  
  ## digs      number of digits after comma
  ## matrix    matrix with values to be made a table
  ## cap       latex table caption
  ## lab       latex table lable
  ## filename  
  ## k         number of columns between multicolumn spaces
  ## measures. Default are Bias, St.Error, MSE, Rejection Frequency
  # step<-length(measures)
  #options(digits=12)
  align<-rep("r",k)
  align<-paste0(align,collapse="")
  align<-paste0(align,"|")
  align<-rep(align,step)
  align<-paste0(align,collapse="")
  align<-paste0("r|",align)
  tab<-xtable(matrix,digits=digs,caption=cap,label=lab,align=align)
  
  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste0(paste0('& \\multicolumn{',as.character(k),'}{c}{', measures, '}', collapse=''), '\\\\')
  
  print(tab,file=filename,append=TRUE, 
        include.rownames=TRUE,
        include.colnames=TRUE,
        sanitize.text.function=function(x){x},
        add.to.row=addtorow)
  
  
  
}






boxwhisker<-function(data,xbreaks=NULL,ci_alpha=0.05,
                     het,limitsy=c(-4,1),xtext=het,outname=NULL,subset.name="",...) {
  #data<-read.csv(filename)
  # data is a data.frame (name,estimator,st.error)
  est<-data[,grep("est",colnames(data))]
  st.error.hat<-data[,grep("st.error.hat",colnames(data))]
  inds<-est-st.error.hat*qnorm(1-ci_alpha/2)>limitsy[1] & est+st.error.hat*qnorm(1-ci_alpha/2)<limitsy[2]
  
  data<-data[inds,]
  est<-est[inds]
  st.error.hat<-st.error.hat[inds]
  show(dim(data))
  if (is.null(xbreaks)) {
    xbreaks<-1:dim(data)[1]
  } else{
    xbreaks<-xbreaks[inds]
  }
  
  if (is.null(outname)) {
    outname<-paste0(level1_inds,het)
  }
  if (dim(data)[2] != 2) {
    show("Check dim data")
  } else {

    data<-data.frame(x=xbreaks,est=est,ci=st.error.hat*qnorm(1-ci_alpha/2))
    # x axis is time: connect dots
    # x axis is group: do not
    
    q<-ggplot(data, aes(x=x, y = est))+
      geom_errorbar(aes(ymin =est-ci,ymax = est+ci ),colour ="red")+
      geom_point(colour ="red")
    if (het %in% c("month","week","week|month")) {
   
      q<-q+geom_line(colour = "grey") 
    } 
     
      q<-q+xlab("")+
      ylab("")+
      theme(axis.line.x = element_line(color="black", size = 0.25),
            axis.line.y = element_line(color="black", size = 0.25))+
      scale_y_continuous(limits = limitsy )+
      scale_x_discrete(limits =xbreaks)+
      theme_bw()+
      theme(legend.title=element_blank(),axis.text.x = element_text(size=15,face="bold"),
              axis.text.y = element_text(size=20))
    
  
    
  }
  png(paste0(figdirectory,"Box",outname,subset.name,".png"))
  print(q)
  dev.off()
  #return(q)
}
compare_ols_ridge<-function(data,critval=1.96, H=3.5,
                      W=6,
                      eps=0.01,xlabels=NULL,outname=NULL) {
  
  
  if (is.null(outname)) {
    outname<-as.character(level1_inds[1])
  }
  n<-dim(data)[1]
  if (is.null(xlabels)) {
    xlabels<-rownames(data)
  }
 

 xbreaks<-seq(1/length(xlabels),H,H/length(xlabels))
 
 
 dat<-data.frame(OLS=data[,"OLS.est"],Ridge=data[,"Ridge.est"],Lasso=data[,"Lasso.est"],
                 ytick=seq(1/n,H,H/n),
                 count = 1:n)
 df<-melt(dat,id.vars=c("ytick","count"))
 
 colnames(df)<-c("ytick","count","name","estimate")
 df$ytick[df$name=="Ridge"]<-df$ytick[df$name=="Ridge"]-eps
 df$ytick[df$name=="Lasso"]<-df$ytick[df$name=="Lasso"]-2*eps
 
 df$left<-df$estimate-c(data[,"OLS.st.error.hat"], data[,"Ridge.st.error.hat"], rep(NA,length(data[,"OLS.st.error.hat"])))*critval
 df$right<-df$estimate+c(data[,"OLS.st.error.hat"], data[,"Ridge.st.error.hat"], rep(NA,length(data[,"OLS.st.error.hat"])))*critval
 
 
  
  p<-ggplot(df, aes(ytick, estimate, shape = name,colour = name)) + 
    ylim(c(min(df$left),max(df$right) ))+
    theme(legend.position="right", legend.direction="vertical",
          legend.title=element_blank(),
          legend.background = element_rect(linetype="solid",colour="black")) +
    scale_fill_discrete("")+
    geom_segment(aes(y=left,yend=right,x=ytick,xend=ytick,colour=name))+
    scale_y_continuous(breaks=seq(from=floor(min(df$left,na.rm=TRUE)),
                                  to = ceiling(max(df$right,na.rm=TRUE) ),
                                  by = 2
    ))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    scale_x_continuous(breaks=xbreaks,labels=xlabels)+
    theme(legend.background = element_rect(colour = "black"),
          legend.position = c(0.85,0.5),
          axis.text.y = element_text(size=15),
          axis.text.x = element_text(size=10))+
    scale_color_manual(values = c("blue", "black","red"))+
    geom_point()
  
  #save_plot(filename =paste0(figdirectory,"OLSRidge",outname,".png"), p)
  print(p)
  ggsave(paste0(figdirectory,"OLSvsRidge",outname,".png"),width=20,height=5,device="png")
  
  dev.off()
  
  
  
}
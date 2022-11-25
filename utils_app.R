
drop_multicollinear_categories<-function(categoryname) {
  
  #Drinks
  
  if (categoryname=="Drinks") {
    names_to_exclude<-c("Level3NULL","Level3227","Level4NULL","Level5NULL","Level381")
    
  } else if (categoryname =="Dairy") {
    names_to_exclude<-c("Level233","Level260","Level29","Level3NULL",
                        "Level396","Level392","Level360","Level399","Level398",
                        "Level3259","Level348","Level393","Level340","Level333",
                        "Level346","Level381","Level3181","Level3101","Level3229",
                        "Level361","Level4NULL","Level474","Level483",
                        "Level495","Level451","Level416","Level496","Level482","Level478",
                        "Level466","Level462","Level457","Level455","Level454","Level449",
                        "Level448","Level442","Level441","Level410","Level434",
                        "Level5NULL","Level533","Level541","Level538","Level532")
  }  else if (categoryname =="Snacks") {
    names_to_exclude<-c("Level2145","Level2183","Level279","Level3NULL","Level3108",
                        "Level4NULL","Level5NULL","Level299")
  } else if (categoryname == "NonEdible") {
    names_to_exclude<-c("Level294","Level297","Level2NULL","Level3NULL","Level377","Level3263",
                        "Level356","Level386","Level387","Level389","Level3241","Level3234","Level3261",
                        "Level4NULL","Level477","Level453","Level5NULL","Level3237", "Level3254", "Level359" , "Level372",  "Level373" 
    )
  } else {
    names_to_exclude<-NULL
  }
  

  return(names_to_exclude)
}

combine_est_sd<-function(a,b) {
  n<-length(a)
  z<-rep(0,2*n)
  z[2*c(1:n)]<-b
  z[2*c(1:n)-1]<-a
  return(z)
}

#### Assign names to elasticity estimates
#### Requires knowledge how to translate numeric category code into categoryname
#### E.g., Level2188 is Water; Level2166 is Sodas

#### Create box and whisker plots to plot elasticities

boxwhisker<-function(data,ci_alpha=0.05,
                     het.name,figdirectory,limitsy=c(-4,1),xtext=het.name,outname=NULL,subset.name="",...) {

 
 
  # select products whose CI fit in the chosen picture limits
  inds<-data$est-data$st.error.hat*qnorm(1-ci_alpha/2)>limitsy[1] & data$est +data$st.error.hat*qnorm(1-ci_alpha/2)<limitsy[2]
  data<-data[inds,]
  
  
  if (is.null(outname)) {
    outname<-paste0(level1_inds,het.name)
  }

    # CI is std.error times the t-statistics
    data$ci<-data$st.error.hat *qnorm(1-ci_alpha/2)
   
    # x axis is time: connect dots
    # x axis is group: do not
    
    q<-ggplot(data, aes(x=xbreaks, y = est))+
      geom_errorbar(aes(ymin =est-ci,ymax = est+ci ),colour ="red")+
      geom_point(colour ="red")
    if (het.name %in% c("month","week","week|month")) {
      
      q<-q+geom_line(colour = "grey") 
    } 
    
    q<-q+xlab("")+
      ylab("")+
      theme(axis.line.x = element_line(color="black", size = 0.25),
            axis.line.y = element_line(color="black", size = 0.25))+
      scale_y_continuous(limits = limitsy )+
      scale_x_discrete(limits =data$xbreaks)+
      theme_bw()+
      theme(legend.title=element_blank(),axis.text.x = element_text(size=15,face="bold"),
            axis.text.y = element_text(size=20))
    
    
    
  
  png(paste0(figdirectory,"Box",outname,subset.name,".png"))
  print(q)
  print(paste0("Plotting estimated elasticities and confidence bands at ",paste0(figdirectory,"Box",outname,subset.name,".png")))
  
  dev.off()
  #return(q)
}

# Histogram of estimated elasticities at Level determined by bottomlevel
# Grouped by color determined by Level1
hist_coef<-function(data,grouping_level,sample_size,outname,figdirectory,
                    xtext=NULL,maxy=10,eps=0.5,minx=15,lambda_ridge=0.9,xlimh,...) {

  legend_breaks<-unique(data[,grouping_level])
 legend_values<-c("red","blue","green","yellow","pink",
                     "orange","turquoise","maroon","gold",
                     "tan","violet","skyblue")
  
  
  
  data_long<-melt(data,id.vars=grouping_level)
  df<-data.frame(x=1:length( data_long$variable),est= data_long$value,name= data_long$variable,grouping_level=data_long[,grouping_level])
  m<- ggplot(df, aes(x =est,colour=grouping_level,fill=grouping_level))+
    geom_histogram()+
    facet_grid(. ~ name)+
    background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
    panel_border()+
    xlab("")+
    ylab("Count")+
    xlim(xlimh[1],xlimh[2])+
    geom_vline(xintercept=0)+
    scale_fill_manual(values =legend_values,
                      labels = legend_breaks, drop = FALSE)+
    scale_color_manual(values =legend_values,
                       labels = legend_breaks, drop = FALSE)+
    theme_bw()+
    theme(legend.title=element_blank())+
    theme(legend.title=element_blank(),axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30), axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),legend.text = element_text(size=20),
          strip.text.x = element_text(size = 20)
    ) # and a border around each panel
  
  
  #print(m)
 # print(paste0("Plotting estimated elasticities and confidence bands at ",paste0(figdirectory,"Hist",outname,".png")))
  
  ggsave(paste0(figdirectory,"Hist",outname,".png"),width=20,height=10,device="png")
  #dev.off()
  
}
make.name<-function(x) {
  if (!is.na(strsplit(x,"Level1_Name")[[1]][2])) {
    x<-strsplit(x,"Level1_Name")[[1]][2]
  }
  if (!is.na(strsplit(x,"Level2_Name")[[1]][2])) {
    x<-strsplit(x,"Level2_Name")[[1]][2]
  }
  if (!is.na(strsplit(x,"Level3_Name")[[1]][2])) {
    x<-strsplit(x,"Level3_Name")[[1]][2]
  }
  x<-make.names(x)
  return(x)
}


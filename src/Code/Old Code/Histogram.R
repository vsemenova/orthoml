# Histogram of coefficients
# Grouped by color determined by previous level/not
hist_coef<-function(data,bottomlevel,toplevel,fe_data,toplevel_inds=NULL,
                    legend_breaks=NULL,
                    legend_values=NULL,
                    xtext=NULL,maxy=10,outname=NULL,estimator.names=c("Lasso","Ridge","OLS"),
                    eps=0.5,missing_cat,minx=15,mu_bar=1,...) {
  # data is a data.frame of estimates
  colnames(data)<-gsub(".est","",colnames(data))
  data<-data[,estimator.names]
  show(colnames(data))
#  if (is.null(outname)) {
#    outname<-paste0(level1_inds,het)
#  }
  
  if (is.null(estimator.names)) {
    estimator.names<-colnames(data)
  }
  if (is.null(outname)){
    if (!is.null(legend_breaks)){
      outname<-paste0(bottomlevel,legend_breaks[1])
    } else {
      outname<-paste0(bottomlevel,as.character(level1_inds[1]))
    }
  
    show(outname)
  }
  source("FindMyLevel.R")
  map<-grouping(bottomlevel,toplevel,fe_data)
  show(map[sort(rownames(map)),])
  toplevel_vec<-as.character(sapply(rownames(data),get_my_level,map=map))
  show(toplevel_vec)
 
  
  if (level1_inds[1]!=7) {
    toplevel_vec<-gsub(toplevel,"",toplevel_vec)
    toplevel_vec<-gsub("Level1None","1",toplevel_vec)
    toplevel_vec[toplevel_vec=="None"]<-missing_cat
  }
  
  show(unique(toplevel_vec))
  
  if (!is.null(toplevel_inds)) {
    data<-data[toplevel_vec %in% toplevel_inds,]
    toplevel_vec<-toplevel_vec[toplevel_vec %in% toplevel_inds]
  }
  if (is.null(legend_breaks)) {
    legend_breaks<- as.character(sort(as.numeric(setdiff(unique(toplevel_vec),c("None","NULL") ))  ))
    show(legend_breaks)
  }
  if (is.null(legend_values)) {
    legend_values<-c("red","blue","green","yellow","pink",
                     "orange","turquoise","maroon","gold",
                     "tan","violet","skyblue")
  }
 # if (is.null(xtext)) {
  #  xtext<-cbind(het, " estimates")
 # }

  hists<-list()
  
  library(cowplot)
  
  #require(cowplot)
  theme_set(theme_cowplot(font_size=30))
  
  data$toplevel_vec<-as.numeric(toplevel_vec)
  data<-data[order(data$toplevel_vec),]
  data$toplevel_vec<-as.factor(data$toplevel_vec)
  #for (name.est in setdiff(colnames(data),"toplevel_vec")){
  show(estimator.names)
  
  x<-melt(data,id.vars="toplevel_vec")
  df<-data.frame(x=1:length(x$variable),est=x$value,name=x$variable,toplevel=x$toplevel_vec)
 m<- ggplot(df, aes(x =est,colour=toplevel,fill=toplevel))+
    geom_histogram()+
   facet_grid(. ~ name)+
   background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
   panel_border()+
   xlab(paste( "Number of groups= ",as.character(dim(data)[1]), "Total Sample Size= ",as.character(dim(res_fs$fe_pdata)[1]), "Ridge penalty ",as.character(mu_bar),collapse="."))+
   ylab("Count")+
   scale_y_continuous(limits=c(0,maxy))+
   scale_x_continuous(limits=c(-minx,minx))+
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

 
  print(m)
  ggsave(paste0(figdirectory,"Hist",outname,".png"),width=20*dim(data)[2]/3,height=10,device="png")
 # save_plot(filename =paste0(figdirectory,"Hist",outname,".png"), m, base_width = 10)
  dev.off()
  ## Need to rewrite using facet_wrap
}


  
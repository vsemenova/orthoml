rm(list=ls())
# set directoryname
#directoryname<-"/bbkinghome/vsemen/ALICE"
directoryname<-"/Users/virasemenora/Dropbox (MIT)/MSR/Code/ALICE"
setwd(directoryname)
setwd("JJFoods")
level1_inds<<-1
source("Source.R")
setwd("Output/Text")
#name<-"Drinks"
#name<-"Snacks"
name<-"Dairy"

#name<-"NonEdible"
cptable<-read.csv(paste0("CrossPriceEst",name,".csv"))
cptable$X<-as.character(cptable$X)
# Remove own price elasticities from consideration
diag(cptable[,2:dim(cptable)[2]])<-0

if (name == "Dairy") {
  colnames(cptable)<-gsub(", Other","",colnames(cptable))
  colnames(cptable)<-gsub("Other","",colnames(cptable))
  cptable$X<-gsub("Other","", cptable$X)
  cptable$X<-gsub(", Other","", cptable$X)
  colnames(cptable)<-gsub(", ","",colnames(cptable))
  cptable$X<-gsub(", ","",cptable$X)
}

rescale<-function(vec) {
  
  minV<-min(vec)
  maxV<-max(vec)
  vec<-(vec-minV)/(maxV-minV)
}



if (name=="Drinks") {
  find_my_level_drinks<-function(word,vec) {
    if (word %in% grep("*Wine|Champagne|Alcohol",vec,value=TRUE )) {
      level1<-"Alcohol"
    } else if (word %in% grep("*Water",vec,value=TRUE)) {
      level1<-"Water" 
    } else {
      level1<-"SD"
    }
    return(level1)
  }
  cptable$X[cptable$X=="Other Drinks"]<-"Alcohol"
  colnames(cptable)[25]<-"Alcohol"
  cptable$level1<-sapply(cptable$X,find_my_level_drinks,vec = as.character(cptable$X))
} else if (name=="Snacks") {
  find_my_level_snacks<-function(word,vec) {
     if (word %in% grep("*Quorn|Veggie",vec,value=TRUE)) {
      level1<-"Veggie Meals"
    } else if (word %in% grep("*Sugar|Sweeteners|Spread",vec,value=TRUE)) {
      level1<-"Sugar" 
    } else if (word %in% grep("*Cookies|Nuts|Crackers|Popcorn|Poppadoms|Bars|Crisps|Gum|Lollypop|Chocolate|Wafers|Waffle|Onion|Party Snacks|Party.Snacks",vec,value=TRUE))  {
      level1<-"Snacks"
    } else {
      level1<-"Desserts"
    }
    return(level1)
  }
  cptable$level1<-sapply(cptable$X,find_my_level_snacks,vec = as.character(cptable$X))
} else if (name =="Dairy") {
  find_my_level_dairy<-function(word,vec) {
    if (word %in% grep("*Chiken|Turkey|Poultry",vec,value=TRUE)) {
      level1<-"Poultry"
    } else if (word %in% grep("*Cream|Cheddar|Milk|Cheese|Eggs|Mozzarella|Ghee|Dairy|Yoghurt|Butter|Margarine",vec,value=TRUE)) {
      level1<-"Dairy" 
    } else if (word %in% grep("Pork|Steak|Meat|Pizza.Toppings|Lamb|Beef",vec,value=TRUE))  {
      level1<-"Meat"
    } else {
      level1<-"Fish"
    }
    return(level1)
  }
  cptable$level1<-sapply(cptable$X,find_my_level_dairy,vec = as.character(cptable$X))
  
} else if (name =="NonEdible") {
  find_my_level_ne<-function(word,vec) {
   
    mat<-as.data.frame(cbind(diag(as.matrix(cptable[,2:dim(cptable)[2]])),colnames(cptable)[-1]))
   mat[,1]<-as.numeric(as.character(mat[,1]))
   mat<-mat[order(mat[,1]),]
    if (word %in% mat[1:35,2]) {
      level1<-"packaging"
    } else if (word %in% mat[36:83,2] ) {
      level1<-"hygiene"
    } else {
      level1<-"stationery"
    }
    return(level1)
  }
  cptable$level1<-sapply(cptable$X,find_my_level_ne,vec = as.character(cptable$X))
}
  
  
cptable.m<-melt(cptable)
colnames(cptable.m)<-c("Name.x","level1","Name.y","Elasticity")
cptable.m$Name.y<-as.character(cptable.m$Name.y)


if (name=="Drinks"){
  cptable.m<-arrange(cptable.m,level1,desc(Name.x),desc(Name.y))%>%
    mutate(level1.y=sapply(Name.y, find_my_level_drinks, vec=unique(cptable.m$Name.y))) %>%
    arrange(level1,level1.y,desc(Name.x),desc(Name.y))%>%
    mutate(Elasticity.resc=rescale(Elasticity)) 
  
} else if (name == "Snacks") {
  cptable.m<-arrange(cptable.m,level1,desc(Name.x),desc(Name.y))%>%
    mutate(level1.y=sapply(Name.y, find_my_level_snacks, vec=unique(cptable.m$Name.y))) %>%
    arrange(level1,level1.y,desc(Name.x),desc(Name.y))%>%
    mutate(Elasticity.resc=rescale(Elasticity)) 
  
  
} else if (name == "Dairy")  {
  cptable.m<-arrange(cptable.m,level1,desc(Name.x),desc(Name.y))%>%
    mutate(level1.y=sapply(Name.y, find_my_level_dairy, vec=unique(cptable.m$Name.y))) %>%
    arrange(level1,level1.y,desc(Name.x),desc(Name.y))%>%
    mutate(Elasticity.resc=rescale(Elasticity)) 
  
}else if (name == "NonEdible")  {
  cptable.m<-arrange(cptable.m,level1,desc(Name.x),desc(Name.y))%>%
    arrange(level1,desc(Name.x),desc(Name.y))%>%
    mutate(Elasticity.resc=rescale(Elasticity)) 
  
}


my.values<-sort(unique(cptable.m$Elasticity.resc))
mv<-sort(unique(cptable.m$Elasticity))
  
names(my.values)[mv==0]<-"white"
 
names(my.values)[mv>0]<-rep("lightyellow",length(my.values[mv>0]))
names(my.values)[mv>(0.1)]<-"lightyellow2"
names(my.values)[mv>(0.2)]<-"lightyellow3"
names(my.values)[mv>(0.4)]<-"lightyellow4"

names(my.values)[mv<0]<-rep("lightblue",length(my.values[mv<0]))
names(my.values)[mv>(-0.1) & mv<0]<-"lightblue1"
names(my.values)[mv<(-0.2)]<-"steelblue1"
names(my.values)[mv<(-0.4)]<-"steelblue2"


my.colors<-names(my.values)
my.labels<-c(paste0("Min ",as.character(min(cptable.m$Elasticity))), rep("",length(my.values)-2),paste0("Max ",as.character(max(cptable.m$Elasticity))))
  




#cptable.m$Name.y<-as.factor(cptable.m$Name.y)


#
# my.values for drinks

# snacks
p2 <- ggplot(cptable.m, aes(Name.x,Name.y)) + geom_tile(aes(fill = Elasticity.resc),   colour = "white")+
  scale_fill_gradientn(colours=my.colors,
                       values=my.values, breaks=my.values, labels=my.labels)+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(size=30,face="bold"),axis.text.y=element_text(size=30,face="bold"))+
  theme(legend.title=element_blank(),legend.text = element_text(size=30,face="bold"),
        legend.key.size=unit(2,"cm"))+
  scale_x_discrete(limits = unique(cptable.m$Name.x),breaks=as.character(unique(cptable.m$Name.x)),labels=as.character(1:length(unique(cptable.m$Name.x))))+
  scale_y_discrete(limits = unique(cptable.m$Name.y),breaks=as.character(unique(cptable.m$Name.y)),labels=paste(as.character(unique(cptable.m$Name.y)),as.character(1:length(cptable$X))))


q<-dplyr::select(cptable,X,level1) %>%
  mutate(level1 = as.factor(level1)) %>%
  group_by(level1) %>%
  summarise(sum = count(level1))

q<-as.matrix(q$sum)
q<-cumsum(q[,2])

for ( i in 1:(length(q)-1)){
  
  x1<-q[i]+1
  p2<-p2+
    geom_segment(x=x1,y=0,xend=x1,yend= q[length(q)]+1,col="red",size=1)+
    geom_segment(x=0,y=x1,xend= q[length(q)]+1,yend= x1,col="red",size=1)
}
ggsave(paste0(figdirectory,"HeatMap",name,".png"),height=40,width=49)




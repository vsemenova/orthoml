# Price plots
res<-res_fs


theme_set(theme_cowplot(font_size=20))

dat<-res$my_data%>%
  dplyr::select(logprice,week)%>%
  group_by(week) %>%
  dplyr::summarise(av_price = mean(logprice[logprice>-Inf]))
q<-ggplot(dat, aes(x=week,y=av_price))+
  geom_point()+
  ylab("Average log price")+
  theme(axis.line.x = element_line(color="black", size = 0.25),
        axis.line.y = element_line(color="black", size = 0.25))+
  scale_x_discrete(breaks=4*c(0:13))
print(q)
png(paste0(figdirectory,"week",as.character(level1_inds),"Price map",subset.name,".png"))
print(q)
dev.off()
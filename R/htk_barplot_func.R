htk_barplot_func <- function(data_source,yvar,country,title,subtitle=NULL,Xlabel=NULL,Ylabel=NULL){

  myv<-data_source %>% filter(ISO3_code==country) %>% dplyr::select(yvar)
  if(dim(myv)[1]!=0){
    plot <- ggplot(data =data_source) +
      geom_bar(stat="identity",aes(x=reorder(ISO3_code, -get(yvar)),y=get(yvar)),fill="steelblue",width=0.7,alpha=0.6) +
      geom_bar(stat="identity",aes(x=reorder(ISO3_code, get(yvar)),y=ifelse(ISO3_code!=country,NA,get(yvar))),fill="steelblue",color="navyblue",width=0.7) +
      labs(title = title,
           subtitle= subtitle,
           x= Xlabel,
           y= Ylabel) +
      my_OECD_theme() + #set background as minimal
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size =10,angle=90, vjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size=10),
            plot.title=element_text(face="bold",colour ="steelblue",size=10, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5))
  }   else {
    myctry=countrycode::countrycode(country,origin="iso3c",destination="country.name")
    plot<-ggplot()+
      geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
      geom_point(aes(x=c(0,20),y=c(0,20)),color="white")+
      theme_void()
  }
  return(plot)
}

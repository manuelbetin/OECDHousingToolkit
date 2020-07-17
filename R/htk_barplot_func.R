htk_barplot_func <- function(data_source,yvar,country,title,subtitle=NULL,Xlabel=NULL,Ylabel=NULL){

  myv<-data_source %>% filter(ISO3_code==country) %>% dplyr::select(yvar) %>% pull()
  if (!is.na(myv)){
    plot <- ggplot(data =data_source) +
      geom_bar(stat="identity",aes(x=reorder(ISO3_code, get(yvar)),y=get(yvar)),fill="steelblue",width=0.7,alpha=0.6) +
      geom_bar(stat="identity",aes(x=reorder(ISO3_code, get(yvar)),y=ifelse(ISO3_code!=country,NA,get(yvar))),fill="steelblue",color="navyblue",width=0.7) +
      labs(title = title,
           subtitle= subtitle,
           x= Xlabel,
           y= Ylabel) +
      theme_minimal() + #set background as minimal
      theme(panel.grid.minor =  element_blank(),
            axis.text.x = element_text(size =8,angle=90),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size=7),
            axis.text.y = element_text(size=7),
            plot.title=element_text(face="bold",colour ="steelblue",size=10, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5))
  }   else if(is.na(myv)) {
    plot<-plot.new()
  }
  return(plot)
}

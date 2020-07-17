htk_barplot_func <- function(data_source,xvar,yvar,country,title,subtitle=NULL,Xlabel=NULL,Ylabel=NULL){

  #xvar <- enquo(xvar)
  #yvar <- enquo(yvar)
  myv<-data_source %>% filter(xvar==country)
  myv<- myv [,2]
  if (!is.na(myv)){
  plot <- ggplot(data =data_source,aes(x=reorder(xvar, -yvar),y=yvar)) +
    geom_bar(fill=ifelse(data_source$ISO3_code == country,"red","steelblue"), stat="identity") +
    labs(title = title,
         subtitle= subtitle,
         x= Xlabel,
         y= Ylabel) +
    theme_minimal() + #set background as minimal
    theme(panel.grid.minor =  element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5))
  }   else if (is.na(myv)) {
    plot<-plot.new()
}
  return(plot)
}

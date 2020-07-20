cf_stackedbars_func <- function(data_source,yvar,fillvar,country,title=NULL,subtitle=NULL,Xlabel=NULL,Ylabel=NULL) {

  myv<-data_source %>% filter(ISO3_code==country) %>% dplyr::select(yvar)
  if(dim(myv)[1]!=0){
    plot<-ggplot(data_source) +
      geom_bar(position="stack", stat="identity",aes(fill=get(fillvar), y=get(yvar), x=ISO3_code),alpha=0.5,width=0.7)+
      geom_bar(position="stack", stat="identity",aes(fill=get(fillvar), y=ifelse(ISO3_code==country,get(yvar),NA), x=ISO3_code),color="black",alpha=1,width=0.7)+
      labs(title= NULL,
           subtitle = NULL,
           x=NULL,
           y=NULL) +
      theme_minimal() +
      theme(panel.grid.minor =  element_blank(),
            axis.text.x = element_text(size =10,angle=90, hjust=1),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size=7),
            axis.text.y = element_text(size=10),
            plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.title=element_blank(),
            legend.text = element_text(size=10),
            legend.position = "bottom",
            legend.key.size = unit(0.5,"line"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
      scale_fill_manual(breaks=c("Other", "Own_outright", "Owner_with_mortgage",
                                 "Private_rent", "Subsidized_rent"),
                        values=c("grey60", "olivedrab3", "steelblue3",
                                 "gold2", "khaki"),
                        labels=c("Other", "Own outright", "Owner with mortgage",
                                 "Private rent", "Subsidized rent"))

  }   else {
    myctry=countrycode::countrycode(ctry,origin="iso3c",destination="country.name")
    plot<-ggplot()+
      geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
      geom_point(aes(x=c(0,20),y=c(0,20)),color="white")+
      theme_void()
  }
  return(plot)

}



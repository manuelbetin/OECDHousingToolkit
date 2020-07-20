cf_stackedbars_func <- function(data_source,xvar,yvar,fillvar,country,title=NULL,subtitle=NULL,Xlabel=NULL,Ylabel=NULL) {
  
  plot<-ggplot(data_source, aes(fill=Tenure_cat, y=Tenure_share, x=ISO3_code)) + 
    geom_bar(position="stack", stat="identity")+
    labs(title= NULL,
         subtitle = NULL,
         x=NULL,
         y=NULL) +
    theme_minimal() +
    theme(panel.grid.minor =  element_blank(),
          axis.text.x = element_text(size =7,angle=90, hjust=1),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size=7),
          axis.text.y = element_text(size=7),
          plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5))  +
    theme(legend.title=element_blank())+
    theme(legend.text = element_text(size=3.5))+ theme(legend.position = "bottom")+
    scale_fill_manual(breaks=c("Other", "Own_outright", "Owner_with_mortgage", 
                               "Private_rent", "Subsidized_rent"), 
                      values=c("grey60", "olivedrab3", "steelblue3", 
                               "gold2", "khaki"),  
                      labels=c("Other", "Own outright", "Owner with mortgage", 
                               "Private rent", "Subsidized rent"))+ 
    theme(legend.key.size = unit(0.5,"line"))+
    theme(legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
  
  
  return(plot)
  
}

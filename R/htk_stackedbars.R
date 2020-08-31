htk_stackedbars <- function(data_source,xvar=NULL,yvar=NULL,fillvar=NULL,title=NULL,subtitle=NULL,Xlabel=NULL,Ylabel=NULL,my_labels=NULL) {
  
  data_source = data_source %>% mutate(color=ifelse(iso3 == "OECD","Specific","")) %>%
    mutate(fill_var=paste0(Tenure_cat,color,sep=""))
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  fillvar <- enquo(fillvar)
  
  plot <- ggplot(data=data_source) +
    geom_bar(aes(x=!!xvar,y=!!yvar,fill = !!fillvar,text =paste0("Country: ",!!xvar,"\n",
                                                                       "Value: ", !!yvar,"\n")),stat="identity") +
    labs(title= title,
         subtitle = subtitle,
         x=Xlabel,
         y=Ylabel) +
    theme_minimal() +
    theme(panel.grid.minor =  element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5))  +
    theme(legend.title=element_blank()) + 
    scale_fill_brewer(labels=my_labels)
  # scale_fill_brewer(palette=c("","Spectral"))
    return(plot)
  
}

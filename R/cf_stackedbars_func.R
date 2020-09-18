cf_stackedbars_func <- function(data_source,yvar,fillvar, sortvar,country,title=NULL,subtitle=NULL,Xlabel=NULL,Ylabel=NULL) {

  # sort the variables i want to show in the graph
  data_source$Tenure_cat<-factor(data_source$Tenure_cat,
                                 levels=c("Other",
                                          "Subsidized_rent",
                                          "Private_rent",
                                          "Owner_with_mortgage",
                                          "Own_outright"))
  myv<-data_source %>% filter(ISO3_code==country) %>% dplyr::select(yvar)
  #  yinter<- data_source %>% filter(ISO3_code=="OECD") %>% select("tot_own")
  # yinter <-yinter[1,1]
  data_source_noOECD<-subset(data_source, ISO3_code!="OECD")
  # start plot
  if(dim(myv)[1]!=0){
    plot<-ggplot(data_source_noOECD) +
      geom_bar(position="stack", stat="identity",
               aes(fill=get(fillvar), y= get(yvar), x=reorder(ISO3_code, -get(sortvar))),
               alpha=0.5,width=0.7)  +
      geom_bar(position="stack", stat="identity",
               aes(fill=get(fillvar), y=ifelse(ISO3_code==country,get(yvar),NA), x=ISO3_code),
               color="black",alpha=1,width=0.7)  +
      my_OECD_theme()+
      theme(panel.grid.minor =  element_blank(),
            axis.text.x = element_text(size =10,angle=90, vjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size=10),
            legend.position = "bottom")+
      # plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
      # plot.subtitle =element_text(size =7, hjust = 0.5),
      # legend.title=element_blank(),
      # legend.text = element_text(size=10),
      # legend.position = "bottom",
      # legend.box="vertical",
      # legend.spacing.y = unit(-0.1, "cm"),
      # legend.box.margin = margin(t=-10),
      # legend.key.size = unit(0.5,"line"),
      # legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
      scale_fill_manual(breaks=c( "Own_outright", "Owner_with_mortgage",
                                  "Private_rent", "Subsidized_rent", "Other"),
                        values=c("olivedrab3", "steelblue3",
                                 "gold2", "khaki", "grey60"),
                        labels=c("Own outright", "Owner with mortgage",
                                 "Private rent", "Subsidized rent", "Other"))
    # geom_hline(aes(yintercept=yinter, linetype="OECD home-ownership rate"), colour = "red")+
    #  scale_linetype_manual(name = NULL, values = 2,
    #                 guide = guide_legend(override.aes = list(color ="red")))
  }   else {
    myctry=countrycode::countrycode(country,origin="iso3c",destination="country.name")
    plot<-ggplot()+
      geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
      geom_point(aes(x=c(0,20),y=c(0,20)),color="white") +
      theme_void()
  }
  return(plot)
}

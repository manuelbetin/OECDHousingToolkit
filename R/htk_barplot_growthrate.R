
htk_barplot_growthrate <- function(data_source,yvar,country,title=NULL,subtitle=NULL,Xlabel=NULL,Ylabel=NULL){

  nas<-c("BRA", "CHN", "CZE", "HUN", "IND", "LTU", "LUX" ,"LVA" ,"POL", "SVN" ,"TUR")
  myv<- data_source  %>% mutate(year= year(period))
  myv<-myv %>% group_by(ISO3_code)  %>%
    mutate(year_min=ifelse(ISO3_code %in% nas, min(year), 2005)) %>%
    mutate(year_max=max(year)) %>%
    filter(year==year_min | year==year_max)
  myv<- myv %>%
    mutate(year_to_use=ifelse(year_min!=2005, 2005,year ))

  if(dim(myv)[1]!=0){
    plot <- ggplot(data=myv, aes(x=ISO3_code, y=get(yvar)))+
      geom_bar(data=subset(myv, year_to_use==2005),aes(x=reorder(ISO3_code,get(yvar)),y=get(yvar),fill=factor(year_to_use)),stat='identity', width=0.7,alpha=0.6)    +
      geom_bar(data=subset(myv, year_to_use==2005),aes(x=reorder(ISO3_code,get(yvar)),y=ifelse(ISO3_code!=country,NA,get(yvar)),
                                                       fill=factor(year_to_use)), stat='identity',color="navyblue", width=0.7) +
      geom_point(data=subset(myv, year==2018),aes(colour="2018"),shape=17) +
      theme_minimal()+
      scale_y_continuous(limits = c(0, 180), breaks=(seq(0, 180, 30)))+
      theme(axis.text.x = element_text(size =10, angle = 90, vjust = 0.5),
            axis.text.y = element_text(size=10,),
            panel.grid.major.x = element_blank())+
      labs(title="", y="",x="")+
      theme(legend.title=element_blank(),
            legend.text = element_text(size=10),
            legend.position = "bottom",
            legend.box.margin = margin(t=-25),
            legend.key.size = unit(0.5,"line"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
      scale_fill_manual(values=c("steelblue","black"), name  =NULL, labels=c("2005*", "2018"))+
      scale_color_manual(name = NULL, values = c("2005*" = "steelblue","2018" = "black")) +
      guides(fill = guide_legend(override.aes = list(shape = NA)))
  }   else {
    myctry=countrycode::countrycode(country,origin="iso3c",destination="country.name")
    plot<-ggplot()+
      geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
      geom_point(aes(x=c(0,20),y=c(0,20)),color="white")+
      theme_void()
  }
  return(plot)
}


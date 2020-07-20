cf_lineplot_gen <- function(data_source,xvar,yvar,valuevar,title=NULL,
                            subtitle=NULL,Xlabel=NULL,Ylabel=NULL,country) {


  data_source2 <- data_source %>% filter(ISO3_code==country) %>%
    mutate(ISO3_code=countrycode::countrycode(country,origin="iso3c",destination="country.name"))
  data_source3 <- data_source %>% group_by(period) %>% summarize(OECDmean=mean(get(valuevar),na.rm=T))

  ggplot(data=data_source) +
    geom_line(aes(x=get(xvar), y=get(valuevar), group=get(yvar)),alpha=0.05) +
    labs(title = title,
         subtitle = subtitle,
         x = Xlabel,
         y = Ylabel) +
    theme_minimal() +   #set the background of the plot as white
    theme(panel.grid.minor =  element_blank(),
          legend.position ="bottom",
          legend.title=element_blank(),
          legend.text = element_text(size=10),
          legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          legend.key.size = unit(0.5,"line"),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size=9),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=7),
          plot.title=element_text(face="bold",colour="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(hjust = 0.5)) +
    geom_line(data=data_source2,aes(x=get(xvar), y=get(valuevar),colour=get(yvar))) +
    geom_line(data=data_source3,aes(x=get(xvar), y=OECDmean),colour="blue",alpha=0.5) +
    theme(legend.position = "right")

}

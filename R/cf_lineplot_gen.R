cf_lineplot_gen <- function(data_source,xvar,yvar,valuevar,title=NULL,
                            subtitle=NULL,Xlabel=NULL,Ylabel=NULL,country) {
  data_source2 <- data_source %>% filter(ISO3_code==country)
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  valuevar <- enquo(valuevar)
  ggplot(data=data_source) +
    geom_line(aes(x=!!xvar, y=!!valuevar, group=!!yvar),alpha=0.05) +
    labs(title = title,
         subtitle = subtitle,
         x = Xlabel,
         y = Ylabel) +
    theme_minimal() +   #set the background of the plot as white
    theme(panel.grid.minor =  element_blank(),
          legend.position ="bottom",
          legend.title=element_blank(),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size=9),
          axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          plot.title=element_text(face="bold",colour="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(hjust = 0.5)) +
    geom_line(data=data_source2,aes(x=!!xvar, y=!!valuevar,colour=!!yvar)) +
    theme(legend.position = "none")
  
}
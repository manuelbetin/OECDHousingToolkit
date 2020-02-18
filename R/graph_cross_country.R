graph_cross_country=function(mydata,variable,
                                 title="Title",
                                 source="Source",
                                 ylabel="ylabel",
                                 ref_period="2018-01-01",
                                 start_date_avg="1995-01-01",
                                 end_date_avg="2015_01-01",
                                 #♪font_family_graph="#1564b2",
                                 font_size_graph=10,
                                 font_family_graph="Times New Roman",
                                 fill_color_graph="dodgerblue3",
                                 font_size_xlabels=1.3){
  
  var=as.character(variable)
  y=mydata %>% dplyr::select(ISO3_Code,Period,var)
  y_ref_period= y %>% filter(Period==ref_period)
  
  #if(all(is.na(y_ref_period %>% dplyr::select(var)))){
  #  ref_period=ref_period-1
  #  y_ref_period= y %>% filter(Period==ref_period-1)
  #}
  
  
  j=1
  while(all(is.na(y_ref_period %>% dplyr::select(var))) && (ref_period-1)>0){
    ref_period=ref_period-1
    y_ref_period= y %>% filter(Period==ref_period)
    start_date_avg2=start_date_avg-1
    end_date_avg2 = end_date_avg-1
    j=j+1
  }
  
  y_mean_avg_period= y %>% group_by(ISO3_Code) %>% filter(Period>=start_date_avg & Period<=end_date_avg) %>% summarise(mean.var=mean(get(variable), na.rm = TRUE))
  
  mean_ref_period=y %>% filter(Period==ref_period) %>% summarise(mean.var=mean(get(variable), na.rm = TRUE))
  mean_avg_period=y %>% filter(Period>=start_date_avg & Period<=end_date_avg) %>% summarise(mean.var=mean(get(variable), na.rm = TRUE))
  
  title=paste(title,ref_period,sep=" ")
  
  font_family_graph_save=font_family_graph
  if(source=="Source: ADB, OECD"){
    font_family_graph="#1564b2"
  }
  
  graph=ggplot() +
    geom_bar(stat = "identity",aes(x = reorder(y_ref_period[,"ISO3_Code"],y_ref_period[,var]), y = y_ref_period[,var]),fill=fill_color_graph) +
    geom_point(data=y_mean_avg_period,aes(x=ISO3_Code, y=mean.var,color="Red"),size=3)+
    geom_hline(yintercept = mean_ref_period$mean.var ,col=fill_color_graph) +
    geom_hline(yintercept = mean_avg_period$mean.var ,col='red') +
    xlab("") +
    ylab(ylabel) +
    labs(title=title,
         subtitle = "",
         caption=source, colour="") +
    #  theme_ECO(legendposition  = c(0.48, 0.9), 
    #                      legenddirection = "vertical",
    #                      panelontop      = FALSE,
    #                      rotateXLabel    = "YES",
    #                      empty_axis_on   = "right",
    #                      ticksOnX        = "YES") 
    scale_color_manual(labels = paste("Average:",start_date_avg,"-",end_date_avg,sep=""), values ="red") +
    theme_bw(base_size = font_size_graph,base_family = font_family_graph)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size = rel(font_size_xlabels)),legend.position = "bottom")
  
  if(all(is.na(y_ref_period %>% dplyr::select(var)))){
    ref_period=ref_period+j
    start_date_avg=start_date_avg+j 
    end_date_avg = end_date_avg+j
  }
  y_ref_period<<-y_ref_period %>% dplyr::select(-c(Period))
  y<<-y
  if(source=="Source: ADB, OECD"){
    font_family_graph=font_family_graph_save
  }
  return(graph)
}

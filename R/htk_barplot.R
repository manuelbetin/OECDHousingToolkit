
## Generate a standard barplot template

htk_barplot <- function(data_source,x,y,y2=NULL,y3=NULL,Xlabel=NULL,Ylabel=NULL,title=NULL,subtitle=NULL, file=NULL, path=NULL)
{

  data_source=data_source %>% mutate(mycolor=ifelse(ISO3_code=="OECD","red","steelblue"))

  plot <- ggplot(data =data_source) +
    geom_bar(aes(x=get(x),y=get(y),fill=mycolor),stat="identity",show.legend = F) +
    labs(title = title,
         subtitle=subtitle,
         x= Xlabel,
         y= Ylabel) +
    theme_minimal() + #set background as minimal
    scale_fill_manual(name = NULL, values=c("darkred","steelblue")) +
    theme(legend.position = "none",
          panel.grid.minor =  element_blank(),
          axis.text.x = element_text(size = 6,angle=90),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size=8),
          axis.text.y = element_text(size=6),
          plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5)) +

    {if(!is.null(y2)) {
      geom_point(aes(x,y=y2), na.rm = TRUE, shape=17, show.legend = TRUE, colour ="black")
    }} +
    {if(!is.null(y3)) {
      geom_point(aes(x,y=y3), na.rm = TRUE, shape =18, show.legend = TRUE, colour="red")
    }}
  return(plot)
  if (!is.null(file)){
    ggsave(filename=file,device = 'png',path=path,width = 4, height = 4, dpi = 300 )
  }
}



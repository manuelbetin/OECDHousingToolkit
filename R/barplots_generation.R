
# htk_barplot_extract <- function (variable=".GDP.A", date= 2018)
# {
#
#   series_filter <- paste0(paste0(as.character(FE_GroupCountries("OECD")[,1]),collapse='+'),variable) #Add to the list of OECD countries the chosen suffix that specifies the data to extract
#   data <- GetData("ECO","EO",series_filter,date,date,"ROW") #extract country data for chosen in rows
#   data <- select(data,"LOCATIONS","VALUE") %>% arrange(VALUE) #keep only locations and value variables
#   data <- arrange(data, VALUE) %>%mutate(LOCATIONS = fct_reorder(LOCATIONS,VALUE)) #rearrange the data in order
#   return(data)
# }

## Generate a standard barplot template

htk_barplot_graph <- function(data_source,x,y,y2=NULL,y3=NULL,Xlabel=NULL,Ylabel=NULL,title=NULL,subtitle=NULL, file=NULL, path=NULL)
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



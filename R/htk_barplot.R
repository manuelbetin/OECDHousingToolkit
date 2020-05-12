#' barplots with OECD template style to display cross country comparisons in the housing toolkit chapters
#'
#'@description The functions generates barplots displaying the value of the selected indicator for each country and with the average OECD value in highlighted. The plot respect the OECD style and allow the generation of standardize figures for all the chapters of the housing toolkit
#'
#'@param data_source dataframe that contains the variables and dates to be chosen
#'@param x x axis variable, often OECD countries
#'@param y y axis variable to be plotted
#'@param y2 other y variable to be plotted
#'@param y3 other y variable to be plotted
#'@param Xlabel label of the X axis
#'@param Ylabel label of the Y axis
#'@param ylegend legend of the first y variable
#'@param y2legend legend of the second y variable
#'@param y3legend legend of the third y variable
#'@param title title of the plot
#'@param subtitle subtitle of the plot
#'@param file name of the file that contains the plot if you decide to save it, filename must end with .png
#'@param path1 specify exit path of the file
#'
#'@return returns a standardized barplot
#'
#'@author
#'Manuel Betin
#'Maxime Nguyen
#'
#'@export

htk_barplot <- function(data_source,x,y,y2=NULL,y3=NULL,Xlabel=NULL,Ylabel=NULL,ylegend=NULL,y2legend=NULL,y3legend=NULL,title=NULL,subtitle=NULL, file=NULL, path1=NULL)
{

  data_source=data_source %>% mutate(mycolor=ifelse(ISO3_code=="OECD","red","steelblue"))
  plot <- ggplot(data =data_source) +
    geom_bar(aes(x=get(x),y=get(y), fill=ylegend,text=paste0("Country: ",get(x),"\n",
                                                            "Value: ", round(get(y),2),"\n")),stat="identity",show.legend = F) +
    labs(title = title,
         subtitle=subtitle,
         x= Xlabel,
         y= Ylabel) +
    theme_minimal() + #set background as minimal
    theme(panel.grid.minor =  element_blank(),
          axis.text.x = element_text(size =11,angle=90),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5)) +

    {if(!is.null(y2)) {
      geom_point(aes(x=get(x),y=get(y2), colour=y2legend, text=paste0("Country: ",get(x),"\n","Value: ", round(get(y),2),"\n")), na.rm = TRUE, shape=17, show.legend = TRUE)
    }} +
    {if(!is.null(y3)) {
      geom_point(aes(x=get(x),y=get(y3), colour=y3legend,text=paste0("Country: ",get(x),"\n","Value: ", round(get(y),2),"\n")), na.rm = TRUE, shape =19, show.legend = TRUE)
    }} +
    scale_fill_manual(name=NULL, values="steelblue") +
    scale_color_manual(name=NULL,values=c("black","gray71")) +
    theme(legend.position="bottom")
  return(plot)
  if (!is.null(file)){
    ggsave(filename=file,device = 'png',path=path1,width = 4, height = 4, dpi = 300 )
  }
}



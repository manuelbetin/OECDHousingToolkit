#'Line barplots with OECD template style to display timeseries evolutions in the housing toolkit chapters
#'
#'@description Function that plots a lines in a standardized way. The function is built on the assumption that the data is structured in the agreed format (first column as countries, second as period, others as key variables)
#'
#'@param data dataframe to be used for the plot
#'@param subtitle subtitle of the plot
#'@param title title of the plot
#'@param Xlabel label of the X axis
#'@param Ylabel label of the Y axis
#'@param Ymin minimum y value to be displayed
#'@param Ymax maximum y value to be displayed
#'@param Ybreak difference between Y ticks
#'@param Xbreak difference between X ticks
#'@param start_date data start date
#'@param end_date data end date
#'@param file name of the file containing the plot were it to be saved, name must be followed by .png
#'@param path path of where the file is to be saved
#'@param width width of the image to be saved
#'@param height height of the image to be saved
#'@param dpi resolution
#'
#'@return Returns a standardized single line plot
#'
#'@author
#'Manuel Betin
#'Maxime Nguyen
#'
#'@export

htk_lineplot <- function(data = resilience_database,
                                subtitle =".",
                                title=NULL,
                                Xlabel ="Date",
                                Ylabel = "Price index",
                                ymin = 0,
                                ymax = 120,
                                ybreak = 10,
                                xbreak = "5 years",
                                start_date = 1970,
                                end_date = 2018,
                                file = NULL,
                                path=NULL,
                                width=4,
                                height=4,
                                dpi=300)
  {

    country_plot <- ggplot(data=data)+

    geom_line(aes(x=get(colnames(data)[2]), y=get(colnames(data)[4]),
                  group=ISO3_code,
                  color=ISO3_code,
                  text=paste0("",get(colnames(data)[2]),"\n",
                              "",round(get(colnames(data)[4]),2),"\n"))) +

    labs(x        = Xlabel,
         y        = Ylabel,
         title    = title,
         subtitle = subtitle)  +  #add title, subtitles and labels on both X and Y axis
    #scale_y_continuous(breaks=seq(ymin,ymax,ybreak)) + #set y ticks
    #scale_x_date(date_breaks= xbreak, date_labels ="%Y") +  #set x ticks
    theme_minimal() +   #set the background of the plot as white
    theme(panel.grid.minor =  element_blank(),
          legend.position ="bottom",
          legend.title=element_blank(),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=11),
          plot.title=element_text(face="bold",colour="steelblue",size=15, hjust =0.5),
          plot.subtitle =element_text(hjust = 0.5))  #clean background grid elements

  if (!is.null(file)){
    ggsave(filename=file,device = 'png',path=path,width = 4, height = 4, dpi = 300 )
  }
    return(country_plot)
}



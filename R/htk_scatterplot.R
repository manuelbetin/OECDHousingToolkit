#'scatter barplots with OECD template style to display correlation between two variables in the housing toolkit chapters
#'
#'@description The functions generates scatter plots in an agreed format to ensure presentation consistency in the dashboard
#'
#'@param data_source dataframe that contains the variables to plot
#'@param myvar_x x axis variable name
#'@param myvar_y y axis variable name
#'@param my_label labels used to label the points in the plot
#'@param Xlabel label of the X axis
#'@param Ylabel label of the Y axis
#'@param title title of the plot
#'@param subtitle subtitle of the plot
#'
#'@return returns a standardized scatter plot
#'
#'@author
#'Manuel Betin
#'Maxime Nguyen
#'
#'@export


htk_scatterplot <- function(data_source,myvar_x,myvar_y,my_label,Xlabel=NULL,Ylabel=NULL,xlimits = c(0,35),ylimits = c(0,100),title=NULL,subtitle=NULL)
{
    plot <- ggplot(data =data_source, aes(x=get(myvar_x),y=get(myvar_y),
                                          label=get(my_label))) +
                                          #group=get(my_label),
                                          #text=paste0("",get(myvar_x),"\n",
                                                      #"",round(get(myvar_y),2),"\n"))) +
      geom_point() +
      geom_text_repel() +
      scale_x_continuous(name=Xlabel, limits=xlimits, labels = function(x) paste0(x, "%")) +
      scale_y_continuous(name=Ylabel,limits=ylimits,labels = function(x) paste0(x, "%")) +
      geom_smooth(method=lm, se = FALSE, color="darkred") +
      labs(title = title,
           subtitle=subtitle,
           x= Xlabel,
           y = Ylabel) +
      theme_minimal() + #set background as minimal
      theme(panel.grid.minor =  element_blank()) + #set background grid as minimal
      theme(axis.text.x =element_text(size = 11,angle=90)) + #set aesthetic details
      theme(axis.title.x = element_text(size = 11)) +
      theme(axis.title.y = element_text(size=11)) +
      theme(axis.text.y = element_text(size=11)) +
      theme(plot.title=element_text(face="bold",colour ="steelblue",size=15, hjust =0.5)) +
      theme(plot.subtitle =element_text(size =7, hjust = 0.5))
    return(plot)
    }



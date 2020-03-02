htk_scatterplot <- function(data_source,myvar_x,myvar_y,my_label,Xlabel=NULL,Ylabel=NULL,title=NULL,subtitle=NULL)
{
    plot <- ggplot(data =data_source, aes(x=get(myvar_x),y=get(myvar_y),
                                          label=get(my_label),
                                          group=get(my_label),
                                          text=paste0("",get(myvar_x),"\n",
                                                      "",round(get(myvar_y),2),"\n"))) +
      geom_point() +
      geom_text_repel() +
      scale_x_continuous(name="Peak-to-trough of residential investment", limits=c(-100, 0)) +
      scale_y_continuous(name="Time to recovery of real GDP",limits=c(0, 40)) +
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

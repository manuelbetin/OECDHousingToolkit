my_OECD_theme=function(){
  theme(plot.margin = unit(c(0.5,0,0,0), "cm"),
        plot.background   = element_blank(),
        panel.background  = element_blank(),                        # backgroung of the chart
        panel.border      = element_blank(),
        strip.background =element_rect(fill="white"),
        legend.position = c(0.5,1),
        legend.title=element_blank(),
        legend.text        = element_text(size=8),
        legend.key         = element_rect(fill="transparent", color = 0), # define the legend left aprt (with line colors)
        legend.key.width   = unit(20,"pt"),                               # define width of the legend line or box
        legend.key.size    = unit(10,"pt"),                               # define size between series in legend
        legend.background  = element_blank(),                             # remobve border around the legend box  
        legend.margin      = margin(0,0,0,0, "cm"),                       # remobve legend box margin
        legend.spacing     = unit(-0.5, "cm"),
        axis.title.y       = element_text(size=10,vjust = 1.05), # put title (unit) of the Y axis on top and centered (leeft and right)
        axis.title.y.right = element_text(size=10,vjust = 1.05),
        axis.text.y         = element_text(size=10,margin=margin(15,5,0,15,"pt")), # define margin around the Y axis text
        axis.text.y.right   = element_text(size=10,margin=margin(15,15,0,5,"pt")),
        panel.grid.major.y  = element_line(color="#c8c8c8", size = 0.2),                     # define y grid size and color
        axis.ticks.y        = element_blank(),                                               # remove tick on the y acis
        axis.line.x         = element_line(color="black", size = 0.2)      ,                 # define color and siez of x axis
        axis.ticks          = element_line(colour = "black", size = 0.2)  ,                  # define color and size of x tick
        axis.text.x         = element_text(size=8,angle = 0, margin=margin(2,0,0,0,"pt")) # rotate the X axis text
  )
}

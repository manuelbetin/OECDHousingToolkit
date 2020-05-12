htk_policyradar=function(data,ctry,title=NULL){

  #' @title Radar chart for policy variables
  #' @description Radar chart to produce the spider graph
  #' to display the selected policy variables
  #' @param data a dataframe with the policy variables in
  #' columns and iso3 codes in rows.
  #' @param ctry the iso3 code for the selected country
  #' @param title an optional title for the figure
  #' @return fmsl radarchart object
  #' @author Manuel Betin
  #' @export

  varcodes=c("POL_Rent","POL_Ten","POL_LTV","POL_METR","POL_SPENSOC","POL_Plan","POL_Build")
  #Make sure all dimension are present in the database
  if(any(colnames(data) %in% varcodes)){

    #filter the selected country
    data=data %>% filter(country %in% c("min","max",ctry,"OECD")) #%>% dplyr::select(-country,varcodes)
    data=rbind(rep(0,7),rep(5,7),data)
    rownames(data)=data$country
    data=data%>% dplyr::select(varcodes)
    #use proper label names
    colnames(data) <- c("Rent control\n stringency","Tenant protection\n stringency","Maximum\n loan to \nvalue","Marginal effective\ntax rate\non mortgage","Public spending\n on social rental\n housing", "Planning \n systems","Building and energy \n code stringency")

    # set colors
    colors_border=c(rgb(0,0,1,1),rgb(1,0,0,0.3))
    colors_in=c(rgb(0,0,1,0.5),rgb(1,0,0,0.3))

    # plot the radar chart
    radarchart( data  , axistype=4 ,
                title=title,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,pty=16,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=1,
                #custom labels

                vlcex=0.7
    ) %>%
      legend(x=0.85, y=-0.7,
             legend = rownames(data[-c(1,2),]),
             bty = "n", pch=20 , col=colors_in , text.col = c("black","darkgrey"), cex=0.8, pt.cex=3)
  }

}

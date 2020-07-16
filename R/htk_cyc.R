#'@title compare-your-country plot for each selected indicator in the housing toolkit chapters
#'
#'@description The functions generates a (line) plot displaying the value of the selected indicator for each country and with the average OECD value, the bottom and top performing countries.
#'@param mydata dataframe that contains the variables and dates to be chosen
#'@param ranking dataframe that contains the ranking of variables needed

#'@param ctry selected OECD countries for which the country note is produced
#'@param var_codes list of indicators
#'@param sub_vars list of replacement indicators
#'@param sub_vars_label names of the replacement  indicators

#'@param sec_col colours for each section
#'@param title a title for the figure
#'
#'@return returns a standardized plot
#'
#'@author
#'Manuel Betin
#'Federica De Pace
#'
#'@export



htk_CyC=function(mydata,ranking, ctry,var_codes, sec_col, title=NULL){

  #prepare the dataset with the proper variables
  vars_needed=prep_data(mydata,ranking,ctry,var_codes,type_var="outcomes")

  var_codes=vars_needed$var_codes
  var_codes_rank=paste0(var_codes,"_rank")
  var_names=vars_needed$var_names

  vars_needed=vars_needed$data

  # 2. create min, max, mean, valu
  for (var in var_codes) {
    name_col=paste0(var, '_country_min')
    vars_needed<- vars_needed %>%
      mutate(!!name_col := vars_needed$Iso_code3[which.min(get(var))] )
  }

  for (var in var_codes) {
    name_col=paste0(var, '_country_max')
    vars_needed<- vars_needed %>%
      mutate(!!name_col := vars_needed$Iso_code3[which.max(get(var))] )
  }


  OECD<- vars_needed %>%  filter(Iso_code3=="OECD")
  OECD<-OECD  %>%   select( contains('rank', ignore.case = TRUE))

  OECD_final<-data.frame(t(OECD)) %>%
    rename(rank_OECD=t.OECD.)
  OECD_final<-OECD_final %>% mutate(main_v=rownames(OECD_final))
  OECD_final<-OECD_final %>% mutate(main_v=gsub("_rank","",OECD_final$main_v, fixed = T))

  OECD_final<-OECD_final[, c(2,1)]

  temp_long<- vars_needed %>%  filter(Iso_code3==ctry)%>% dplyr::select(-Iso_code3) %>%
    gather(key = "variable", value = "value")

  for (var in var_codes) {
    temp_long<-temp_long %>%
      mutate(main_v=ifelse( (str_detect(variable, var_codes)), var_codes, NA))
  }
  temp_long<-temp_long %>%
    mutate(ext=ifelse( (str_detect(variable, "mean")), "mean", "value"),
           ext=ifelse( (str_detect(variable, "min")), "min", ext),
           ext=ifelse( (str_detect(variable, "max")), "max", ext),
           ext=ifelse( (str_detect(variable, "country_max")), "country_max", ext),
           ext=ifelse( (str_detect(variable, "country_min")), "country_min", ext),
           ext=ifelse( (str_detect(variable, "rank")), "rank", ext)) %>%
    dplyr::select(-variable)

  final<-reshape(temp_long, idvar = "main_v" , timevar =  "ext" , direction = "wide")
  names(final)
  name_vars<-c("value.value","value.mean","value.min","value.max", "value.rank")
  final<-  final %>% mutate_at(vars(name_vars),as.numeric)
  final<-merge(final, OECD_final, by="main_v")

  #produce the figure

  ggplot(data=final,aes(x=main_v))+
    geom_segment(aes(xend=main_v, y=-0.05, yend=1.05), color="grey") +
    geom_point(aes(y= value.rank ), shape=19, color='red', size=4)  +
    geom_point(aes(y=rank_OECD  ) , shape=18, color='darkblue', size=3) +
    geom_point(aes(y=0), shape=1, color='grey', size=2)  +
    geom_point(aes(y=1 ), shape=1, color='grey', size=2) +
    coord_flip() +
    labs(x = "", y=""
         #title=title
    ) +
    theme(panel.background = element_blank(),
          axis.text.x = element_blank(),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          axis.text.y = element_text(angle=40,size=11,color=sec_col),
          axis.ticks =element_blank() )+
    scale_x_discrete(breaks=final$main_v,labels=var_names) +
    geom_text(aes(y= value.rank   , label=paste(ctry,": ", round(value.value, digits = 2))),
              size=3.5, nudge_x = -0.1, nudge_y = 0.0,  check_overlap = TRUE) +
    geom_text(aes(y= rank_OECD   , label=paste("OECD: ", round(value.mean, digits = 2))),
              size=3, nudge_x = 0.1, nudge_y = 0.0,  check_overlap = TRUE,color="steelblue") +
    geom_text(aes(y= 0   , label=paste(value.country_min,":" , round(value.min, digits = 2))),
              size=3, nudge_x = 0.2, nudge_y = 0.03,  check_overlap = TRUE,color="steelblue") +
    geom_text(aes(y= 1   , label=paste(value.country_max,":", round(value.max, digits = 2))),
              size=3, nudge_x = 0.2, nudge_y = -0.05,  check_overlap = TRUE,color="steelblue")+
    annotate("text", x =3.4, y = 0.14, label = "min OECD performer", size=5,color="darkgrey") +
    annotate("text", x =3.4, y = 0.90, label = "max OECD performer", size=5,color="darkgrey")

}

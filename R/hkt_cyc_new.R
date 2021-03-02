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
#'Volker Ziemann
#'Federica De Pace
#'
#'@export

htk_cyc_new=function(mydata,ranking, ctry,var_codes, sec_col, type_var, title=NULL){

  # mydata=dt_effic
  # ranking=ranking_eff
  # var_codes=var_codes_eff
  # ctry=params$ctry_code
  # type_var="outcomes"
  # sec_col=sec_col_eff
  #prepare the dataset with the proper variables
  vars_needed=prep_data_indsel(mydata,ranking,ctry,var_codes,type_var)

  var_codes=vars_needed$var_codes
  var_codes_rank=paste0(var_codes,"_rank")
  var_names=vars_needed$var_names

  vars_needed=vars_needed$data

  if(length(var_codes)>1){
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

    # clean names of variables
    temp_long<-temp_long %>%
      mutate(main_v= ifelse(str_detect(variable, var_codes[1]), var_codes[1], NA))
    for (var in var_codes[2:length(var_codes)]){
      temp_long<-temp_long %>%
        mutate(main_v= ifelse(str_detect(variable, var), var, main_v))
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
    rownames(final)<-final[,1]
    final <- final[match(var_codes, rownames(final)),]


    #produce the figure

    mylabels<-reshape(final, idvar = "main_v" , varying =  list(2:5), v.names = "value", direction = "long")%>%
      arrange(main_v,time)%>%
      mutate(x=ifelse(time==1,value.rank,
                      ifelse(time==2,rank_OECD,
                             ifelse(time==3,0,1))))%>%
      mutate(mylabel=ifelse(time==1,paste0(ctry,"\n(" , round(value, digits = 2),")"),
                            ifelse(time==2,paste0("OECD\n(" , round(value, digits = 2),")"),
                                   ifelse(time==3,ifelse(ctry==value.country_min,"",paste0(value.country_min,"\n(" , round(value, digits = 2),")")),
                                          ifelse(ctry==value.country_max,"",paste0(value.country_max,"\n(" , round(value, digits = 2),")")))))) %>%
      mutate(mycolor=ifelse(time==1 ,"ctry",
                          ifelse(time==2,"OECD",
                                   ifelse(time==3,"minmax","minmax"))))%>%
      dplyr::select(main_v,x,mylabel,mycolor)

    # adjust for Lva, LTu, EST
    mylabels<-mylabels%>% mutate(mylabel=
                                   ifelse( (ctry=="LVA" |ctry=="EST"| ctry=="LTU")&  mycolor=="ctry"&main_v=="ECO_resilience_HP_vol" ,
                                           paste0(ctry," (2005-2019)","\n(" , round(final$value.value[final$main_v=="ECO_resilience_HP_vol"], digits = 2),")") ,
                                           mylabel))
    mylabels<-mylabels%>% mutate(mylabel=
                                   ifelse( mycolor=="minmax" &x==1& main_v=="ECO_resilience_HP_vol" ,
                                           paste0("EST*","\n(" , round(final$value.max[final$main_v=="ECO_resilience_HP_vol"], digits = 2),")") ,
                                           mylabel))
    mysize=3

    # rio::export(mylabels,"mylabels.RData")
    # rio::export(final,"final.RData")
    #
    # mylabels<-rio::import("mylabels.RData")
    # final<-rio::import("final.RData")

    par(lheight=0.2)
    mynudge_y=ifelse(final$value.value==final$value.mean ,0.05,-0.05 )

    ggplot(final,aes(y=main_v, x=0, xend=1))+
      geom_segment(aes(yend=main_v),color="grey", size=1)+
      geom_dumbbell(size=mysize, color="grey93", size_x=mysize, size_xend =mysize,
                    colour_x ="grey", colour_xend="grey")+
      geom_point(aes(x= value.rank ), shape=19, color="red", size=mysize)  +
      geom_point(aes(x=rank_OECD  ) , shape=18, color="darkgreen", size=mysize) +
      geom_point(aes(x=0), shape=1, color='grey', size=mysize)  +
      geom_point(aes(x=1 ), shape=1, color='grey', size=mysize) +
      geom_text(size=2.5,data=mylabels%>%filter(mycolor=="minmax"),
                aes(x=x,vjust=-0.5,label=mylabel,color=mycolor,lineheight = .8))+
      geom_text_repel(seed = 10, size=2.5,
                      data=mylabels%>%filter(mycolor=="OECD"),
                      nudge_y = 0.15,
                      # direction="y",
                      force=3,
                      aes(x=x,label=mylabel,color=mycolor,lineheight = .8))+
      geom_text_repel(seed = 10, size=3,
                      data=mylabels%>%filter(mycolor=="ctry"),
                      nudge_y = -0.15,
                      # direction="y",
                      force=3,
                      aes(x=x,label=mylabel,color=mycolor,lineheight = .8))+
      scale_y_discrete(breaks=final$main_v,labels=var_names, limits = rev(final$main_v)) +
      scale_color_manual(values = c("minmax"="black","OECD"="darkgreen","ctry"="red"))+
      theme(panel.background = element_blank(),
            axis.text.x = element_blank(),
            plot.title=element_blank(),
            axis.text.y = element_text(size=12,color=sec_col, face="bold"),
            axis.ticks =element_blank(),
            legend.position = 'none',
            axis.title = element_blank())

  }
  else{
    myctry=countrycode::countrycode(ctry,origin="iso3c",destination="country.name")
    ggplot()+
      geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
      geom_point(aes(x=c(0,20),y=c(0,20)),color="white")+
      theme_void()
  }

}

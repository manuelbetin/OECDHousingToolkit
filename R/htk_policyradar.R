htk_policyradar=function(mydata,ranking, ctry, var_codes, var_names, title=NULL){

  #' @title Radar chart for policy variables
  #' @description Radar chart to produce the spider graph
  #' to display the selected policy variables
  #' @param data a dataframe with all the policy variables
  #' columns and iso3 codes in rows.
  #' @param ranking a dataframe with the ranking of the policy variables
  #' @param ctry the iso3 code for the selected country
  #' @param var_codes a dataframe with the the code of the policy variables
  #' @param var_names a dataframe with the names of the policy variables
  #' @param title an optional title for the figure
  #' @return fmsl radarchart object
  #' @author Manuel Betin
  #' @export


  this_country<-mydata %>%  filter(Iso_code3==ctry)  %>%  select(Iso_code3, first(var_codes):last(var_codes))
  this_country_long<-gather(this_country, variable , value,  first(var_codes):last(var_codes) )
  this_country_long<- merge(this_country_long, ranking[1:2], by="variable")

  # which of the main var is NA?
  ind_NA<-which(is.na(this_country_long$value) == TRUE, arr.ind=TRUE)

  if (length(ind_NA) ==0) {
    dt_non_na = this_country_long
  }  else if (length(ind_NA) >0) {
    dt_non_na = this_country_long[-ind_NA, ]
  }

  #n_var_nonna<-nrow(dt_non_na)
  # count Non NA vars ranked 1 or 2 or 3

  n_rank1 <- nrow(dt_non_na[with( dt_non_na,which(rank==1 )), ])
  n_rank2 <- nrow(dt_non_na[with( dt_non_na,which(rank==2 )), ])
  n_rank3 <- nrow(dt_non_na[with( dt_non_na,which(rank==3 )), ])

  vars_touse=get_vars_policy(n_rank1, n_rank2, n_rank3, dt_non_na )
  temp_names<-merge(vars_touse, ranking , by="variable")

  var_codes=temp_names$variable
  var_names=temp_names$variable_name
  # which variables to use:
  vars_needed=mydata %>%  select(Iso_code3, var_codes)
  vars_needed =data.frame(vars_needed)
  # transform in numeric
  sapply(vars_needed, class)
  vars_needed[var_codes] <- sapply(vars_needed[var_codes],as.numeric)
  sapply(vars_needed, class)
names(vars_needed)


dt_mean <-vars_needed %>% summarise_at(.vars = var_codes,
                                       .funs=list(mean=~mean(.,na.rm=T)))

dt_mean <-dt_mean %>% mutate(Iso_code3="OECD")
dt_mean<-dt_mean %>% arrange(Iso_code3, "")
dt_mean <- dt_mean %>%
  select(Iso_code3, everything())


names(dt_mean)
# change the name of variables
dt_mean<-dt_mean  %>%  rename_all( funs(str_replace(., "_mean", "")))
names(dt_mean)
names(vars_needed)


vars_needed<-rbind(vars_needed, dt_mean)


  # old version
 # varcodes=c("POL_Rent","POL_Ten","POL_LTV","POL_METR","POL_SPENSOC","POL_Plan","POL_Build")
  #Make sure all dimension are present in the database
 # if(any(colnames(data) %in% varcodes)){

    #filter the selected country



vars_needed_plus<- vars_needed  %>%
  mutate_at(vars(var_codes),.funs=list(mean=~mean(.,na.rm=T),
                                       min=~min(.,na.rm=T),
                                       max=~max(.,na.rm=T),
                                       rank=~ percent_rank(.,na.rm=T) ))
  names(vars_needed_plus)
  #vars_needed_plus<- vars_needed_plus  %>%mutate(rank = rank*5)

  # 2. create min, max, mean, valu
  for (var in var_codes) {
    name_col=paste0(var, '_country_min')
    vars_needed_plus<- vars_needed_plus %>%
      mutate(!!name_col := vars_needed_plus$Iso_code3[which.min(get(var))] )
  }

  for (var in var_codes) {
    name_col=paste0(var, '_country_max')
    vars_needed_plus<- vars_needed_plus %>%
      mutate(!!name_col := vars_needed_plus$Iso_code3[which.max(get(var))] )
  }


  temp_long<- vars_needed_plus %>%  filter(Iso_code3==ctry)%>% dplyr::select(-Iso_code3) %>%
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
  name_vars<-c("value.value","value.mean","value.min","value.max", "value.rank")

  OECD<- vars_needed_plus %>%  filter(Iso_code3=="OECD")
  OECD<-OECD  %>%   select( contains('rank', ignore.case = TRUE))

  OECD_final<-data.frame(t(OECD)) %>%
    rename(rank_OECD=t.OECD.)
  OECD_final<-OECD_final %>% mutate(main_v=rownames(OECD_final))
  OECD_final<-OECD_final %>% mutate(main_v=gsub("_rank","",OECD_final$main_v, fixed = T))

  OECD_final<-OECD_final[, c(2,1)]
  final<-merge(final, OECD_final, by="main_v")
  sapply(final, class)
  final[name_vars] <- sapply(final[name_vars],as.numeric)
  sapply(final, class)

  final_t<-data.frame(t(final))

   names(final_t) <- var_codes
   final_t <- final_t[-1,]

   names_sel<-c("value.rank",  "rank_OECD" )

   final_t1<- subset(final_t, rownames(final_t) %in% names_sel)
   data=data.frame(final_t1)
   sapply(final_t, class)
   as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
   data[var_codes] <- sapply(data[var_codes], as.numeric.factor)
   sapply(data, class)


   #[c(2,1,3,4 ),]
max=rep(1.2,5)
min=rep(0,5)
    #%>% select(value.) #%>% dplyr::select(-country,varcodes)
    data=rbind(min,max,data)
    rownames(data)[1:2]<-c("min", "max")
  #  rownames(data)=data$country
   # data=data%>% dplyr::select(var_codes)
    #use proper label names
  #  colnames(data) <- var_names

    # set colors
    colors_border=c(rgb(0,0,1,1),rgb(1,0,0,0.3))
    colors_in=c(rgb(0,0,1,0.5),rgb(1,0,0,0.3))
    colnames(data)=var_names
    # plot the radar chart
    radarchart( data  , axistype=4 ,
                title="",
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,pty=16,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1.2,.2), cglwd=1,
                #custom labels : check here

                vlcex=0.7
    ) %>%
      legend(x=0.85, y=-0.7,
             legend = c(ctry,"OECD"),
             bty = "n", pch=20 , col=colors_in , text.col = c("black","darkgrey"), cex=0.8, pt.cex=3)
 # }

}
